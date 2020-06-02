/**
 * This file is part of DCD, a development tool for the D programming language.
 * Copyright (C) 2014 Brian Schott
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

module dcd.server.main;

import core.sys.posix.sys.stat;
import core.stdc.string : strcmp;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime.stopwatch : AutoStart, StopWatch;
import std.exception : enforce;
import stdx.allocator;
import stdx.allocator.mallocator;
import std.experimental.logger;
import std.file;
import std.getopt;
import std.path;
import std.process;
import std.socket;
import std.stdio;

import msgpack;

import dcd.common.dcd_version;
import dcd.common.messages;
import dcd.common.socket;
import dcd.server.autocomplete;
import dcd.server.server;

import dmd.dmodule;
import dmd.globals;
import dmd.root.string : toDString;
import dmd.root.filename;
import dmd.identifier;
import dmd.frontend;
import dmd.errors;
import dmd.console;
import dmd.mars;

import core.stdc.stdarg;


int main(string[] args)
{
	try
	{
		return runServer(args);
	}
	catch (Exception e)
	{
		stderr.writeln(e);
		return 1;
	}
}

int runServer(string[] args)
{
	ushort port;
	bool help;
	bool printVersion;
	bool ignoreConfig;
	string[] importPaths;
	LogLevel level = globalLogLevel;
	version(Windows)
	{
		bool useTCP = true;
		string socketFile;
	}
	else
	{
		bool useTCP = false;
		string socketFile = generateSocketName();
	}

	sharedLog.fatalHandler = () {};

	global.gag = 1;
	global.params.doDocComments = true;
	global.path = new Strings();
    // global.path.push("/home/cristian/dlang/phobos");
    // global.path.push("/home/cristian/dlang/druntime/import");

	diagnosticHandler = (const ref Loc location,
                            Color headerColor,
                            const(char)* header,
                            const(char)* messageFormat,
                            va_list args,
                            const(char)* prefix1,
                            const(char)* prefix2) => true;
	initDMD();


	try
	{
		getopt(args, "port|p", &port, "I", &importPaths, "help|h", &help,
			"version", &printVersion, "ignoreConfig", &ignoreConfig,
			"logLevel", &level, "tcp", &useTCP, "socketFile", &socketFile);
	}
	catch (ConvException e)
	{
		fatal(e.msg);
		printHelp(args[0]);
		return 1;
	}

	globalLogLevel = level;

	if (printVersion)
	{
		writeln(DCD_VERSION);
		return 0;
	}

	if (help)
	{
		printHelp(args[0]);
		return 0;
	}

	// If the user specified a port number, assume that they wanted a TCP
	// connection. Otherwise set the port number to the default and let the
	// useTCP flag deterimen what to do later.
	if (port != 0)
		useTCP = true;
	else
		port = DEFAULT_PORT_NUMBER;

	if (useTCP)
		socketFile = null;

	version (Windows) if (socketFile !is null)
	{
		fatal("UNIX domain sockets not supported on Windows");
		return 1;
	}

	if (serverIsRunning(useTCP, socketFile, port))
	{
		fatal("Another instance of DCD-server is already running");
		return 1;
	}

	info("Starting up...");
	StopWatch sw = StopWatch(AutoStart.yes);

	if (!ignoreConfig)
	{
		auto configuredImportDirs = loadConfiguredImportDirs();
		foreach (configuredImportDir; configuredImportDirs)
			global.path.push(configuredImportDir.ptr);
	}

	Socket socket;
	if (useTCP)
	{
		socket = new TcpSocket(AddressFamily.INET);
		socket.blocking = true;
		socket.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, true);
		socket.bind(new InternetAddress("localhost", port));
		info("Listening on port ", port);
	}
	else
	{
		version(Windows)
		{
			fatal("UNIX domain sockets not supported on Windows");
			return 1;
		}
		else
		{
			socket = new Socket(AddressFamily.UNIX, SocketType.STREAM);
			if (exists(socketFile))
			{
				info("Cleaning up old socket file at ", socketFile);
				remove(socketFile);
			}
			socket.blocking = true;
			socket.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, true);
			socket.bind(new UnixAddress(socketFile));
			setAttributes(socketFile, S_IRUSR | S_IWUSR);
			info("Listening at ", socketFile);
		}
	}
	socket.listen(32);

	scope (exit)
	{
		info("Shutting down sockets...");
		socket.shutdown(SocketShutdown.BOTH);
		socket.close();
		if (!useTCP)
			remove(socketFile);
		info("Sockets shut down.");
	}

	foreach (path; importPaths)
	{
		auto absPath = absolutePath(expandTilde(path));
		if (!absPath.exists())
		{
			warning(path, " does not exist");
			continue;
		}

		global.path.push(path.ptr);
	}

	infof("Import directories:\n    %-(%s\n    %)", importPaths);

	ubyte[] buffer = cast(ubyte[]) Mallocator.instance.allocate(1024 * 1024 * 4); // 4 megabytes should be enough for anybody...
	scope(exit) Mallocator.instance.deallocate(buffer);

	sw.stop();
	info("Startup completed in ", sw.peek().total!"msecs"(), " milliseconds.");

	// No relative paths
	version (Posix) chdir("/");

	version (LittleEndian)
		immutable expectedClient = IPv4Union([1, 0, 0, 127]);
	else
		immutable expectedClient = IPv4Union([127, 0, 0, 1]);

	serverLoop: while (true)
	{
		auto s = socket.accept();
		s.blocking = true;

		if (useTCP)
		{
			// Only accept connections from localhost
			IPv4Union actual;
			InternetAddress clientAddr = cast(InternetAddress) s.remoteAddress();
			actual.i = clientAddr.addr;
			// Shut down if somebody tries connecting from outside
			if (actual.i != expectedClient.i)
			{
				fatal("Connection attempted from ", clientAddr.toAddrString());
				return 1;
			}
		}

		scope (exit)
		{
			s.shutdown(SocketShutdown.BOTH);
			s.close();
		}

		ptrdiff_t bytesReceived = s.receive(buffer);

		sw.reset();
		sw.start();

		size_t messageLength;
		// bit magic!
		(cast(ubyte*) &messageLength)[0..size_t.sizeof] = buffer[0..size_t.sizeof];
		while (bytesReceived < messageLength + size_t.sizeof)
		{
			immutable b = s.receive(buffer[bytesReceived .. $]);
			if (b == Socket.ERROR)
			{
				bytesReceived = Socket.ERROR;
				break;
			}
			bytesReceived += b;
		}

		if (bytesReceived == Socket.ERROR)
		{
			warning("Socket recieve failed");
			break;
		}

		AutocompleteRequest request;
		msgpack.unpack(buffer[size_t.sizeof .. bytesReceived], request);

		Strings libmodules;
		auto dcdProjectDir = dirName(dirName(dirName(dirName(__FILE_FULL_PATH__))));

		Module rootModule = createModule(isAbsolute(request.fileName)
											? buildNormalizedPath(request.fileName).ptr
											: buildNormalizedPath(dcdProjectDir, request.fileName).ptr,
										libmodules);
		rootModule.importedFrom = rootModule;

		rootModule.read(Loc.initial);

		if (request.kind & RequestKind.clearCache)
		{
			info("Clearing cache.");
			// cache.clear();
		}
		else if (request.kind & RequestKind.shutdown)
		{
			info("Shutting down.");
			break serverLoop;
		}
		else if (request.kind & RequestKind.query)
		{
			s.sendResponse(AutocompleteResponse.ack);
			continue;
		}

		if (request.kind & RequestKind.addImport)
		{
			foreach (path; request.importPaths)
				global.path.push(path.ptr);
		}

		if (request.kind & RequestKind.removeImport)
		{
			foreach (removePath; request.importPaths)
			{
				size_t idx = size_t.max;
				foreach (i, path; *global.path)
					if (strcmp(removePath.ptr, path) == 0)
					{
						idx = i;
						break;
					}

				if (idx != size_t.max)
					global.path.remove(idx);
			}
		}

		if (request.kind & RequestKind.listImports)
		{
			AutocompleteResponse response;
			foreach (path; *global.path)
				response.importPaths ~= to!string(path);

			info("Returning import path list");
			s.sendResponse(response);
		}
		else if (request.kind & RequestKind.autocomplete)
		{
			info("Getting completions");
			s.sendResponse(complete(request, rootModule));
		}
		else if (request.kind & RequestKind.doc)
		{
			info("Getting doc comment");
			s.trySendResponse(getDoc(request, rootModule), "Could not get DDoc information");
		}
		else if (request.kind & RequestKind.symbolLocation)
			s.trySendResponse(findDeclaration(request, rootModule), "Could not get symbol location");
		else if (request.kind & RequestKind.search)
			s.sendResponse(symbolSearch(request, rootModule));
		else if (request.kind & RequestKind.localUse)
			s.trySendResponse(findLocalUse(request, rootModule), "Couldnot find local usage");

		sw.stop();
		info("Request processed in ", sw.peek().total!"msecs"(), " milliseconds");
	}

	deinitializeDMD();

	return 0;
}

/// Lazily evaluates a response with an exception handler and sends it to a socket or logs msg if evaluating response fails.
void trySendResponse(Socket socket, lazy AutocompleteResponse response, lazy string msg)
{
	try
	{
		sendResponse(socket, response);
	}
	catch (Exception e)
	{
		warningf("%s: %s", msg, e.msg);
	}
}

/// Packs an AutocompleteResponse and sends it to a socket.
void sendResponse(Socket socket, AutocompleteResponse response)
{
	ubyte[] responseBytes = msgpack.pack(response);
	socket.send(responseBytes);
}

/// IP v4 address as bytes and a uint
union IPv4Union
{
	/// the bytes
	ubyte[4] b;
	/// the uint
	uint i;
}


/**
 * Implements the --help switch.
 */
void printHelp(string programName)
{
    writefln(
`
    Usage: %s options

options:
    -I PATH
        Includes PATH in the listing of paths that are searched for file
        imports.

    --help | -h
        Prints this help message.

    --version
        Prints the version number and then exits.

    --port PORTNUMBER | -pPORTNUMBER
        Listens on PORTNUMBER instead of the default port 9166 when TCP sockets
        are used.

    --logLevel LEVEL
        The logging level. Valid values are 'all', 'trace', 'info', 'warning',
        'error', 'critical', 'fatal', and 'off'.

    --tcp
        Listen on a TCP socket instead of a UNIX domain socket. This switch
        has no effect on Windows.

    --socketFile FILENAME
        Use the given FILENAME as the path to the UNIX domain socket. Using
        this switch is an error on Windows.`, programName);
}
