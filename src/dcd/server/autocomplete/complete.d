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

module dcd.server.autocomplete.complete;

import core.stdc.string : strcmp;

import std.algorithm;
import std.array;
import std.conv;
import std.experimental.logger;
import std.file;
import std.path;
import std.range : assumeSorted;
import std.string;
import std.typecons;

import dcd.server.autocomplete.util;
import dcd.common.constants;
import dcd.common.messages;

import dmd.dsymbol;
import dmd.dscope;
import dmd.dmodule;
import dmd.tokens;
import dmd.globals;
import dmd.statement;
import dmd.arraytypes;


import dmd.gluelayer;


import std.stdio : writeln;



/**
 * Handles autocompletion
 * Params:
 *     request = the autocompletion request
 * Returns:
 *     the autocompletion response
 */
public AutocompleteResponse complete(const AutocompleteRequest request, Module rootModule)
{
	auto cursorLoc = Loc(rootModule.srcfile.toChars(), request.cursorLinnum, request.cursorCharnum);
	const(Token)[] tokenArray;
	auto beforeTokens = getTokensBeforeCursor(request.sourceCode,
		cursorLoc, tokenArray, rootModule);

	// allows to get completion on keyword, typically "is"
	if (beforeTokens.length &&
		(beforeTokens[$-1].isKeyword()
		|| beforeTokens[$-1].value.among(TOK.int8, TOK.uns8, TOK.int16, TOK.uns16,
											TOK.int32, TOK.uns32, TOK.int64, TOK.uns64,
											TOK.int128, TOK.uns128, TOK.float32, TOK.float64,
											TOK.float80, TOK.bool_, TOK.char_, TOK.wchar_,
											TOK.dchar_, TOK.imaginary32, TOK.imaginary64,
											TOK.imaginary80, TOK.complex32, TOK.complex64,
											TOK.complex80, TOK.void_)))
	{
		Token* fakeIdent = cast(Token*) (&beforeTokens[$-1]);
		// fakeIdent.ptr = str(fakeIdent.value);
		fakeIdent.value = TOK.identifier;
	}

	const bool dotId = beforeTokens.length >= 2 &&
		beforeTokens[$-1].value == TOK.identifier && beforeTokens[$-2].value == TOK.dot;

	// detects if the completion request uses the current module `ModuleDeclaration`
	// as access chain. In this case removes this access chain, and just keep the dot
	// because within a module semantic is the same (`myModule.stuff` -> `.stuff`).
	if (tokenArray.length >= 3 && tokenArray[0].value == TOK.module_ && beforeTokens.length &&
		(beforeTokens[$-1].value == TOK.dot || dotId))
	{
		const moduleDeclEndIndex = tokenArray.countUntil!(a => a.value == TOK.semicolon);
		bool beginsWithModuleName;
		// enough room for the module decl and the fqn...
		if (moduleDeclEndIndex != -1 && beforeTokens.length >= moduleDeclEndIndex * 2)
			foreach (immutable i; 0 .. moduleDeclEndIndex)
		{
			const expectIdt = bool(i & 1);
			const expectDot = !expectIdt;
			const j = beforeTokens.length - moduleDeclEndIndex + i - 1 - ubyte(dotId);

			// verify that the chain is well located after an expr or a decl
			if (i == 0)
			{
				if (!beforeTokens[j].value.among(TOK.leftCurly, TOK.rightCurly, TOK.semicolon,
					TOK.leftBracket, TOK.leftParentheses, TOK.comma,  TOK.colon))
						break;
			}
			// then compare the end of the "before tokens" (access chain)
			// with the firsts (ModuleDeclaration)
			else
			{
				// even index : must be a dot
				if (expectDot &&
					(tokenArray[i].value != TOK.dot || beforeTokens[j].value != TOK.dot))
						break;
				// odd index : identifiers must match
				else if (expectIdt &&
					(tokenArray[i].value != TOK.identifier || beforeTokens[j].value != TOK.identifier ||
					to!string(tokenArray[i].ident) != to!string(beforeTokens[j].ident)))// !!!!!!!!!!
						break;
			}
			if (i == moduleDeclEndIndex - 1)
				beginsWithModuleName = true;
		}


		// replace the "before tokens" with a pattern making the remaining
		// parts of the completion process think that it's a "Module Scope Operator".

		// if (beginsWithModuleName) // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		// {
		// 	if (dotId)
		// 		beforeTokens = assumeSorted([const Token(tok!"{"), const Token(tok!"."),
		// 			cast(const) beforeTokens[$-1]]);
		// 	else
		// 		beforeTokens = assumeSorted([const Token(tok!"{"), const Token(tok!".")]);
		// }
	}

	if (beforeTokens.length >= 2)
	{
		if (beforeTokens[$ - 1].value == TOK.leftParentheses
			|| beforeTokens[$ - 1].value == TOK.leftBracket
			|| beforeTokens[$ - 1].value == TOK.comma)
		{
			immutable size_t end = goBackToOpenParen(beforeTokens);
			if (end != size_t.max) {
				return parenCompletion(beforeTokens[0 .. end], tokenArray,
					cursorLoc, rootModule);
			}
		}
		else
		{
			ImportKind kind = determineImportKind(beforeTokens);
			if (kind == ImportKind.neither)
			{
				if (beforeTokens.isUdaExpression)
					beforeTokens = beforeTokens[$-1 .. $];
				return dotCompletion(beforeTokens, tokenArray, cursorLoc,
					rootModule);
			}
			else
				return importCompletion(beforeTokens, kind, rootModule);
		}
	}
	return dotCompletion(beforeTokens, tokenArray, cursorLoc, rootModule);
}

/**
 * Handles dot completion for identifiers and types.
 * Params:
 *     beforeTokens = the tokens before the cursor
 *     tokenArray = all tokens in the file
 *     cursorPosition = the cursor position in bytes
 * Returns:
 *     the autocompletion response
 */
AutocompleteResponse dotCompletion(T)(T beforeTokens, const(Token)[] tokenArray,
	Loc cursorPosition, ref Module rootModule)
{
	AutocompleteResponse response;

	// Partial symbol name appearing after the dot character and before the
	// cursor.
	string partial;

	// Type of the token before the dot, or identifier if the cursor was at
	// an identifier.
	TOK significantTokenType;

	if (beforeTokens.length >= 1 && beforeTokens[$ - 1].value == TOK.identifier)
	{
		// Set partial to the slice of the identifier between the beginning
		// of the identifier and the cursor. This improves the completion
		// responses when the cursor is in the middle of an identifier instead
		// of at the end
		auto t = beforeTokens[$ - 1];

		import core.stdc.string : strlen;

		// !!!!!!!!!!!!!!!!!!
		if ((cursorPosition.linnum > t.loc.linnum || (cursorPosition.linnum == t.loc.linnum && cursorPosition.charnum >= t.loc.charnum))
				&& cursorPosition.charnum - t.loc.charnum <= strlen(t.ident.toChars())) // sa fie bine aici cu colnum???
		{
			partial = to!string(t.ptr[0 .. cursorPosition.charnum - t.loc.charnum]);
			// issue 442 - prevent `partial` to start in the middle of a MBC
			// since later there's a non-nothrow call to `toUpper`
			import std.utf : validate, UTFException;
			try validate(partial);
			catch (UTFException)
			{
				import std.experimental.logger : warning;
				warning("cursor positioned within a UTF sequence");
				partial = "";
			}
		}
		significantTokenType = partial.length ? TOK.identifier : TOK.max_;
		beforeTokens = beforeTokens[0 .. $ - 1];
	}
	else if (beforeTokens.length >= 2 && beforeTokens[$ - 1].value == TOK.dot)
	{
		significantTokenType = beforeTokens[$ - 2].value;

		if (beforeTokens[$ - 2].value == TOK.identifier)
		{
			auto symbols = getSymbolsInCompletionScope(cursorPosition, rootModule);
			foreach (id; *symbols)
				if (strcmp(id.ident.toChars(), beforeTokens[$ - 2].ident.toChars()) == 0)
				{
					auto dec = id.isDeclaration();
					if (dec is null)
						continue;

					foreach (type; *symbols)
						if (to!string(type) == to!string(dec.type))
						{
							auto sds = type.isScopeDsymbol();
							if (sds is null)
								continue;

							Dsymbol[] members;
							foreach (mem; *sds.members)
								if (mem !is null
									&& mem.ident !is null
									&& !mem.isImport()
									&& strcmp(mem.loc.filename, cursorPosition.filename) == 0
									&& strcmp(mem.ident.toChars(), "__ctor") != 0)
									members ~= mem;

							foreach (mem; members)
							{
								auto declaration = mem.isDeclaration();
								response.completions ~= AutocompleteResponse.Completion(
										to!string(mem.ident),
										getSymbolCompletionKind(mem),
										declaration is null ? null : to!string(declaration.type),
										to!string(mem.loc.filename), 0,
										// symbol.loc.linnum, symbol.loc.charnum,
										to!string(mem.comment));
							}
						}
				}
			response.completionType = CompletionType.identifiers;
			return response;
		}
	}
	else
		return response;


	switch (significantTokenType)
	{
	mixin(STRING_LITERAL_CASES); // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		// foreach (symbol; arraySymbols)
		// 	response.completions ~= makeSymbolCompletionInfo(symbol, symbol.kind);
		// response.completionType = CompletionType.identifiers;
		break;
	mixin(TYPE_IDENT_CASES);
	case TOK.rightParentheses:
	case TOK.rightBracket:
		auto symbols = getSymbolsInCompletionScope(cursorPosition, rootModule);
		// response.setCompletions(symbols, getExpression(beforeTokens),
		// 	cursorPosition, CompletionType.identifiers, false, partial);
		foreach (symbol; *symbols)
		{
			if (!toUpper(symbol.ident.toString()).startsWith(toUpper(partial)))
				continue;

			auto dec = symbol.isDeclaration();
			response.completions ~= AutocompleteResponse.Completion(
										to!string(symbol.ident),
										getSymbolCompletionKind(symbol),
										to!string(dec.type),
										to!string(symbol.loc.filename), 0,
										// symbol.loc.linnum, symbol.loc.charnum,
										to!string(symbol.comment));
			response.completionType = CompletionType.identifiers;
		}
		break;
	//  these tokens before a "." mean "Module Scope Operator"
	case TOK.colon:
	case TOK.leftParentheses:
	case TOK.leftBracket:
	case TOK.leftCurly:
	case TOK.semicolon:
	case TOK.rightCurly:
	case TOK.comma:
		auto symbols = getSymbolsInCompletionScope(Loc.initial, rootModule);
		// response.setCompletions(symbols, getExpression(beforeTokens),
		// 	Loc.initial, CompletionType.identifiers, false, partial);
		break;
	default:
		break;
	}
	return response;
}

/**
 * Handles paren completion for function calls and some keywords
 * Params:
 *     beforeTokens = the tokens before the cursor
 *     tokenArray = all tokens in the file
 *     cursorPosition = the cursor position in bytes
 * Returns:
 *     the autocompletion response
 */
AutocompleteResponse parenCompletion(T)(T beforeTokens,
	const(Token)[] tokenArray, Loc cursorPosition, ref Module rootModule)
{
	AutocompleteResponse response;
	immutable(ConstantCompletion)[] completions;
	switch (beforeTokens[$ - 2].value)
	{
	case TOK.traits:
		completions = traits;
		goto fillResponse;
	case TOK.scope_:
		completions = scopes;
		goto fillResponse;
	case TOK.version_:
		completions = predefinedVersions;
		goto fillResponse;
	case TOK.extern_:
		completions = linkages;
		goto fillResponse;
	case TOK.pragma_:
		completions = pragmas;
	fillResponse:
		response.completionType = CompletionType.identifiers;
		foreach (completion; completions)
		{
			response.completions ~= AutocompleteResponse.Completion(
				completion.identifier,
				CompletionKind.keyword,
				null, null, 0, // definition, symbol path+location
				completion.ddoc
			);
		}
		break;
	case TOK.int32Literal:
	case TOK.uns32Literal:
	case TOK.int64Literal:
	case TOK.uns64Literal:
	case TOK.int128Literal:
	case TOK.uns128Literal:
	case TOK.float32Literal:
	case TOK.float64Literal:
	case TOK.float80Literal:
	case TOK.imaginary32Literal:
	case TOK.imaginary64Literal:
	case TOK.imaginary80Literal:
	case TOK.charLiteral:
	case TOK.wcharLiteral:
	case TOK.dcharLiteral:
	case TOK.identifier:
	case TOK.this_:
	case TOK.super_:
	case TOK.rightParentheses:
	case TOK.rightBracket:
	mixin(STRING_LITERAL_CASES);
		// nu cred ca e nevoie de analiza semantica aici, dar fac momentan asa
		// as putea pur si simplu sa traversez AST-ul si aia e
		auto symbols = getSymbolsInCompletionScope(cursorPosition, rootModule);
		string[] callTips;
		foreach (sym; *symbols)
		{
			if (to!string(sym.ident) != to!string(beforeTokens[$ - 2].ident)) // deci asta o sa mearga doar pe cazul in care am bla(|2)!!!!!!!!
				continue;

			if (auto fd = sym.isFuncDeclaration())
			{
				if (fd.isAuto()) {
					callTips ~= "auto " ~ to!string(fd) ~ to!string(fd.originalType.toChars());
				} else {
					auto sig = to!string(fd.originalType.toChars());
					auto paren = indexOf(sig, '(');
					callTips ~= sig[0..paren] ~ " " ~ to!string(fd) ~ sig[paren..$];
				}
			}
			else if (auto td = sym.isTemplateDeclaration())
			{
				callTips ~= to!string(td.toChars());
			}
			else if (auto sd = sym.isAggregateDeclaration()) // struct/class
			{
				// searching for constructor
				foreach (mem; *sd.members)
				{
					if (mem is null || mem.ident is null)
						continue;

					if (strcmp(mem.ident.toChars(), "__ctor") == 0)
					{
						auto fd = mem.isFuncDeclaration();
						auto sig = to!string(fd.type);

						// sig has the following format:
						//			"ref *struct name*(*list of params*)"
						// ref is dropped in the following
						callTips ~= sig[indexOf(sig, sd.ident.toString())..$];
					}
				}
			}
			else if (auto ad = sym.isAliasDeclaration) {
				if (auto fd = ad.aliassym.isFuncDeclaration())
				{
					if (fd.isAuto()) {
						callTips ~= "auto " ~ to!string(fd) ~ to!string(fd.originalType.toChars());
					} else {
						auto sig = to!string(fd.originalType.toChars());
						auto paren = indexOf(sig, '(');
						callTips ~= sig[0..paren] ~ " " ~ to!string(fd) ~ sig[paren..$];
					}
				}
			}
		}

		foreach (tip; callTips)
		{
			response.completions ~= AutocompleteResponse.Completion(
										null, CompletionKind.functionName,
										tip, null, 0,
										// symbol.loc.linnum, symbol.loc.charnum,
										null);
		}
		response.completionType = CompletionType.calltips;
		break;
	default:
		break;
	}
	return response;
}

/// Wrapper to check some attribute of a path, ignoring errors
/// (such as on a broken symlink).
private static bool existsAnd(alias fun)(string fn)
{
	try
		return fun(fn);
	catch (FileException e)
		return false;
}

/**
 * Params:
 *     moduleName = the name of the module being imported, in "a/b/c" style
 * Returns:
 *     The absolute path to the file that contains the module, or null if
 *     not found.
 */
string resolveImportLocation(string moduleName)
{
	assert(moduleName !is null, "module name is null");
	if (isRooted(moduleName))
		return moduleName;
	string[] alternatives;
	foreach (importPath; *global.path)
	{
		auto path = to!string(importPath);
		if (path.existsAnd!isFile)
		{
			if (path.stripExtension.endsWith(moduleName))
				alternatives ~= path;
		}
		else
		{
			string dotDi = buildPath(path, moduleName) ~ ".di";
			string dotD = dotDi[0 .. $ - 1];
			string withoutSuffix = dotDi[0 .. $ - 3];
			if (existsAnd!isFile(dotD))
				alternatives = dotD ~ alternatives;
			else if (existsAnd!isFile(dotDi))
				alternatives ~= dotDi;
			else if (existsAnd!isDir(withoutSuffix))
			{
				string packagePath = buildPath(withoutSuffix, "package.di");
				if (existsAnd!isFile(packagePath))
				{
					alternatives ~= packagePath;
					continue;
				}
				if (existsAnd!isFile(packagePath[0 .. $ - 1]))
					alternatives ~= packagePath[0 .. $ - 1];
			}
		}
	}
	return alternatives.length > 0 ? alternatives[0] : null;
}

Dsymbol[] getGlobalPublicSymbols(ref Module mod)
{
	Dsymbol[] symbols;

	foreach (s; *mod.members)
	{
		// if (__traits(hasMember, typeof(s), "protection")
		// 	&& __traits(getMember, s, "protection") == Prot.Kind.public_)
		if (s && __traits(getProtection, s) == "public"
				&& s.ident !is null && !s.isImport())
			symbols ~= s;
	}

	return symbols;
}

/**
 * Provides autocomplete for selective imports, e.g.:
 * ---
 * import std.algorithm: balancedParens;
 * ---
 */
AutocompleteResponse importCompletion(T)(T beforeTokens, ImportKind kind,
	ref Module rootModule)
in
{
	assert (beforeTokens.length >= 2);
}
body
{
	AutocompleteResponse response;
	if (beforeTokens.length <= 2)
		return response;

	size_t i = beforeTokens.length - 1;

	if (kind == ImportKind.normal)
	{
		while (beforeTokens[i].value != TOK.comma && beforeTokens[i].value != TOK.import_
				&& beforeTokens[i].value != TOK.assign)
			i--;
		setImportCompletions(beforeTokens[i .. $], response, rootModule);
		return response;
	}

	loop: while (true) switch (beforeTokens[i].value)
	{
	case TOK.identifier:
	case TOK.assign:
	case TOK.comma:
	case TOK.dot:
		i--;
		break;
	case TOK.colon:
		i--;
		while (beforeTokens[i].value == TOK.identifier || beforeTokens[i].value == TOK.dot)
			i--;
		break loop;
	default:
		break loop;
	}

	size_t j = i;
	loop2: while (j <= beforeTokens.length) switch (beforeTokens[j].value)
	{
	case TOK.colon: break loop2;
	default: j++; break;
	}

	if (i >= j)
	{
		warning("Malformed import statement");
		return response;
	}

	immutable string path = beforeTokens[i + 1 .. j]
		.filter!(token => token.value == TOK.identifier)
		.map!(token => cast() token.ident.toString()) // token.text?
		.joiner(dirSeparator)
		.text();


	string resolvedLocation = resolveImportLocation(path);
	if (resolvedLocation is null)
	{
		warning("Could not resolve location of ", path);
		return response;
	}

	rootModule.parse();
	semanticAnalysis(rootModule); //!!!!!!!!!!!!!!!!

	Dsymbol[] symbols;
	foreach (mod; rootModule.aimports) {
		if (resolvedLocation == mod.srcfile.toString())
		{
			symbols = getGlobalPublicSymbols(mod);
			break;
		}
	}

	import containers.hashset : HashSet;
	HashSet!string h;

	string partial;
	if (beforeTokens[$ - 1].value == TOK.identifier)
		partial = to!string(beforeTokens[$ - 1].ident);

	void addSymbolToResponses(Dsymbol sy)
	{
		if (auto imp = sy.isImport())
			if (imp.prot.kind != Prot.Kind.public_)
				return;

		if (sy.ident !is null && !h.contains(to!string(sy.ident))
				&& (partial is null || toUpper(sy.ident.toString()).startsWith(toUpper(partial)))
				&& !sy.isUnitTestDeclaration()
				&& resolvedLocation == to!string(sy.loc.filename)
				&& !sy.isCtorDeclaration())
		{
			response.completions ~= AutocompleteResponse.Completion(
										to!string(sy.ident),
										getSymbolCompletionKind(sy), null,
										resolvedLocation, 0,
										to!string(sy.comment));
			h.insert(to!string(sy.ident));
		}
	}

	foreach (s; symbols)
	{
		// if (s.isImport())
		// 	foreach (sy; s.type.opSlice().filter!(a => !a.skipOver))
		// 		addSymbolToResponses(sy);
		// else
			addSymbolToResponses(s);
	}
	response.completionType = CompletionType.identifiers;
	return response;
}

/**
 * Populates the response with completion information for an import statement
 * Params:
 *     tokens = the tokens after the "import" keyword and before the cursor
 *     response = the response that should be populated
 */
void setImportCompletions(T)(T tokens, ref AutocompleteResponse response,
	ref Module cache)
{
	response.completionType = CompletionType.identifiers;
	string partial = null;
	if (tokens[$ - 1].value == TOK.identifier)
	{
		partial = to!string(tokens[$ - 1].ident);
		tokens = tokens[0 .. $ - 1];
	}
	auto moduleParts = tokens.filter!(a => a.value == TOK.identifier)
							 .map!("a.ident.toString()").array();
	string path = buildPath(moduleParts);

	bool found = false;

	foreach (ip; *global.path)
	{
		auto importPath = to!string(ip);
		if (importPath.isFile)
		{
			if (!exists(importPath))
				continue;

			found = true;

			auto n = importPath.baseName(".d").baseName(".di");
			if (isFile(importPath) && (importPath.endsWith(".d") || importPath.endsWith(".di"))
					&& (partial is null || n.startsWith(partial)))
				response.completions ~= AutocompleteResponse.Completion(n, CompletionKind.moduleName, null, importPath, 0);
		}
		else
		{
			string p = buildPath(importPath, path);
			if (!exists(p))
				continue;

			found = true;

			try foreach (string name; dirEntries(p, SpanMode.shallow))
			{
				import std.path: baseName;
				if (name.baseName.startsWith(".#"))
					continue;


				auto n = name.baseName(".d").baseName(".di");
				if (isFile(name) && (name.endsWith(".d") || name.endsWith(".di"))
					&& (partial is null || n.startsWith(partial)))
					response.completions ~= AutocompleteResponse.Completion(n, CompletionKind.moduleName, null, name, 0);
				else if (isDir(name))
				{
					if (n[0] != '.' && (partial is null || n.startsWith(partial)))
					{
						immutable packageDPath = buildPath(name, "package.d");
						immutable packageDIPath = buildPath(name, "package.di");
						immutable packageD = exists(packageDPath);
						immutable packageDI = exists(packageDIPath);
						immutable kind = packageD || packageDI ? CompletionKind.moduleName : CompletionKind.packageName;
						immutable file = packageD ? packageDPath : packageDI ? packageDIPath : name;
						response.completions ~= AutocompleteResponse.Completion(n, kind, null, file, 0);
					}
				}
			}
			catch(FileException)
			{
				warning("Cannot access import path: ", importPath);
			}
		}
	}
	if (!found)
		warning("Could not find ", moduleParts);
}
