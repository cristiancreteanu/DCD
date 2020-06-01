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

module dcd.server.autocomplete.doc;

import core.stdc.string : strcmp;

import std.algorithm;
import std.array;
import std.experimental.logger;
import std.typecons;
import std.conv : to;

import dcd.server.autocomplete.util;
import dcd.common.messages;

import dmd.dmodule;
import dmd.tokens;
import dmd.globals;

import std.stdio : writeln;

/**
 * Gets documentation for the symbol at the cursor
 * Params:
 *     request = the autocompletion request
 * Returns:
 *     the autocompletion response
 */
public AutocompleteResponse getDoc(const AutocompleteRequest request,
	ref Module rootModule)
{
//	trace("Getting doc comments");
	AutocompleteResponse response;

	auto cursorLoc = Loc(rootModule.srcfile.toChars(), request.cursorLinnum, request.cursorCharnum);
	const(Token)[] tokenArray;
	auto beforeTokens = getTokensBeforeCursor(request.sourceCode,
		cursorLoc, tokenArray, rootModule);

	auto symbols = getSymbolsInCompletionScope(cursorLoc, rootModule);

	foreach (sym; *symbols) {
		if (strcmp(sym.ident.toChars(), beforeTokens[$ - 1].ident.toChars()) == 0)
		{
			writeln(to!string(sym.comment));
			break;
		}
	}


	return response;
}
