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

module dcd.server.autocomplete.util;

import std.algorithm;
import stdx.allocator;
import std.experimental.logger;
import std.range;
import std.string;
import std.typecons;

import dcd.common.messages;

import dmd.dsymbol;
import dmd.dscope;
import dmd.dmodule;
import dmd.tokens;
import dmd.lexer;
import dmd.globals;
import dmd.declaration;
import dmd.dimport;
import dmd.func;

import std.stdio : writeln;

enum ImportKind : ubyte
{
	selective,
	normal,
	neither
}

struct SymbolStuff
{
	void destroy()
	{
		typeid(Dsymbol).destroy(symbol);
		typeid(Scope).destroy(scope_);
	}

	Dsymbol*[] symbols;
	Dsymbol* symbol;
	Scope* scope_;
}

/**
 * Params:
 *     completionType = the completion type being requested
 *     kind = the kind of the current item in the completion chain
 *     current = the index of the current item in the symbol chain
 *     max = the number of items in the symbol chain
 * Returns:
 *     true if the symbol should be swapped with its type field
 */
bool shouldSwapWithType(CompletionType completionType, Dsymbol symbol,
	size_t current, size_t max) pure nothrow @safe
{
	// packages never have types, so always return false
	if (symbol.isPackage()
		|| symbol.isClassDeclaration()
		|| symbol.isStructDeclaration()
		|| symbol.isInterfaceDeclaration()
		|| symbol.isEnumDeclaration()
		|| symbol.isUnionDeclaration()
		|| symbol.isTemplateDeclaration())
		/*|| kind == CompletionKind.keyword)*/ //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	{
		return false;
	}
	// Swap out every part of a chain with its type except the last part
	if (current < max)
		return true;
	// Only swap out types for these kinds
	immutable bool isInteresting =
		symbol.isVarDeclaration()
		|| symbol.isImport()
		|| symbol.isAliasDeclaration()
		|| symbol.isEnumMember()
		|| symbol.isFuncDeclaration();
	return isInteresting && (completionType == CompletionType.identifiers
		|| (completionType == completionType.calltips && symbol.isVarDeclaration())) ;
}

// istring stringToken()(auto ref const Token a)
// {
// 	return internString(a.text is null ? str(a.type) : a.text);
// }

//void dumpTokens(const Token[] tokens)
//{
	//foreach (t; tokens)
		//writeln(t.line, ":", t.column, " ", stringToken(t));
//}

/**
 * Params:
 *     sourceCode = the source code of the file being edited
 *     cursorPosition = the cursor position in bytes
 * Returns:
 *     a sorted range of tokens before the cursor position
 */
auto getTokensBeforeCursor(const(ubyte[]) sourceCode, Loc cursorPosition,
	out const(Token)[] tokenArray, Module rootModule)
{
	auto lexer = new Lexer(rootModule.srcfile.toChars(),
							(cast(const(char)[])rootModule.srcBuffer.data).ptr,
							0,
							rootModule.srcBuffer.data.length,
							false,
							false);

	// sa nu am probleme aici cu comentariile, ca se facea diferit in libdparse!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    while (lexer.nextToken()) {
        tokenArray ~= lexer.token;

        if (lexer.token.value == TOK.endOfFile) break;
    }

	const(Token)[] beforeTokens;
	foreach (tok; tokenArray) {
		if (tok.loc >= cursorPosition) {
			break;
		}
		// writeln(tok.value);
		beforeTokens ~= tok;
	}
	return beforeTokens;
}

/**
 * Params:
 *     request = the autocompletion request
 *     type = type the autocompletion type
 * Returns:
 *     all symbols that should be considered for the autocomplete list based on
 *     the request's source code, cursor position, and completion type.
 */
SymbolStuff getSymbolsForCompletion(const AutocompleteRequest request,
	const CompletionType type, IAllocator allocator)
{
	const(Token)[] tokenArray;
	// auto beforeTokens = getTokensBeforeCursor(request.sourceCode,
	// 	request.cursorPosition, cache, tokenArray);
	// ScopeSymbolPair pair = generateAutocompleteTrees(tokenArray, allocator,
	// 	rba, request.cursorPosition, moduleCache);
	// auto expression = getExpression(beforeTokens);
	// return SymbolStuff(getSymbolsByTokenChain(pair.scope_, expression,
	// 	request.cursorPosition, type), pair.symbol, pair.scope_);
	return SymbolStuff(); ///////?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

bool isSliceExpression(T)(T tokens, size_t index) //!!!!!!!!!!!!!!
{
	while (index < tokens.length) switch (tokens[index].value)
	{
	case TOK.leftBracket:
		tokens.skipParen(index, TOK.leftBracket, TOK.rightBracket);
		break;
	case TOK.leftParentheses:
		tokens.skipParen(index, TOK.leftParentheses, TOK.rightParentheses);
		break;
	case TOK.rightBracket:
	case TOK.rightCurly:
		return false;
	case TOK.slice:
		return true;
	default:
		index++;
		break;
	}
	return false;
}

/**
 *
 */
// Dsymbol*[] getSymbolsByTokenChain(T)(Scope* completionScope,
// 	T tokens, Loc cursorPosition, CompletionType completionType)
// {
// 	//writeln(">>>");
// 	//dumpTokens(tokens.release);
// 	//writeln(">>>");

// 	// Find the symbol corresponding to the beginning of the chain
// 	Dsymbol*[] symbols;
// 	if (tokens.length == 0)
// 		return [];
// 	// Recurse in case the symbol chain starts with an expression in parens
// 	// e.g. (a.b!c).d
// 	if (tokens[0].value == /*tok!"("*/TOK.leftParentheses)
// 	{
// 		size_t j;
// 		tokens.skipParen(j, /*tok!"("*/TOK.leftParentheses, /*tok!")"*/ TOK.rightParentheses);
// 		if (j > 1)
// 		{
// 			symbols = getSymbolsByTokenChain(completionScope, tokens[1 .. j],
// 				cursorPosition, completionType);
// 			tokens = tokens[j + 1 .. $];
// 		}
// 		//writeln("<<<");
// 		//dumpTokens(tokens.release);
// 		//writeln("<<<");
// 		if (tokens.length == 0) // workaround (#371)
// 			return [];
// 	}
// 	else if (tokens[0].value == /*tok!"."*/ TOK.dot && tokens.length >= 1)
// 	{
// 		if (tokens.length == 1)
// 		{
// 			// Module Scope Operator
// 			auto s = completionScope.getScopeByCursor(1);
// 			return s.symbols.map!(a => a.ptr).filter!(a => a !is null).array;
// 		}
// 		else
// 		{
// 			tokens = tokens[1 .. $];
// 			symbols = completionScope.getSymbolsAtGlobalScope(stringToken(tokens[0]));
// 		}
// 	}
// 	else
// 		symbols = completionScope.getSymbolsByNameAndCursor(stringToken(tokens[0]), cursorPosition);

// 	if (symbols.length == 0)
// 	{
// 		//TODO: better bugfix for issue #368, see test case 52 or pull #371
// 		if (tokens.length)
// 			warning("Could not find declaration of ", stringToken(tokens[0]),
// 				" from position ", cursorPosition);
// 		else assert(0, "internal error");
// 		return [];
// 	}

// 	// If the `symbols` array contains functions, and one of them returns
// 	// void and the others do not, this is a property function. For the
// 	// purposes of chaining auto-complete we want to ignore the one that
// 	// returns void. This is a no-op if we are getting doc comments.
// 	void filterProperties() @nogc @safe
// 	{
// 		if (symbols.length == 0 || completionType == CompletionType.ddoc)
// 			return;
// 		if (symbols[0].kind == CompletionKind.functionName
// 			|| symbols[0].qualifier == SymbolQualifier.func)
// 		{
// 			int voidRets = 0;
// 			int nonVoidRets = 0;
// 			size_t firstNonVoidIndex = size_t.max;
// 			foreach (i, sym; symbols)
// 			{
// 				if (sym.type is null)
// 					return;
// 				if (&sym.type.name[0] == &getBuiltinTypeName(tok!"void")[0])
// 					voidRets++;
// 				else
// 				{
// 					nonVoidRets++;
// 					firstNonVoidIndex = min(firstNonVoidIndex, i);
// 				}
// 			}
// 			if (voidRets > 0 && nonVoidRets > 0)
// 				symbols = symbols[firstNonVoidIndex .. $];
// 		}
// 	}

// 	filterProperties();

// 	// if (shouldSwapWithType(completionType, symbols[0].kind, 0, tokens.length - 1)) /// !!!!!!!!!!!!!!!!!!!!!!
// 	// {
// 	// 	//trace("Swapping types");
// 	// 	if (symbols.length == 0 || symbols[0].getType() is null || symbols[0].getType() is symbols[0])
// 	// 		return [];
// 	// 	else if (symbols[0].type.kind == CompletionKind.functionName)
// 	// 	{
// 	// 		if (symbols[0].type.type is null)
// 	// 			symbols = [];
// 	// 		else
// 	// 			symbols = [symbols[0].type.type];
// 	// 	}
// 	// 	else
// 	// 		symbols = [symbols[0].type];
// 	// }

// 	loop: for (size_t i = 1; i < tokens.length; i++)
// 	{
// 		void skip(TOK open, TOK close)
// 		{
// 			tokens.skipParen(i, open, close);
// 		}

// 		switch (tokens[i].value)
// 		{
// 		// case tok!"int":
// 		// case tok!"uint":
// 		// case tok!"long":
// 		// case tok!"ulong":
// 		// case tok!"char":
// 		// case tok!"wchar":
// 		// case tok!"dchar":
// 		// case tok!"bool":
// 		// case tok!"byte":
// 		// case tok!"ubyte":
// 		// case tok!"short":
// 		// case tok!"ushort":
// 		// case tok!"cent":
// 		// case tok!"ucent":
// 		// case tok!"float":
// 		// case tok!"ifloat":
// 		// case tok!"cfloat":
// 		// case tok!"idouble":
// 		// case tok!"cdouble":
// 		// case tok!"double":
// 		// case tok!"real":
// 		// case tok!"ireal":
// 		// case tok!"creal":
// 		// case tok!"this":
// 		// case tok!"super":
// 		case TOK.int8:
// 		case TOK.uns8:
// 		case TOK.int16:
// 		case TOK.uns16:
// 		case TOK.int32:
// 		case TOK.uns32:
// 		case TOK.int64:
// 		case TOK.uns64:
// 		case TOK.int128:
// 		case TOK.uns128:
// 		case TOK.float32:
// 		case TOK.float64:
// 		case TOK.float80:
// 		case TOK.bool_:
// 		case TOK.char_:
// 		case TOK.wchar_:
// 		case TOK.dchar_:
// 		case TOK.imaginary32:
// 		case TOK.imaginary64:
// 		case TOK.imaginary80:
// 		case TOK.complex32:
// 		case TOK.complex64:
// 		case TOK.complex80:
// 		case TOK.this_:
// 		case TOK.super_:
// 			symbols = symbols[0].getPartsByName(internString(str(tokens[i].type)));
// 			if (symbols.length == 0)
// 				break loop;
// 			break;
// 		case /*tok!"identifier"*/ TOK.identifier:
// 			//trace(symbols[0].qualifier, " ", symbols[0].kind);
// 			filterProperties();

// 			if (symbols.length == 0)
// 				break loop;

// 			// Use type instead of the symbol itself for certain symbol kinds
// 			// while (symbols[0].qualifier == SymbolQualifier.func
// 			// 	|| symbols[0].kind == CompletionKind.functionName
// 			// 	|| (symbols[0].kind == CompletionKind.moduleName
// 			// 		&& symbols[0].type !is null && symbols[0].type.kind == CompletionKind.importSymbol)
// 			// 	|| symbols[0].kind == CompletionKind.importSymbol
// 			// 	|| symbols[0].kind == CompletionKind.aliasName)
// 			// while(cast(FuncDeclaration)symbols[0] !is null
// 			// 	|| cast(Module)symbols[0] !is null
// 			// 	|| cast(AliasDeclaration)symbols[0] !is null)
// 			// {
// 			// 	symbols = symbols[0].type is null || symbols[0].type is symbols[0] ? [] : [symbols[0].type];
// 			// 	if (symbols.length == 0)
// 			// 		break loop;
// 			// } ???????????????????

// 			//trace("looking for ", tokens[i].text, " in ", symbols[0].name);
// 			symbols = symbols[0].getPartsByName(internString(tokens[i].text));
// 			//trace("symbols: ", symbols.map!(a => a.name));
// 			filterProperties();
// 			if (symbols.length == 0)
// 			{
// 				//trace("Couldn't find it.");
// 				break loop;
// 			}
// 			if (shouldSwapWithType(completionType, symbols[0].kind, i, tokens.length - 1))
// 			{
// 				symbols = symbols[0].type is null || symbols[0].type is symbols[0] ? [] : [symbols[0].type];
// 				if (symbols.length == 0)
// 					break loop;
// 			}
// 			if ((symbols[0].kind == CompletionKind.aliasName
// 				|| symbols[0].kind == CompletionKind.moduleName)
// 				&& (completionType == CompletionType.identifiers
// 				|| i + 1 < tokens.length))
// 			{
// 				symbols = symbols[0].type is null || symbols[0].type is symbols[0] ? [] : [symbols[0].type];
// 			}
// 			if (symbols.length == 0)
// 				break loop;
// 			if (tokens[i].type == tok!"!")
// 			{
// 				i++;
// 				if (tokens[i].type == tok!"(")
// 					goto case;
// 				else
// 					i++;
// 			}
// 			break;
// 		case tok!"(":
// 			skip(tok!"(", tok!")");
// 			break;
// 		case tok!"[":
// 			if (symbols.length == 0)
// 				break loop;
// 			if (symbols[0].qualifier == SymbolQualifier.array)
// 			{
// 				skip(tok!"[", tok!"]");
// 				if (!isSliceExpression(tokens, i))
// 				{
// 					symbols = symbols[0].type is null || symbols[0].type is symbols[0] ? [] : [symbols[0].type];
// 					if (symbols.length == 0)
// 						break loop;
// 				}
// 			}
// 			else if (symbols[0].qualifier == SymbolQualifier.assocArray)
// 			{
// 				symbols = symbols[0].type is null || symbols[0].type is symbols[0] ? [] : [symbols[0].type];
// 				skip(tok!"[", tok!"]");
// 			}
// 			else
// 			{
// 				skip(tok!"[", tok!"]");
// 				DSymbol*[] overloads;
// 				if (isSliceExpression(tokens, i))
// 					overloads = symbols[0].getPartsByName(internString("opSlice"));
// 				else
// 					overloads = symbols[0].getPartsByName(internString("opIndex"));
// 				if (overloads.length > 0)
// 				{
// 					symbols = overloads[0].type is null ? [] : [overloads[0].type];
// 				}
// 				else
// 					return [];
// 			}
// 			break;
// 		case tok!".":
// 			break;
// 		default:
// 			break loop;
// 		}
// 	}
// 	return symbols;
// }

	// case tok!"int":
	// case tok!"uint":
	// case tok!"long":
	// case tok!"ulong":
	// case tok!"char":
	// case tok!"wchar":
	// case tok!"dchar":
	// case tok!"bool":
	// case tok!"byte":
	// case tok!"ubyte":
	// case tok!"short":
	// case tok!"ushort":
	// case tok!"cent":
	// case tok!"ucent":
	// case tok!"float":
	// case tok!"ifloat":
	// case tok!"cfloat":
	// case tok!"idouble":
	// case tok!"cdouble":
	// case tok!"double":
	// case tok!"real":
	// case tok!"ireal":
	// case tok!"creal":
	// case tok!"this":
	// case tok!"super":
	// case tok!"identifier":
enum TYPE_IDENT_CASES = q{
	case TOK.int8:
	case TOK.uns8:
	case TOK.int16:
	case TOK.uns16:
	case TOK.int32:
	case TOK.uns32:
	case TOK.int64:
	case TOK.uns64:
	case TOK.int128:
	case TOK.uns128:
	case TOK.float32:
	case TOK.float64:
	case TOK.float80:
	case TOK.bool_:
	case TOK.char_:
	case TOK.wchar_:
	case TOK.dchar_:
	case TOK.imaginary32:
	case TOK.imaginary64:
	case TOK.imaginary80:
	case TOK.complex32:
	case TOK.complex64:
	case TOK.complex80:
	case TOK.this_:
	case TOK.super_:
	case TOK.identifier:
};

	// case tok!"stringLiteral":
	// case tok!"wstringLiteral":
	// case tok!"dstringLiteral":
enum STRING_LITERAL_CASES = q{
	case TOK.string_:
    case TOK.hexadecimalString:
};

enum TYPE_IDENT_AND_LITERAL_CASES = TYPE_IDENT_CASES ~ STRING_LITERAL_CASES;

/**
 *
 */
T getExpression(T)(T beforeTokens)
{
	enum EXPRESSION_LOOP_BREAK = q{
		if (i + 1 < beforeTokens.length) switch (beforeTokens[i + 1].value)
		{
		mixin (TYPE_IDENT_AND_LITERAL_CASES);
			i++;
			break expressionLoop;
		default:
			break;
		}
	};

	if (beforeTokens.length == 0)
		return beforeTokens[0 .. 0];
	size_t i = beforeTokens.length - 1;
	size_t sliceEnd = beforeTokens.length;
	TOK open;
	TOK close;
	uint skipCount = 0;

	expressionLoop: while (true)
	{
		switch (beforeTokens[i].value)
		{
		case TOK.import_:
			i++;
			break expressionLoop;
		mixin (TYPE_IDENT_AND_LITERAL_CASES);
			mixin (EXPRESSION_LOOP_BREAK);
			break;
		case TOK.dot:
			break;
		case TOK.rightParentheses:
			open = TOK.rightParentheses;
			close = TOK.leftParentheses;
			goto skip;
		case TOK.rightBracket:
			open = TOK.rightBracket;
			close = TOK.leftBracket;
		skip:
			mixin (EXPRESSION_LOOP_BREAK);
			immutable bookmark = i;
			i = beforeTokens.skipParenReverse(i, open, close);

			skipCount++;

			// check the current token after skipping parens to the left.
			// if it's a loop keyword, pretend we never skipped the parens.
			if (i > 0) switch (beforeTokens[i - 1].value)
			{
				case TOK.scope_:
				case TOK.if_:
				case TOK.while_:
				case TOK.for_:
				case TOK.foreach_:
				case TOK.foreach_reverse_:
				case TOK.do_:
				case TOK.cast_:
				case TOK.catch_:
					i = bookmark + 1;
					break expressionLoop;
				case TOK.not:
					// only break if the bang is for a template instance
					if (i - 2 >= 0  && beforeTokens[i - 2].value == TOK.identifier && skipCount == 1)
					{
						sliceEnd = i - 1;
						i -= 2;
						break expressionLoop;
					}
					break;
				default:
					break;
			}
			break;
		default:
			i++;
			break expressionLoop;
		}
		if (i == 0)
			break;
		else
			i--;
	}
	return beforeTokens[i .. sliceEnd];
}

/**
 * Determines if an import is selective, whole-module, or neither.
 */
ImportKind determineImportKind(T)(T tokens)
{
	assert (tokens.length > 1);
	size_t i = tokens.length - 1;

	if (!(tokens[i].value == TOK.colon || tokens[i].value == TOK.comma
			|| tokens[i].value == TOK.dot || tokens[i].value == TOK.identifier))
		return ImportKind.neither;

	bool foundColon = false;
	while (true) switch (tokens[i].value)
	{
	case TOK.colon:
		foundColon = true;
		goto case;
	case TOK.identifier:
	case /*tok!"="*/ TOK.assign:
	case TOK.dot:
	case TOK.comma:
		if (i == 0)
			return ImportKind.neither;
		else
			i--;
		break;
	case TOK.import_:
		return foundColon ? ImportKind.selective : ImportKind.normal;
	default:
		return ImportKind.neither;
	}
	return ImportKind.neither;
}

unittest
{
	import std.stdio : writeln;

	Token[] t = [
		Token(tok!"import"), Token(tok!"identifier"), Token(tok!"."),
		Token(tok!"identifier"), Token(tok!":"), Token(tok!"identifier"), Token(tok!",")
	];
	assert(determineImportKind(t) == ImportKind.selective);
	Token[] t2;
	t2 ~= Token(tok!"else");
	t2 ~= Token(tok!":");
	assert(determineImportKind(t2) == ImportKind.neither);
	writeln("Unittest for determineImportKind() passed");
}

bool isUdaExpression(T)(ref T tokens)
{
	bool result;
	ptrdiff_t skip;
	auto i = cast(ptrdiff_t) tokens.length - 2;

	if (i < 1)
		return result;

	// skips the UDA ctor
	if (tokens[i].value == /*tok!")"*/ TOK.rightParentheses)
	{
		++skip;
		--i;
		while (i >= 2)
		{
			skip += tokens[i].value == /*tok!")"*/ TOK.rightParentheses;
			skip -= tokens[i].value == /*tok!"("*/ TOK.leftParentheses;
			--i;
			if (skip == 0)
			{
				// @UDA!(TemplateParameters)(FunctionParameters)
				if (i > 3 && tokens[i].value == /*tok!"!"*/ TOK.not && tokens[i-1].value == /*tok!")"*/ TOK.rightParentheses)
				{
					skip = 1;
					i -= 2;
					continue;
				}
				else break;
			}
		}
	}

	if (skip == 0)
	{
		// @UDA!SingleTemplateParameter
		if (i > 2 && tokens[i].value == TOK.identifier && tokens[i-1].value == /*tok!"!"*/TOK.not)
		{
			i -= 2;
		}

		// @UDA
		if (i > 0 && tokens[i].value == TOK.identifier && tokens[i-1].value == /*tok!"@"*/TOK.at)
		{
			result = true;
		}
	}

	return result;
}

/**
 * Traverses a token slice in reverse to find the opening parentheses or square bracket
 * that begins the block the last token is in.
 */
size_t goBackToOpenParen(T)(T beforeTokens)
in
{
	assert (beforeTokens.length > 0);
}
body
{
	size_t i = beforeTokens.length - 1;
	while (true) switch (beforeTokens[i].value)
	{
	case TOK.comma:
	case TOK.dot:
	case TOK.star: // TOK.mul?????
	case TOK.address: // TOK.and???
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
	mixin(TYPE_IDENT_AND_LITERAL_CASES);
		if (i == 0)
			return size_t.max;
		else
			i--;
		break;
	case TOK.leftParentheses:
	case TOK.leftBracket:
		return i + 1;
	case TOK.rightParentheses:
		i = beforeTokens.skipParenReverseBefore(i, TOK.rightParentheses, TOK.leftParentheses);
		break;
	case TOK.rightCurly:
		i = beforeTokens.skipParenReverseBefore(i, TOK.rightCurly, TOK.leftCurly);
		break;
	case TOK.rightBracket:
		i = beforeTokens.skipParenReverseBefore(i, TOK.rightBracket, TOK.leftBracket);
		break;
	default:
		return size_t.max;
	}
}

/**
 * Skips blocks of parentheses until the starting block has been closed
 */
void skipParen(T)(T tokenSlice, ref size_t i, TOK open, TOK close)
{
	if (i >= tokenSlice.length || tokenSlice.length <= 0)
		return;
	int depth = 1;
	while (depth != 0 && i + 1 != tokenSlice.length)
	{
		i++;
		if (tokenSlice[i].value == open)
			depth++;
		else if (tokenSlice[i].value == close)
			depth--;
	}
}

/**
 * Skips blocks of parentheses in reverse until the starting block has been opened
 */
size_t skipParenReverse(T)(T beforeTokens, size_t i, TOK open, TOK close)
{
	if (i == 0)
		return 0;
	int depth = 1;
	while (depth != 0 && i != 0)
	{
		i--;
		if (beforeTokens[i].value == open)
			depth++;
		else if (beforeTokens[i].value == close)
			depth--;
	}
	return i;
}

size_t skipParenReverseBefore(T)(T beforeTokens, size_t i, TOK open, TOK close)
{
	i = skipParenReverse(beforeTokens, i, open, close);
	if (i != 0)
		i--;
	return i;
}

///
unittest
{
	Token[] t = [
		Token(tok!"identifier"), Token(tok!"identifier"), Token(tok!"("),
		Token(tok!"identifier"), Token(tok!"("), Token(tok!")"), Token(tok!",")
	];
	size_t i = t.length - 1;
	i = skipParenReverse(t, i, tok!")", tok!"(");
	assert(i == 2);
	i = t.length - 1;
	i = skipParenReverseBefore(t, i, tok!")", tok!"(");
	assert(i == 1);
}

AutocompleteResponse.Completion makeSymbolCompletionInfo(Dsymbol symbol)
{
	import std.conv;

	string definition;
	if ((symbol.isVarDeclaration()) && symbol.getType())
		definition = to!string(symbol.getType()) ~ ' ' ~ to!string(symbol.ident);
	else if (symbol.isEnumMember())
		definition = to!string(symbol.ident); // TODO: add enum value to definition string
	// else
	// 	definition = symbol.callTip; // de implementat calltip???????????????????????????????????????????????????????????


	// // TODO: definition strings could include more information, like on classes inheritance
	// return AutocompleteResponse.Completion(symbol.ident.toString(), kind, definition,
	// 	symbol.getModule().srcfile.toString(), symbol.loc, symbol.comment);

	return AutocompleteResponse.Completion(to!string(symbol.ident.toString()), 0, null,
		to!string(symbol.getModule().srcfile.toString()), 0, to!string(symbol.comment));
}
