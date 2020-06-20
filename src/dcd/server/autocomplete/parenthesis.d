module dcd.server.autocomplete.parenthesis;

import dcd.server.autocomplete.util;

import dcd.common.constants;
import dcd.common.messages;

import core.stdc.string;

import dmd.tokens;
import dmd.globals;
import dmd.dmodule;
import dmd.expression;
import dmd.expressionsem;
import dmd.dscope;
import dmd.visitor;
import dmd.mtype;
import dmd.astcodegen;
import dmd.compiler;
import dmd.arraytypes;
import dmd.dsymbol;
import dmd.func;
import dmd.aggregate;

import std.conv : to;
import std.string;
import std.stdio : writeln;

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

    auto i = beforeTokens.length - 1;
    int rightParenthesis = 0;
    while (i >= 0 && !(beforeTokens[i].value == TOK.leftParentheses && !rightParenthesis))
    {
        if (beforeTokens[i].value == TOK.rightParentheses)
            rightParenthesis++;
        else if (beforeTokens[i].value == TOK.leftParentheses)
            rightParenthesis--;
        i--;
    }
    Loc startOfCall = beforeTokens[i].loc; // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Loc newLoc = Loc(null, 0, 0);
    if (i >= 2 && beforeTokens[i - 2].value == TOK.new_)
        newLoc = beforeTokens[i - 2].loc;

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

        // offering alternative semantic analysis through the custom visitor
        Compiler.alternativeExpressionSemantic =
            function Expression(Expression e, Scope *sc)
                {
                    scope v = new CustomExpSemVisitor(sc);
                    e.accept(v);
                    return v.result;
                };
		auto symbols = getSymbolsInCompletionScope(cursorPosition, rootModule);

        // retrieving the call/new expression at cursor level
		FunctionCallRetrieval visitor;
        if (newLoc.filename)
            visitor = new FunctionCallRetrieval(newLoc);
        else
            visitor = new FunctionCallRetrieval(startOfCall);
        rootModule.accept(visitor);


        string[] callTips;
        createCallTipsForExpression(visitor.e, callTips, symbols);

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

private void createCallTipsForExpression(Expression e, ref string[] callTips,
                                        Dsymbols* symbols)
{
    if (auto callExp = e.isCallExp())
    {
        if (auto dotExp = callExp.e1.isDotVarExp())
        {
            createCallTipsForDotVarExp(dotExp, callTips, symbols);
        }
        else
        {
            createCallTipsForCallExp(callExp, callTips, symbols);
        }
    }
    else if (auto newExp = e.isNewExp())
    {
        createCallTipsForNewExp(newExp, callTips, symbols);
    }
}

private void createCallTipsForFuncDeclaration(FuncDeclaration fd, ref string[] callTips)
{
    if (fd.isAuto()) {
        callTips ~= "auto " ~ to!string(fd) ~ to!string(fd.originalType.toChars());
    } else {
        auto sig = to!string(fd.originalType.toChars());
        auto paren = indexOf(sig, '(');
        callTips ~= sig[0..paren] ~ " " ~ to!string(fd) ~ sig[paren..$];
    }
}

private void createCallTipsForAggregateDeclaraton(AggregateDeclaration ad, ref string[] callTips)
{
    foreach (mem; *ad.members)
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
                callTips ~= sig[indexOf(sig, ad.ident.toString())..$];
            }
        }
}

private void createCallTipsForSymbol(Dsymbol sym, ref string[] callTips)
{
    if (auto fd = sym.isFuncDeclaration())
    {
        createCallTipsForFuncDeclaration(fd, callTips);
    }
    else if (auto td = sym.isTemplateDeclaration())
    {
        callTips ~= to!string(td.toChars());
    }
    else if (auto ad = sym.isAggregateDeclaration()) // struct/class
    {
        // searching for constructor
        createCallTipsForAggregateDeclaraton(ad, callTips);
    }
    else if (auto ad = sym.isAliasDeclaration) {
        if (auto fd = ad.aliassym.isFuncDeclaration())
        {
            createCallTipsForFuncDeclaration(fd, callTips);
        }
    }
}

private void createCallTipsForDotVarExp(DotVarExp e, ref string[] callTips,
                                        Dsymbols* symbols)
{
    auto dotVarExpType = e.e1.type;
    auto dotVarCallId = e.var;
    // writeln(to!string(dotVarExpType.toPrettyChars()));
    // writeln(dotExp.var);
    Dsymbol aggregateDeclaration = null;
    foreach (sym; *symbols)
    {
        if (sym is null || sym.ident is null)
            continue;

        if (strcmp(sym.ident.toChars(), dotVarExpType.toPrettyChars()) == 0)
        {
            aggregateDeclaration = sym;
            break;
        }
    }

    if (aggregateDeclaration)
    {
        auto ad = aggregateDeclaration.isAggregateDeclaration();
        foreach (field; *ad.members)
        {
            if (field is null || field.ident is null)
                continue;

            if (strcmp(field.ident.toChars(), dotVarCallId.toChars()) == 0)
                createCallTipsForSymbol(field, callTips);
        }
    }
}

private void createCallTipsForCallExp(CallExp e, ref string[] callTips,
                                        Dsymbols* symbols)
{
    auto callId = e.e1;
    // writeln(e.e1);
    foreach (sym; *symbols)
    {
        if (sym is null || sym.ident is null
            || strcmp(sym.ident.toChars(), callId.toChars()) != 0)
            continue;

        if (auto fd = sym.isFuncDeclaration())
            createCallTipsForFuncDeclaration(fd, callTips);
    }
}

private void createCallTipsForNewExp(NewExp e, ref string[] callTips,
                                        Dsymbols* symbols)
{
    foreach (sym; *symbols)
    {
        if (sym is null || sym.ident is null
            || strcmp(sym.ident.toChars(), e.type.toPrettyChars()) != 0)
            continue;

        if (auto ad = sym.isAggregateDeclaration())
            createCallTipsForAggregateDeclaraton(ad, callTips);
    }
}

private extern (C++) final class CustomExpSemVisitor : ExpressionSemanticVisitorImpl
{
    alias visit = ExpressionSemanticVisitorImpl.visit;

    this(Scope* sc)
    {
        super(sc);
    }

    override void visit(NewExp e)
    {
        this.ExpressionSemanticVisitorImpl.visit(e);

        if (result.isErrorExp())
        {
            result = e;
            result.type = new TypeError();
            // writeln(e);
        }
    }

    override void visit(CallExp e) {
        this.ExpressionSemanticVisitorImpl.visit(e);

        if (result.isErrorExp())
        {
            result = e;
            result.type = new TypeError();
            // writeln(e);
        }
    }
}

private extern (C++) class FunctionCallRetrieval : SemanticTimeTransitiveVisitor
{
    alias visit = SemanticTimeTransitiveVisitor.visit;

    Loc loc;
    bool found;
    Expression e;

    private bool shouldSkip(Loc loc)
    {
        return found || loc.filename is null
                || strcmp(loc.filename, loc.filename) != 0;
    }

    this(Loc loc)
    {
        this.loc = loc;
        found = false;
    }

    override void visit(ASTCodegen.Module m)
    {
        if (m.members)
            foreach (s; *m.members)
            {
                if (s.isImport() || shouldSkip(s.loc))
                    continue;

                s.accept(this);
            }
    }

    override void visit(ASTCodegen.NewExp e)
    {
        if (shouldSkip(e.loc))
            return;

        // new Foo(...);
        // new(...) Foo(...);

        if (!e.loc.isEqual(loc))
        {
            foreach (arg; *e.arguments)
            {
                if (!found)
                    arg.accept(this);
            }
        }
        else
        {
            // writeln("new exp " ~ to!string(e));
            // writeln(to!string(e.type.toPrettyChars()));
            this.e = e;
            found = true;
        }
    }

    override void visit(ASTCodegen.CallExp e)
    {
        if (shouldSkip(e.loc))
            return;

        if (!e.loc.isEqual(loc))
        {
            foreach (arg; *e.arguments)
                if (!found)
                    arg.accept(this);
        }
        else
        {
            // writeln("call exp " ~ to!string(e));
            // checking to see if it's DotVarExp: instance.call(args)
            this.e = e;
            found = true;
        }
    }
}