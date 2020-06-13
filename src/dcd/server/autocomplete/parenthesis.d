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
        expressionSemantic = function Expression(Expression e, Scope *sc)
                                {
                                    scope v = new CustomExpSemVisitor(sc);
                                    e.accept(v);
                                    return v.result;
                                };
		auto symbols = getSymbolsInCompletionScope(cursorPosition, rootModule);
		FunctionCallRetrieval visitor;

        if (newLoc.filename)
            visitor = new FunctionCallRetrieval(newLoc);
        else
            visitor = new FunctionCallRetrieval(startOfCall);

        rootModule.accept(visitor);

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

private extern (C++) final class CustomExpSemVisitor : ExpressionSemanticVisitor
{
    alias visit = ExpressionSemanticVisitor.visit;

    this(Scope* sc)
    {
        super(sc);
    }

    override void visit(NewExp e)
    {
        this.ExpressionSemanticVisitor.visit(e);

        if (result.isErrorExp())
        {
            result = e;
            result.type = new TypeError();
            // writeln(e);
        }
    }

    override void visit(CallExp e) {
        this.ExpressionSemanticVisitor.visit(e);

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
    Type type;

    /*
     * For visit(CallExp) and Visit(NewExp) (will present only for CallExp,
     * since NewExp will be similar):
     *
     * The normal step at the beginning of the visit method would be to
     * return if the cursor is outside the parenthesis of the call.
     * However, it's not easy to determine the end of a CallExp
     *
     * For normal calls (regular functions, methods), it's ok to:
     *   - get the loc of the expression (will point to the left
     *     parenthesis of the call for CallExp or to the beginning of the
     *     expression for NewExp)
     *   - apply to!string on the expression and get the offset at which
     *     the closing right parenthesis is relative to its matching left
     *
     * Things get a bit more complicated when dealing with calls
     * that have other calls inside their args list, because their
     * conversion to string might not be what one might think. And
     * this probably doesn't only apply to arguemnts that are calls,
     * but to any expression whose conversion unpredictable.
     *
     * Examples (will add as time passes and I find more corner cases):
     *   - static methods: a CallExp for BaseClass.method(args_list) will
     *     translate to string as only "method(args_list)"
     *   - this/super calls: this(args_list) -> "this.this(args_list)"
     *
     * Everything gets even messier if the call is on multiple lines.
     *
     * TODO: A solution that could be implemented is to first iterate
     * through the args list of the call and store the end of the last
     * argument. This might still have some inaccuracies, but it's
     * definitely better than the simple approach that only covers
     * the normal calls.
     */

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

    size_t getMatchingRightParen(const(char)* exp, size_t leftPos)
    {
        size_t additionalLeftParens = 0;
        auto length = strlen(exp);
        for (size_t i = leftPos + 1; i < length; ++i)
        {
            if (exp[i] == '(')
                additionalLeftParens++;
            else if (exp[i] == ')')
            {
                if (additionalLeftParens)
                    --additionalLeftParens;
                else
                    return i;
            }
        }

        return size_t.max;
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
        // writeln(to!string(e.type.toPrettyChars()));
        if (shouldSkip(e.loc))
            return;

        auto stringConv = to!string(e);
        // writeln(stringConv);
        // writeln(e.loc);
        // writeln(indexOf(stringConv, '('));
        // writeln(indexOf(stringConv, ')'));

        auto leftParenthesisIndex = indexOf(stringConv, '(');

        if (e.loc.linnum != loc.linnum || e.loc.charnum + leftParenthesisIndex >= loc.charnum)
            return;

        // need a closer look here !!!
        auto matchingRightParen = getMatchingRightParen(stringConv.ptr, leftParenthesisIndex);
        if (matchingRightParen != size_t.max
            && loc.charnum > e.loc.charnum + matchingRightParen) // might need to add +1 here, will see
            return;


        foreach (arg; *e.arguments)
        {
            if (!found)
                arg.accept(this);
        }

        if (!found) {
            found = true;
        }
    }

    override void visit(ASTCodegen.CallExp e)
    {
        if (shouldSkip(e.loc))
            return;

        // auto stringConv = to!string(e);
        // auto leftParenthesisIndex = indexOf(stringConv, '(');

        // if (e.loc.linnum != loc.linnum || e.loc.charnum >= loc.charnum)
        //     return;

        // const auto matchingRightParen = getMatchingRightParen(stringConv.ptr, leftParenthesisIndex);
        // if (matchingRightParen != size_t.max
        //     && loc.charnum >= e.loc.charnum
        //         + matchingRightParen - leftParenthesisIndex + 1)
        //     return;
        if (!e.loc.isEqual(loc))
            return;

        writeln("call exp " ~ to!string(e));

        foreach (arg; *e.arguments)
            if (!found)
                arg.accept(this);


        if (!found) {
            // checking to see if it's DotVarExp: instance.call(args)
            // e.e1.accept(this);
            if (e.e1)
            {
                auto dotExp = e.e1.isDotVarExp();
                if (dotExp)
                {
                    type = dotExp.e1.type;
                }
            }

            found = true;
        }
    }
}