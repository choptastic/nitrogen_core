%% vim: ft=erlang
Definitions.

START_TAG = \<\#\s*[^\/][^\>]*\>
END_TAG = \<\/\#\s*[^\>]*\>
COMMENT = \<\!--.*?--\>
WHITESPACE = [\r\n\t\n\s]
%VAR1 = \@[A-Z]
%VAR2 = \@[A-Z_][A-Za-z0-9]
VAR3 = \@[A-Z_][A-Za-z0-9_][A-Za-z0-9_]+
CHAR = .
%%START_TAG = \<\s*[^\/][^\>]*\>
%%END_TAG = \<\/\s*[^\>]*\>
%%SELF_CLOSED_TAG = \<\s*[^\/][^\>]* \/\>

Rules.

{START_TAG} : {token, open_tag(TokenLine, TokenChars)}.
{END_TAG} : {token, close_tag(TokenLine, TokenChars)}.
{COMMENT} : {token, {wf_comment, TokenLine, TokenChars}}.
{WHITESPACE} : {token, {wf_whitespace, TokenLine, hd(TokenChars)}}.
%{VAR1} : {token, {wf_var, TokenLine, tl(TokenChars)}}.
%{VAR2} : {token, {wf_var, TokenLine, tl(TokenChars)}}.
{VAR3} : {token, {wf_var, TokenLine, tl(TokenChars)}}.
{CHAR} : {token, {wf_char, TokenLine, hd(TokenChars)}}.

Erlang code.

-define(W, "[a-zA-Z0-9_]").
-define(WDASH, "[a-zA-Z0-9_-]").
-define(S, "[\s\t\r\n]").
-define(S_STAR, ?S "*").
-define(S_PLUS, ?S "+").
-define(NS, "[^\s\t\r\n]").
-define(NS_STAR, "[^\s\t\r\n]*").
-define(SELF_CLOSE, "/").
-define(CAP_SELF_CLOSE, "(/)?").

-export([
    test/0,
    html/0
]).

open_tag(Line, Chars) ->
	NoAttr = "<#" ?S_STAR "(" ?W ?NS_STAR ")" ?S_STAR ?CAP_SELF_CLOSE ?S_STAR ">",
    NoAttrFun = fun([TagName, SelfClose]) ->
        Tag = open_or_self_close(SelfClose),
        {Tag, Line, {TagName, []}}
    end,

	WithAttr = "<#" ?S_STAR "(" ?W ?NS_STAR ")" ?S_PLUS "(.*)" ?CAP_SELF_CLOSE ?S_STAR ">",
    WithAttrFun = fun
        ([TagName, Attrs | SelfClose]) ->
            Tag = open_or_self_close(SelfClose),
            AttrTokens = attribute_tokens(Attrs, Line),
            {Tag, Line, {TagName, AttrTokens}}
    end,

    MatchRules = [
        {NoAttr, NoAttrFun},
        {WithAttr, WithAttrFun}
    ],
	
	try_match(Chars, Line, MatchRules).

open_or_self_close([]) ->
    wf_open_tag;
open_or_self_close(_) ->
    wf_self_close_tag.

close_tag(Line, Chars) ->
    MainRE = "</#(" ?WDASH "+)>",
    MainFun = fun([TagName]) ->
        {wf_close_tag, Line, TagName}
    end,
    UniversalRE = "</#>",
    UniversalFun = fun([]) ->
        {wf_universal_close_tag, Line}
    end,
    MatchRules = [
        {MainRE, MainFun},
        {UniversalRE, UniversalFun}
    ],
    try_match(Chars, Line, MatchRules).
	%io:format("~p, ~p~n",[Line, Chars]),
	%{open_tag, Line, Chars}.

attribute_tokens(AttrsRaw, Line) ->
    %io:format("Raw: ~p~n",[AttrsRaw]),
    {ok, Tokens, _} = wf_template_attr_scan:string(AttrsRaw, Line),
    %io:format("Attr Tokens: ~p~n",[AttrsRaw]),
    {ok, FormattedAttrs} = wf_template_attr:parse(Tokens),
    %io:format("Parsed Tokens: ~p~n",[AttrsRaw]),
    FormattedAttrs.

try_match(Chars, _Line, []) ->
    {string, _Line, Chars};
try_match(Chars, Line, [{RE, Fun} | T]) ->
    case re:run(Chars, RE, [unicode, {capture, all_but_first, list}]) of
        {match, Matches} ->
            Fun(Matches);
        nomatch ->
            try_match(Chars, Line, T)
    end.


test() ->
    ?MODULE:string(html()).

html() ->
    "<html>
        <body>
            <#panel id=whatever>
                PrintVar: @Var
                <#span id=something_else class=@Class>
                </#>
            </#panel>
            <#br />
            <a href='http://google.com'>Link</a>
            <!-- Hereis a comment -->
            <b>some</b>
        </body>
    </html>".

    
