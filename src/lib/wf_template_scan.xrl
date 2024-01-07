Definitions.

START_TAG = \<\s*[^\/][^\>]*\>
END_TAG = \<\/\s*[^\>]*\>
COMMENT = \<\!--.*?--\>
WHITESPACE = [\r\n\t\n\s]
CHAR = .

Rules.

{START_TAG} : {token, open_tag(TokenLine, TokenChars)}.
{END_TAG} : {token, close_tag(TokenLine, TokenChars)}.
{COMMENT} : {token, {wf_comment, TokenLine, TokenChars}}.
{CHAR} : {token, {wf_char, TokenLine, hd(TokenChars)}}.
{WHITESPACE} : {token, {wf_whitespace, TokenLine, hd(TokenChars)}}.

Erlang code.

-define(W, "[a-zA-Z0-9_]").
-define(WDASH, "[a-zA-Z0-9_-]").
-define(S, "[\s\t\r\n]").
-define(NS, "[^\s\t\r\n]").

-export([
    test/0,
    html/0
]).

open_tag(Line, Chars) ->
	NoAttr = "<" ?S "*(" ?W ?NS "*)" ?S "*>",
    NoAttrFun = fun([TagName]) ->
        {wf_open_tag, Line, {TagName, []}}
    end,

	WithAttr = "<" ?S "*(" ?W ?NS "*)" ?S "+(.*)>",
    WithAttrFun = fun([TagName, Attrs]) ->
        AttrTokens = attribute_tokens(Attrs, Line),
        {wf_open_tag, Line, {TagName, AttrTokens}}
    end,
    MatchRules = [
        {NoAttr, NoAttrFun},
        {WithAttr, WithAttrFun}
    ],
	
	try_match(Chars, Line, MatchRules).

close_tag(Line, Chars) ->
    RE = "</(" ?WDASH "+)>",
    Fun = fun([TagName]) ->
        {wf_close_tag, Line, TagName}
    end,
    MatchRules = [
        {RE, Fun}
    ],
    try_match(Chars, Line, MatchRules).
	%io:format("~p, ~p~n",[Line, Chars]),
	%{open_tag, Line, Chars}.

attribute_tokens(AttrsRaw, Line) ->
    io:format("Raw: ~p~n",[AttrsRaw]),
    {ok, Tokens, _} = wf_template_attr_scan:string(AttrsRaw, Line),
    io:format("Attr Tokens: ~p~n",[AttrsRaw]),
    {ok, FormattedAttrs} = wf_template_attr:parse(Tokens),
    io:format("Parsed Tokens: ~p~n",[AttrsRaw]),
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
            <a href='http://google.com'>Link</a>
            <!-- Hereis a comment -->
            <b>some bold</b>
        </body>
    </html>".

    
