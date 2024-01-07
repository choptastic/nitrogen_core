Definitions.

DQ_STRING = \"[^\"]*\"
SQ_STRING = '[^']*'
QL_STRING = [^\s='\"\t\r\n]+
%SAFE_STRING = [a-zA-Z0-9_-]+
WHITESPACE = [\s\t\r\n]+
EQUAL = =

Rules.

{DQ_STRING} : {token, {string, TokenLine, string:trim(TokenChars, both, "\"")}}.
{SQ_STRING} : {token, {string, TokenLine, string:trim(TokenChars, both, "'")}}.
{QL_STRING} : {token, {quoteless_string, TokenLine, string:trim(TokenChars, both)}}.
%{SAFE_STRING} : {token, {safe_string, TokenLine, string:trim(TokenChars, both)}}.
%{WHITESPACE} : {token, {whitespace, TokenLine, TokenChars}}.
{WHITESPACE} : skip_token.
{EQUAL} : {token, {'=', TokenLine}}.

Erlang code.

-export([test/0, redo/0]).
test() ->
    Attrs = "a=b fd-f=\"abc 123\" something_else='some other string' href=\"http://google.com\"",
    ?MODULE:string(Attrs).

redo() ->
    leex:file(?FILE, [verbose]).
