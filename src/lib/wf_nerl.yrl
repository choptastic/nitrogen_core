%% vim: ft=erlang
Nonterminals
string
nerl_code
page
body
.

Terminals
char '#[' ']#'
start_file end_file
.

Right 0 char.

Rootsymbol page.

page -> start_file body end_file
        : lists:flatten('$2').

body -> string
        : destring('$1').
body -> nerl_code
        : '$1'.
body -> string body
        : [destring('$1'), '$2'].
body -> nerl_code body
        : [destring('$1'), '$2'].


nerl_code -> '#[' string ']#'
             : process_code('$2').

string -> char
          : stringify('$1').
string -> char string
          : stringify('$1', '$2').

Erlang code.

-export([
    test/0,
    file/1,
    file/2,
    compile/1,
    redo/0
]).

-define(anno(Tup), element(2, Tup)).
-define(EXT, ".nerl").

destring({string, _, String}) ->
    String;
destring(String) when is_list(String) ->
    String.

stringify({char, Anno, Char}) ->
    {string, Anno, [Char]}.

stringify({char, Anno, Char}, {string, _Anno, String}) ->
    NewStr = [Char | String],
    %io:format("Building String: ~ts~n",[NewStr]),
    {string, Anno, NewStr};

stringify({string, Anno, String}, {char, _Anno, Char}) ->
    NewStr = String ++ [Char],
    %io:format("Building String: ~ts~n",[NewStr]),
    {string, Anno, NewStr};

stringify({string, Anno, Str1}, {string, _Anno, Str2}) ->
    {string, Anno, Str1 ++ Str2}.

val({string, _, Val}) ->
    Val.

process_code({string, Anno, String}) ->
    %io:format("Processing NTML: ~s~n", [String]),
    Res = wf_template:string(String, Anno),
    Res2 = lists:flatten(Res),
    {string, Anno, Res2}.

compile(Filename) ->
    {ok, NewFile} = file(Filename),
    compile:file(NewFile).

file(Filename) ->
    file(Filename, []).

file(Filename, _Options) ->
    NewFilename = new_filename(Filename),
    {ok, Tokens} = wf_nerl_scan:file(Filename),
    %file:write_file("/tmp/tokens.erl", io_lib:format("~p",[Tokens])),
    {ok, Code} = parse(Tokens),
    ok = file:write_file(NewFilename, Code),
    {ok, NewFilename}.

new_filename(Filename) ->
    check_ext(Filename),
    Rootname = filename:rootname(Filename),
    _NewFilename = Rootname ++ ".erl".

check_ext(Filename) ->
    case string:to_lower(filename:extension(Filename)) of
        ?EXT ->
            ok;
        Ext ->
            logger:warning("Nerl Warning: ~s has the extension ~s instead of ~s.", [Filename, Ext, ?EXT])
    end.

redo() ->
    yecc:file(?FILE, [verbose, {parserfile, "/tmp/wf_nerl.yrl"}]).

test() ->
    File = "src/test.nerl",
    file(File).


%% try testing - still needs to save .nerl files to .erl files
