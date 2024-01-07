Nonterminals
attrs attr any_string
.

Terminals
string quoteless_string '='
.

Rootsymbol attrs.

attrs -> attr
         : ['$1'].
attrs -> attr attrs
         : ['$1' |'$2'].

attr -> quoteless_string '=' any_string
        : {wf_attr, ?anno('$1'), val('$1') ++ "=\"" ++ safe(val('$3')) ++ "\""}.
attr -> quoteless_string
        : {wf_attr, ?anno('$1'), val('$1')}.

any_string -> string
              : '$1'.
any_string -> quoteless_string
              : '$1'.

Erlang code.

-define(anno(Tup), element(2, Tup)).

-export([string/1]).

string(X) ->
    wf_template_attr_scan:string(X).

val({_, _, Val}) ->
    Val.

safe(Str) ->
    wf_convert:html_encode(Str).
