%% vim: ft=erlang
Nonterminals
attrs attr any_string
compound_val
.

Terminals
string quoteless_string '=' wf_var
.

Rootsymbol attrs.

attrs -> attr
         : ['$1'].
attrs -> attr attrs
         : ['$1' | '$2'].

attr -> quoteless_string '=' any_string
        : {wf_attr, ?anno('$1'), {val('$1'), val('$3')}}.
attr -> quoteless_string
        : {wf_attr, ?anno('$1'), {val('$1'), true}}.
attr -> quoteless_string '=' wf_var
        : {wf_attr, ?anno('$1'), {val('$1'), '$3'}}.
attr -> quoteless_string '=' compound_val
        : {wf_attr, ?anno('$1'), {val('$1'), '$3'}}.

any_string -> string
              : '$1'.
any_string -> quoteless_string
              : '$1'.

compound_val -> any_string wf_var
                : [val('$1'), '$2'].
compound_val -> wf_var any_string
                : ['$1', val('$2')].
compound_val -> wf_var wf_var
                : ['$1', '$2'].
compound_val -> compound_val wf_var
                : ['$1', '$2'].
compound_val -> compound_val any_string
                : ['$1', val('$2')].

Erlang code.

-define(anno(Tup), element(2, Tup)).

%-export([string/1]).
%
%string(X) ->
%    wf_template_attr_scan:string(X).

val({_, _, Val}) ->
    Val.

%safe(Str) ->
%    wf_convert:html_encode(Str).
