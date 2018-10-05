-module(nitrogen_precompile).
-include("wf.hrl").


-export([parse_transform/2]).

-export([
    has_only_static_content/1,
    attr/2,
    attr_n/2,
    pre_render_tag/2,
    do_body_forms/1
]).

parse_transform(Forms, _Options) ->
    %io:format("Forms: ~p",[Forms]),
    file:write_file("precompile_forms", io_lib:format("~p",[Forms])),
    ElementModules = precache_element_modules(Forms),
    erlang:put(element_modules, ElementModules),

    %io:format("ModuleMap: ~p~n",[dict:to_list(ElementModules)]),
    NewForms = do_functions(Forms),
    %io:format("Final Rendered Forms: ~p~n",[NewForms]),
    file:write_file("precompiled_page", io_lib:format("~p", [NewForms])),
    NewForms.

precache_element_modules(Forms) ->
    lists:foldl(fun(Form, Acc) ->
        case precache_module(Form) of
            none -> Acc;
            {ok, Element, Module} ->
                dict:store(Element, Module, Acc)
        end
    end, dict:new(), Forms).

precache_module({attribute, _LN, record, {Record, Fields}}) ->
    case search_fields_for_module(Fields) of
        {ok, Module} -> {ok, Record, Module};
        none -> none
    end;
precache_module(_) ->
    none.

search_fields_for_module([{typed_record_field, {record_field, _, {atom, _, module}, {atom, _, Module}}, _} | _Rest]) ->
    {ok, Module};
search_fields_for_module([_H|T]) ->
    search_fields_for_module(T);
search_fields_for_module([]) ->
    none.


do_functions(Forms) ->
    [do_function(Form) || Form <- Forms].

do_function({function, Line, Function, Arity, Clauses}) ->
    {function, Line, Function, Arity, do_function_clauses(Clauses)};
do_function(Form) ->
    Form.
    
do_function_clauses(Clauses) ->
    [do_function_clause(Clause) || Clause <- Clauses].

do_function_clause({clause, Line, Arg1, Arg2, Body}) ->
    {clause, Line, Arg1, Arg2, do_body_forms(Body)}.

do_body_forms(Forms) when is_list(Forms) ->
    [do_body_form(F) || F <- Forms];
do_body_forms(Form) when is_tuple(Form) ->
    do_body_form(Form).

do_body_form(Form = {record, _Line, RecordName, _Fields}) ->
    maybe_dispatch_to_module(RecordName, Form);
do_body_form(_Form = {cons, Line, Head, Tail}) ->
    {cons, Line, do_body_form(Head), do_body_form(Tail)};
do_body_form(Form) ->
    io:format("No Clause for: ~p~n",[Form]),
    Form.

get_element_module(RecordName) ->
    dict:find(RecordName, erlang:get(element_modules)).


maybe_dispatch_to_module(RecordName, Form) ->
    case get_element_module(RecordName) of
        error ->
            io:format("~p is not an element module~n",[RecordName]),
            Form;
        {ok, Module} ->
            code:ensure_loaded(Module),
            case erlang:function_exported(Module, precompile_element, 1) of
                true ->
                    io:format("Precompiling ~p~n",[Module]),
                    Module:precompile_element(Form);
                false ->
                    io:format("~p has no precompile_element/1~n",[RecordName]),
                    Form
            end
    end.


has_only_static_content({string, _, _}) ->
    true;
has_only_static_content({bin, _, BinElements}) ->
    has_only_static_binaries(BinElements);
has_only_static_content({atom, _, _}) ->
    true;
has_only_static_content({cons, _, Head, Tail}) ->
    has_only_static_content(Head) andalso has_only_static_content(Tail);
has_only_static_content(_) ->
    false.

has_only_static_binaries([{bin_element, _, {string, _, _}} | Rest]) ->
    has_only_static_binaries(Rest);
has_only_static_binaries([{bin_element, _, {integer, _, _}} | Rest]) ->
    has_only_static_binaries(Rest);
has_only_static_binaries([_Other|_Rest]) ->
    false;
has_only_static_binaries([]) ->
    true.

attr({record, _, _Record, Fields}, Field) ->
    %% TODO: This should look up default values based on record
    attr(Fields, Field);
attr([{record_field, _, {atom, _, Field}, Value} | _Fields], Field) ->
    Value;
attr([_ | Rest], Field) ->
    attr(Rest, Field);
attr([], _) ->
    undefined.

attr_n(Fields, Field) ->
    case attr(Fields, Field) of
        undefined -> undefined;
        V -> erl_parse:normalise(V) %% Silly British spelling of normalize
    end.

%attrs(Record, ProvidedFields, DesiredFields) ->

pre_render_tag(Line, {Tag, <<>>, <<>>}) ->
    erl_parse:abstract(Tag, [{line, Line}]);
pre_render_tag(Line, {Open, Content, Close}) ->
    AbsOpen = erl_parse:abstract(Open, [{line, Line}]),
    AbsClose = erl_parse:abstract(Close, [{line, Line}]),
    {cons, Line, AbsOpen, 
        {cons, Line, Content,
            {cons, Line, AbsClose, {nil, Line}}}}.
