-module(wf_element_precompile).
-include("wf.hrl").

-export([simple_precompile/1]).
-export([default_precompile_element/2]).
-export([default_precompile_element/4]).
-export([default_precompile_element/5]).

default_precompile_element(Form, Htmltag) ->
    default_precompile_element(Form, Htmltag, [], []).

default_precompile_element(Form, Htmltag, AddAttrMap, DisqFields) ->
    default_precompile_element(Form, Htmltag, AddAttrMap, DisqFields, []).

default_precompile_element(Form = {record, Line, Record, Fields}, Htmltag, AddAttrMap, DisqFields, Opts) ->
    Bodyfield = proplists:get_value(body_field, Opts, body),
    Precomp = #precomp_element{
        form=Form,
        fields=Fields,
        line=Line,
        record=Record,
        htmltag=Htmltag,
        additional_attribute_map=AddAttrMap,
        disqualifying_fields=DisqFields,
        body_field=Bodyfield
    },
    simple_precompile(Precomp).

simple_precompile(Precomp) ->
    #precomp_element{
        form=Form,
        %line=Line,
        fields=Fields,
        record=Record,
        disqualifying_fields=DisqFields,
        body_field=Bodyfield
    } = Precomp,

    %io:format("Precompiling a #~p{} with fields: ~p~n",[Record, Fields]),
    NewForm = case can_we_precompile(DisqFields, Fields, Bodyfield) of
        true ->
            perform_precompile(Precomp);
        false ->
            %io:format("Cannot Precompile ~p with attrs: ~p~n",[Record, Fields]),
            Form
    end,
    NewForm.
    
can_we_precompile(_, [], _) ->
    true;
can_we_precompile(DisqFields, Fields, Bodyfield) ->
    lists:all(fun({record_field, _LN, {atom, _LN2, FieldName}, Value}) ->
        FieldName=/=actions andalso %% cannot be the "actions" field
        not(lists:member(FieldName, DisqFields)) andalso %% and cannot be any fields whose presence immediately disqualifies it
            (
                %% and the field named body or text are both fine to precompile
                FieldName==Bodyfield orelse FieldName==text orelse
                    %% or the field contains only static content
                    nitrogen_precompile:has_only_static_content(Value)
            )
    end, Fields).


perform_precompile(Precomp) ->
    #precomp_element{
        line=Line,
        fields=Fields,
        htmltag=Htmltag,
        additional_attribute_map=AddAttrMap,
        body_field=Bodyfield
    } = Precomp,

    RawAttrs = [
        {id, html_id},
        {title, title},
        {style, style},
        {class, class},
        {data_fields, data_fields}
    ] ++ AddAttrMap,

    FinalAttrs = lists:map(fun({HtmlAttr, Field}) ->
        Value = nitrogen_precompile:attr_n(Fields, Field),
        {HtmlAttr, Value}
    end, RawAttrs),

    ID = nitrogen_precompile:attr_n(Fields, id),
    Text = nitrogen_precompile:attr(Fields, text),
    Body = nitrogen_precompile:attr(Fields, Bodyfield),
    HtmlEncode = case nitrogen_precompile:attr(Fields, html_encode) of
        undefined -> {atom, Line, true};
        X -> X
    end,
    Class = nitrogen_precompile:attr_n(Fields, class),

    nitrogen_precompile:inc(),

    FullBody = text_and_body(Line, Text, Body, HtmlEncode),

    IDClass = wf:normalize_id(ID),
    FinalClass = [Class, IDClass, precompiled],

    {_Open, _, _Close} = TagParts = wf_tags:emit_tag_parts(Htmltag, FullBody, [
        {class, FinalClass}
        | FinalAttrs
    ]),

    %io:format("Pre-rendered Tag Parts:~nOpen: ~p~nClose: ~p~n",[Open, Close]),

    Res = nitrogen_precompile:pre_render_tag(Line, TagParts),
    %io:format("Post-Rendered Panel: ~p~n",[Res]),
    Res.

text_and_body(Line, undefined, undefined, _) ->
    {bin, Line, []}; %% empty binary
text_and_body(Line, Text, undefined, HtmlEncode) ->
    do_text(Line, Text, HtmlEncode);
text_and_body(_Line, undefined, Body, _) ->
    do_body(Body);
text_and_body(Line, Text, Body, HtmlEncode) ->
    {cons, Line, do_text(Line, Text, HtmlEncode),
        {cons, Line, do_body(Body),
            {nil, Line}}}.


do_text(Line, Text, HtmlEncode) ->
    wrap_args_in_call(Line, wf, html_encode, [Text, HtmlEncode]).

do_body(Body) ->
    nitrogen_precompile:do_body_forms(Body).


wrap_args_in_call(Line, Module, Function, Args) ->
    {call, Line,
        {remote, Line, {atom, Line, Module}, {atom, Line, Function}},
        Args
    }.
