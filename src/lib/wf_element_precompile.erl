-module(wf_element_precompile).
-include("wf.hrl").

-export([simple_precompile/1]).
-export([default_precompile_element/2]).
-export([default_precompile_element/4]).

default_precompile_element(Form, Htmltag) ->
    default_precompile_element(Form, Htmltag, [], []).

default_precompile_element(Form = {record, Line, Record, Fields}, Htmltag, AddAttrMap, DisqFields) ->
    Precomp = #precomp_element{
        form=Form,
        fields=Fields,
        line=Line,
        record=Record,
        htmltag=Htmltag,
        additional_attribute_map=AddAttrMap,
        disqualifying_fields=DisqFields
    },
    simple_precompile(Precomp).

simple_precompile(Precomp) ->
    #precomp_element{
        form=Form,
        %line=Line,
        fields=Fields,
        record=Record,
        disqualifying_fields=DisqFields
    } = Precomp,

    io:format("Precompiling a #~p{} with fields: ~p~n",[Record, Fields]),
    NewForm = case can_we_precompile(DisqFields, Fields) of
        true ->
            perform_precompile(Precomp);
        false ->
            Form
    end,
    NewForm.
    
can_we_precompile(_, []) ->
    true;
can_we_precompile(DisqFields, Fields) ->
    lists:all(fun({record_field, _LN, {atom, _LN2, FieldName}, Value}) ->
        FieldName=/=actions andalso %% cannot be the "actions" field
        not(lists:member(FieldName, DisqFields)) andalso %% and cannot be any fields whose presence immediately disqualifies it
            (
                %% and the field named body or text are both fine to precompile
                FieldName==body orelse
                    %% or the field contains only static content
                    nitrogen_precompile:has_only_static_content(Value)
            )
    end, Fields).


perform_precompile(Precomp) ->
    #precomp_element{
        line=Line,
        fields=Fields,
        htmltag=Htmltag,
        additional_attribute_map=AddAttrMap
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
    Text = nitrogen_precompile:attr_n(Fields, text),
    Body = nitrogen_precompile:attr(Fields, body),
    HtmlEncode = nitrogen_precompile:attr_n(Fields, html_encode),
    Class = nitrogen_precompile:attr_n(Fields, class),

    FullBody = {cons, Line, 
        erl_parse:abstract(wf:html_encode(Text, HtmlEncode)),
        case Body of
            undefined -> {nil, Line};
            _ -> nitrogen_precompile:do_body_forms(Body)
        end 
    },

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
