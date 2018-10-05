% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_list).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    precompile_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, list).

-spec render_element(#list{}) -> body().
render_element(Record) -> 
    Tag = case Record#list.numbered of 
        true -> ol;
        _ -> ul
    end,

    wf_tags:emit_tag(Tag, Record#list.body, [
        {id, Record#list.html_id},
        {class, [list, Record#list.class]},
        {title, Record#list.title},
        {style, Record#list.style},
        {data_fields, Record#list.data_fields}
    ]).


precompile_element(Form = {record, _, _, Fields}) ->
    Tag = case nitrogen_precompile:attr_n(Fields, numbered) of
        true -> ol;
        _ -> ul
    end,
    wf_element_precompile:default_precompile_element(Form, Tag).
