% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_context).
-include("wf.hrl").

-export([
        bridge/0,
        bridge/1,

        in_request/0,
        socket/0,

        path/0,
        protocol/0,
        uri/0,
        url/0,

        peer_ip/0,
        peer_ip/1,
        peer_ip/2,

        request_method/0,
        request_body/0,
        status_code/0,
        status_code/1,

        content_type/1,
        content_type/0,
        encoding/0,
        encoding/1,
        download_as/1,
        headers/0,
        header/1,
        header/2,

        anchor/1,
        anchor/0,

        data/0,
        data/1,
        clear_data/0,

        add_action/2,
        actions/0,
        next_action/0,
        action_queue/0,
        action_queue/1,
        clear_action_queue/0,
        new_action_queue/0,

        page_context/0,
        page_context/1,
       
        entry_point/0,
        entry_point/1,

        series_id/0,
        series_id/1,

        page_module/0,
        page_module/1,

        path_info/0,
        path_info/1,

        async_mode/0,
        async_mode/1,

        event_context/0,
        event_context/1,

        type/0,
        type/1,

        event_module/0,
        event_module/1,

        event_tag/0,
        event_tag/1,

        event_validation_group/0,
        event_validation_group/1,
        event_handle_invalid/0,
        event_handle_invalid/1,

        handlers/0,
        handler/1,
        restore_handler/1,
        set_handler/1,

        init_context/1,
        make_handler/2,

        context/0,
        context/1
    ]).

-export([increment/1]).

%% Exports for backwards compatibility
-export([
        request_bridge/0,
        request_bridge/1,
        response_bridge/0,
        response_bridge/1
    ]).

-define(BRIDGE, (bridge())).

%%% REQUEST AND RESPONSE BRIDGE %%%

bridge() ->
    Context=context(),
    Context#context.bridge.

bridge(Bridge) ->
    Context = context(),
    context(Context#context{bridge=Bridge}).

in_request() ->
    %% If we have a context set and it is a #context{} tuple, then we are in a
    %% request.
    is_record(context, context()).

 socket() ->
    ?BRIDGE:socket().

path() ->
    Req = request_bridge(),
    Req:path().

protocol() ->
    Req = request_bridge(),
    Req:protocol().

uri() ->
    Req = request_bridge(),
    Req:uri().

url() ->
    Protocol = wf:to_list(protocol()),
    Host = header(host),
    Uri = uri(),
    Protocol ++ "://" ++ Host ++ Uri.

peer_ip() ->
    ?BRIDGE:peer_ip().

peer_ip(Proxies) ->
    peer_ip(Proxies,x_forwarded_for).

peer_ip(Proxies,ForwardedHeader) ->
    ConnIP = peer_ip(),
    case header(ForwardedHeader) of
        undefined -> ConnIP;
        RawForwardedIP ->
            ForwardedIP = wf_convert:parse_ip(RawForwardedIP),
            DoesIPMatch = fun(Proxy) ->
                wf_convert:parse_ip(Proxy) =:= ConnIP
            end,
            case lists:any(DoesIPMatch,Proxies) of
                true -> ForwardedIP;
                false -> ConnIP
            end
    end.

request_method() ->
    case ?BRIDGE:request_method() of
        'GET'       -> get;
        get         -> get;
        'POST'      -> post;
        post        -> post;
        'DELETE'    -> delete;
        delete      -> delete;
        'PUT'       -> put;
        put         -> put;
        'TRACE'     -> trace;
        trace       -> trace;
        'HEAD'      -> head;
        head        -> head;
        'CONNECT'   -> connect;
        connect     -> connect;
        'OPTIONS'   -> options;
        options     -> options;
        Other -> list_to_existing_atom(string:to_lower(wf:to_list(Other)))
    end.

request_body() ->
    ?BRIDGE:request_body().

status_code() ->
    ?BRIDGE:status_code().

status_code(StatusCode) ->
    bridge(?BRIDGE:set_status_code(StatusCode)),
    ok.

content_type(ContentType) ->
    header("Content-Type", ContentType).

content_type() ->
    case ?BRIDGE:get_response_header("Content-Type") of
        undefined -> "text/html";
        ContentType -> ContentType
    end.

download_as(Filename0) ->
    Filename = wf_convert:url_encode(Filename0),
    header("Content-Disposition", "attachment; filename=\"" ++ Filename ++ "\"").

headers() ->
    ?BRIDGE:headers().

header(Header) ->
    ?BRIDGE:header(Header).

header(Header, Value) ->
    bridge(?BRIDGE:set_header(Header, Value)),
    ok.

-spec encoding(Encoding :: encoding()) -> ok.
encoding(Encoding) ->
    Context = context(),
    context(Context#context { encoding=Encoding }).

encoding() ->
    Context = context(),
    Context#context.encoding.


%%% TRANSIENT CONTEXT %%%

anchor(Anchor) ->
    Context = context(),
    context(Context#context { anchor=Anchor }).

anchor() ->
    Context = context(),
    Context#context.anchor.

data() ->
    Context = context(),
    Context#context.data.

data(Data) ->
    Context = context(),
    context(Context#context { data = Data }).

clear_data() ->
    Context = context(),
    context(Context#context { data = [] }).

-spec add_action(Priority :: wire_priority(), Action :: actions()) -> ok.
add_action(Priority, Action) when ?IS_ACTION_PRIORITY(Priority) ->
    ActionQueue = action_queue(),
    NewActionQueue = wf_action_queue:in(Priority, Action, ActionQueue),
    action_queue(NewActionQueue).

actions() ->
    ActionQueue = action_queue(),
    Actions = wf_action_queue:all(ActionQueue),
    clear_action_queue(),
    Actions.

-spec next_action() -> {ok, actions()} | empty.
next_action() ->
	ActionQueue = action_queue(),
	case wf_action_queue:out(ActionQueue) of
		{ok, Action, NewActionQueue} ->
            action_queue(NewActionQueue),
            {ok, Action};
		{error, empty} ->
            empty
	end.

action_queue() ->
	Context = context(),
	Context#context.action_queue.

action_queue(ActionQueue) ->
    Context = context(),
    context(Context#context { action_queue = ActionQueue }).

clear_action_queue() ->
    action_queue(new_action_queue()).

new_action_queue() ->
    wf_action_queue:new().

%%% PAGE CONTEXT %%%

page_context() ->
    Context = context(),
    Context#context.page_context.

page_context(PageContext) ->
    Context = context(),
    context(Context#context { page_context = PageContext }).

series_id() ->
    Page = page_context(),
    Page#page_context.series_id.

series_id(SeriesID) ->
    Page = page_context(),
    page_context(Page#page_context { series_id = SeriesID }).

page_module() -> 
    Page = page_context(),
    Page#page_context.module.

page_module(Module) ->
    Page = page_context(),
     page_context(Page#page_context { module = Module }).

entry_point() ->
    Page = page_context(),
    Page#page_context.entry_point.

entry_point(EntryPoint) ->
    Page = page_context(),
    page_context(Page#page_context { entry_point = EntryPoint}).

path_info() -> 
    Page = page_context(),
    Page#page_context.path_info.

path_info(PathInfo) ->
    Page = page_context(),
    page_context(Page#page_context { path_info = PathInfo }).

async_mode() ->
    Page = page_context(),
    Page#page_context.async_mode.

async_mode(AsyncMode) ->
    Page = page_context(),
    page_context(Page#page_context { async_mode=AsyncMode }).


%%% EVENT CONTEXT %%%

event_context() ->
    Context = context(),
    Context#context.event_context.

event_context(EventContext) ->
    Context = context(),
    context(Context#context { event_context = EventContext }).

type() ->
    Context = context(),
    Context#context.type.

type(Type) -> % either first_request, postback_request, postback_websocket, or static_file
    Context = context(),
    context(Context#context { type = Type }).

event_module() ->
    Event = event_context(),
    Event#event_context.module.

event_module(Module) ->
    Event = event_context(),
    event_context(Event#event_context { module = Module }).

event_tag() ->
    Event = event_context(),
    Event#event_context.tag.

event_tag(Tag) ->
    Event = event_context(),
    event_context(Event#event_context { tag = Tag }).

event_validation_group() ->
    Event = event_context(),
    Event#event_context.validation_group.

event_validation_group(ValidationGroup) ->
    Event = event_context(),
    event_context(Event#event_context { validation_group = ValidationGroup }).

event_handle_invalid() ->
    Event = event_context(),
    Event#event_context.handle_invalid.

event_handle_invalid(HandleInvalid) ->
    Event = event_context(),
    event_context(Event#event_context { handle_invalid = HandleInvalid }).

%%% HANDLERS %%%

handlers() ->
    C = context(),
    [
        C#context.config_handler,
        C#context.log_handler,
        C#context.process_registry_handler,
        C#context.cache_handler,
        C#context.query_handler,
        C#context.crash_handler,
        C#context.session_handler,
        C#context.state_handler,
        C#context.identity_handler,
        C#context.role_handler,
        C#context.route_handler,
        C#context.security_handler
    ].

handler(HandlerName) ->
    get_handler(context(), HandlerName).

set_handler(NewHandler) ->
    NewContext = set_handler(context(), NewHandler),
    context(NewContext).

restore_handler(NewHandler) ->
    set_handler(NewHandler).

get_handler(#context{config_handler=H}, config_handler) -> H;
get_handler(#context{log_handler=H}, log_handler) -> H;
get_handler(#context{process_registry_handler=H}, process_registry_handler) -> H;
get_handler(#context{cache_handler=H}, cache_handler) -> H;
get_handler(#context{query_handler=H}, query_handler) -> H;
get_handler(#context{crash_handler=H}, crash_handler) -> H;
get_handler(#context{session_handler=H}, session_handler) -> H;
get_handler(#context{state_handler=H}, state_handler) -> H;
get_handler(#context{identity_handler=H}, identity_handler) -> H;
get_handler(#context{role_handler=H}, role_handler) -> H;
get_handler(#context{route_handler=H}, route_handler) -> H;
get_handler(#context{security_handler=H}, security_handler) -> H;
get_handler(C, Handler) ->
    throw({unknown_handle, Handler}).

set_handler(C,H = #handler_context{name=config_handler}) -> C#context{config_handler=H};
set_handler(C,H = #handler_context{name=log_handler}) -> C#context{log_handler=H};
set_handler(C,H = #handler_context{name=process_registry_handler}) -> C#context{process_registry_handler=H};
set_handler(C,H = #handler_context{name=cache_handler}) -> C#context{cache_handler=H};
set_handler(C,H = #handler_context{name=query_handler}) -> C#context{query_handler=H};
set_handler(C,H = #handler_context{name=crash_handler}) -> C#context{crash_handler=H};
set_handler(C,H = #handler_context{name=session_handler}) -> C#context{session_handler=H};
set_handler(C,H = #handler_context{name=state_handler}) -> C#context{state_handler=H};
set_handler(C,H = #handler_context{name=identity_handler}) -> C#context{identity_handler=H};
set_handler(C,H = #handler_context{name=role_handler}) -> C#context{role_handler=H};
set_handler(C,H = #handler_context{name=route_handler}) -> C#context{route_handler=H};
set_handler(C,H = #handler_context{name=security_handler}) -> C#context{security_handler=H}.

%% MAYBE DO THIS?
%%serializable_handlers() ->
%%    [H || H <- handlers(), is_handler_serializable(H)].
%%
%%is_handler_serializable(#handler_context{module=Module}) ->
%%    case erlang:function_exported(Module, is_serializable, 0) of
%%        true -> Module:is_serializable();
%%        false -> true
%%    end.

%%% CONTEXT CONSTRUCTION %%%

init_context(Bridge) ->
    % Create the new context using the default handlers.
    Context = #context {
        bridge = Bridge,
        page_context = #page_context { series_id = wf:temp_id() },
        event_context = #event_context {},
        action_queue = new_action_queue()
    },
    context(Context).

make_handler(Name, Module) -> 
    #handler_context { 
        name=Name,
        module=Module,
        state=[]
    }.


%%% GET AND SET CONTEXT %%%
% Yes, the context is stored in the process dictionary. It makes the Nitrogen 
% code much cleaner. Trust me.
context() -> get(context).
context(Context) -> put(context, Context).

%% for debugging. Remove when ready
increment(Key) ->
    case get(Key) of
        undefined -> put(Key, 1);
        V -> io:format("~p=~p~n",[Key, V+1]), put(Key, V+1)
    end.

%% Kept for backwards compatibility with nitrogen 2.2 and below (and
%% simple_bridge 1.x)
request_bridge() ->
    bridge().

request_bridge(Bridge) ->
    bridge(Bridge).

response_bridge() ->
    bridge().

response_bridge(Bridge) ->
    bridge(Bridge).

