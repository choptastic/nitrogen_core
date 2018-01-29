%% vim: ft=nitrogen
-module(wf_stats).
-include("wf.hrl").
-behaviour(gen_server).
-compile({no_auto_import, [get/1]}).

-export([
    start_link/0,
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export([
    inc/1,
    zero/1,
    get_core/1,
    inc_core/1,
    get/1,
    reg_self/1,
    unreg_self/0,
    inspect/1
]).

-define(SRV, {local, ?MODULE}).
-define(REG_TABLE, nitrogen_stats).
-define(COUNTER_TABLE, nitrogen_counter).

-record(state, {}).
-record(pid, {pid, added, monitor, type=first_request}).

core_type_map(first_request) -> nitrogen_core_hits;
core_type_map(postback_request) -> nitrogen_core_postbacks;
core_type_map(static_file) -> nitrogen_core_static_file;
core_type_map(postback_websocket) -> nitrogen_core_websocket.

inc_core(Type) -> 
    CoreType = core_type_map(Type),
    inc(CoreType).

get_core(Type) ->
    CoreType = core_type_map(Type),
    get(CoreType).

inc(Var) ->
    gen_server:cast(?SRV, {inc, Var}).

zero(Var) ->
    gen_server:cast(?SRV, {zero, Var}).

reg_self(Type) ->
    Pid = self(),
    gen_server:cast(?SRV, {reg, Pid, Type}).

unreg_self() ->
    Pid = self(),
    gen_server:cast(?SRV, {unreg, Pid}).

get(Var) ->
    case ets:lookup(?COUNTER_TABLE, Var) of
        {error, _} -> undefined;
        [Val] -> Val
    end.

inspect(Pid) ->
    [].


start_link() ->
    gen_server:start_link(?SRV, ?MODULE, [] ,[]).

init(_) ->
    ?COUNTER_TABLE = ets:new(?COUNTER_TABLE, [
        named_table, {read_concurrency, true}, public, {write_concurrency, true}
    ]),
    ?REG_TABLE = ets:new(?REG_TABLE, [
        named_table, {read_concurrency, true}, public, {write_concurrency, true}, {keypos, #pid.pid}
    ]),
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {noreply, State}.


handle_cast({inc, Var}, State) ->
    ets:update_counter(?COUNTER_TABLE, Var, 1, 0),
    {noreply, State};

handle_cast({zero, Var}, State) ->
    ets:insert(?COUNTER_TABLE, {Var, 0}),
    {noreply, State};

handle_cast({reg, Pid, Type}, State) ->
    Monitor = erlang:monitor(process, Pid),
    ets:insert(?REG_TABLE, #pid{pid=Pid, monitor=Monitor, added=os:timestamp(), type=Type}),
    {noreply, State};

handle_cast({unreg, Pid}, State) ->
    ets:delete(?REG_TABLE, Pid),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
    ets:delete(?REG_TABLE, Pid),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
