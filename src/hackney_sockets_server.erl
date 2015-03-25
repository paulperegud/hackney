%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_sockets_server).
-behaviour(gen_server).


%% socket pool api
-export([set_socket_pool/2, get_socket_pool/1,
         incr_active_pool/1, decr_active_pool/1,
         reset_pool_count/1,
         init_pool/2,
         set_pool_size/2,
         get_pool_size/1,
         with_pool/2,
         pool_info/1]).

-export([max_to_int/1, int_to_max/1]).


%% gen server api
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).


-include("hackney_socket.hrl").

-define(SERVER, ?MODULE).

-record(state, {monitors = []}).

-define(MAX_POOL_POS, 2).
-define(ACTIVE_POOL_POS, 3).


set_socket_pool(PoolName, Pid) ->
    true = gen_server:call(?MODULE, {set_sockets_pool, PoolName, Pid}),
    ok.

get_socket_pool(PoolName) ->
   ets:lookup_element(?SOCKETS_SERVER, {pool, PoolName}, 2).

get_socket_pool1(PoolName) ->
    try
        get_socket_pool(PoolName)
    catch
        error:badarg -> unknown_pool
    end.

max_to_int(unlimited)   -> -1;
max_to_int(Max) -> Max.

int_to_max(-1) -> unlimited;
int_to_max(Max) -> Max.


incr_active_pool(PoolName) ->
    ets:update_counter(?SOCKETS_SERVER, {c, PoolName}, {?ACTIVE_POOL_POS, 1}).

decr_active_pool(PoolName) ->
    ets:update_counter(?SOCKETS_SERVER, {c, PoolName}, {?ACTIVE_POOL_POS, -1}).

reset_pool_count(PoolName) ->
    ets:update_counter(?SOCKETS_SERVER, {c, PoolName}, {?MAX_POOL_POS, 0}).

init_pool(PoolName, PoolSize) ->
    ets:insert_new(?SOCKETS_SERVER, {{c, PoolName}, max_to_int(PoolSize), 0}).

set_pool_size(PoolName, PoolSize) ->
    ets:update_element(?SOCKETS_SERVER, {c, PoolName},
                       {?MAX_POOL_POS, max_to_int(PoolSize)}).

get_pool_size(PoolName) ->
    ets:update_counter(?SOCKETS_SERVER, {c, PoolName}, {?MAX_POOL_POS, 0}).

with_pool(PoolName, Fun) ->
    case get_socket_pool1(PoolName) of
        Pid when is_pid(Pid) -> Fun(Pid);
        Error -> Error
    end.

pool_info(PoolName) ->
    case get_socket_pool1(PoolName) of
        Pid when is_pid(Pid) ->
            Stats = ets:update_counter(?SOCKETS_SERVER, {c, PoolName},
                                       [{?MAX_POOL_POS, 0},
                                        {?ACTIVE_POOL_POS, 0}]),
            [PoolSize, PoolCount] = Stats,

            [{name, PoolName},
             {pid, Pid},
             {pool_size, int_to_max(PoolSize)},
             {pool_len, PoolCount}];
        Error ->
            Error
    end.

start_link() ->
    _ = create_tabs(),
    SpawnOpts = hackney_util:valid_opts(sockets_server_options, []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
                          [{spawn_opt, SpawnOpts}]).


create_tabs() ->
    Opts = hackney_util:valid_opts(sockets_ets_options,
                                   [{write_concurrency,true},
                                    {read_concurrency, true}]),

    case ets:info(?SOCKETS_SERVER, name) of
        undefined ->
            ets:new(?SOCKETS_SERVER, [ordered_set, public,
                                      named_table | Opts]);
        _ ->
            ok
    end.

init([]) ->
    Monitors = set_monitors(),
    {ok, #state{monitors=Monitors}}.

handle_call({set_sockets_pool, Name, Pid}, _From, State) ->
    case ets:insert_new(?SOCKETS_SERVER, {{pool, Name}, Pid}) of
        true ->
            %% monitor the pool
            MRef = erlang:monitor(process, Pid),
            Monitors = [{{MRef, Pid}, {pool, Name}} | State#state.monitors],
            {reply, true, State#state{monitors=Monitors}};
        false ->
            {reply, false, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, bad_call, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', MRef, process, Pid, _}, #state{monitors=Monitors}=State) ->
    {_, Ref} = lists:keyfind({MRef, Pid}, 1, Monitors),
    {pool, PoolName} = Ref,
    true = ets:delete(?SOCKETS_SERVER, Ref),
    true = ets:delete(?SOCKETS_SERVER, {c, PoolName}),
    Monitors2 = lists:keydelete({MRef, Pid}, 1, Monitors),
    {noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


set_monitors() ->
    [{{erlang:monitor(process, Pid), Pid}, {pool, Ref}} ||
     [Ref, Pid] <- ets:match(?SOCKETS_SERVER, {{pool, '$1'},'$2'})].
