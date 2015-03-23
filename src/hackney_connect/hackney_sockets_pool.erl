%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_sockets_pool).
-behaviour(gen_server).

%% public API
-export([start_pool/1, start_pool/2,
         checkout/5,
         checkin/2,
         stop/1]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-include("hackney_socket.hrl").

-record(state, {name,
                ref_tab,
                sock_tab,
                timeout}).

-define(REF_TAB, hackney_sockets_pool_ref).
-define(SOCK_TAB, hackney_sockets_pool_sock).
-define(RETRY_MS, 200).
-define(DEFAULT_POOL_SIZE, 200).
-define(DEFAULT_POOL_TIMEOUT, 300000).

start_pool(PoolName) ->
    start_pool(PoolName, []).

start_pool(PoolName, Opts) ->
    supervisor:start_child(hackney_sockets_pool_sup, [PoolName, Opts]).

with_pool(PoolName, Fun) ->
    hackney_sockets_server:with_pool(PoolName, Fun).

checkout(PoolName, Transport, Host, Port, Timeout) ->
    with_pool(PoolName, fun(Pid) ->
                                checkout1(Pid, Transport, Host, Port, Timeout)
                        end).

checkout1(Pid, Transport, Host, Port, Timeout) ->
    case inet:getaddrs(Host, inet, Timeout) of
        {ok, IPs} ->
            case gen_server:call(Pid, {checkout, Transport, IPs, Port,
                                       self()}) of
                {ok, HS} ->
                    {ok, HS};
                {error, _} ->
                    connect(IPs, Port, Transport, [{active, false}], Timeout,
                            undefined)
            end;
        Error ->
            Error
    end.

checkin(PoolName, HS) ->
    with_pool(PoolName, fun(Pid) ->
                                checkin1(Pid, PoolName, HS)
                        end).

checkin1(Pid, PoolName, HS) ->
    hackney_socket:setopts(HS, [{active, false}]),
    case sync_socket(HS) of
        true ->
            Max = hackney_sockets_server:reset_pool_count(PoolName),
            case {Max, hackney_sockets_server:incr_active_pool(PoolName)} of
                {Unlimited, BelowMax} when Unlimited =:= -1; BelowMax =< Max ->
                    hackney_socket:controlling_process(HS, Pid),
                    case gen_server:call(Pid, {checkin, HS}) of
                        ok ->
                            ok;
                        Error ->
                            _ = hackney_sockets_server:decr_active_pool(PoolName),
                            hackney_sock:close(HS),
                            Error
                    end;
                _ ->
                    %% no need to check we already have the maximum of
                    %% connections
                    _ = hackney_sockets_server:decr_active_pool(PoolName),
                    hackney_socket:close(HS),
                    ok
            end;
        false ->
            hackney_socket:close(HS),
            ok
    end.

stop(PoolName) ->
    with_pool(PoolName, fun(Pid) ->
                                gen_server:call(Pid, stop)
                        end).


start_link(Name, Opts) ->
    gen_server:start_link(?MODULE, [Name, Opts], []).


init([Name, Opts]) ->
    RefTab = ets:new(?REF_TAB, [bag]),
    SockTab = ets:new(?SOCK_TAB, [set]),

    Timeout = hackney_util:user_opts(pool_timeout, Opts, ?DEFAULT_POOL_TIMEOUT),

    %% set defaults settings
    PoolSize = hackney_util:user_opts(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    hackney_sockets_server:init_pool(Name, PoolSize),

    %% register the pool in the sockets server
    hackney_sockets_server:set_socket_pool(Name, self()),

    {ok, #state{name=Name, ref_tab=RefTab, sock_tab=SockTab, timeout=Timeout}}.

handle_call({checkout, Transport, IPs, Port, Pid}, _From, State) ->
    case do_checkout(IPs, Port, Transport, State) of
        {ok, HS} ->
            _ = hackney_sockets_server:decr_active_pool(State#state.name),
            hackney_socket:controlling_process(HS, Pid),
            {reply, {ok, HS}, State};
        Error ->
            {reply, Error, State}
    end;
handle_call({checkin, HS}, _From, State) ->
    hackney_socket:setopts(HS, [{passive, false}, {packet, 0}]),
    case hackney_socket:peername(HS) of
        {ok, {Addr, Port}} ->
            #hackney_socket{transport=T, sock=S} = HS,
            Key = {T, Addr, Port},
            TRef = erlang:send_after(State#state.timeout, self(), {timeout, S}),
            ets:insert(State#state.sock_tab, {S, Key, TRef, HS}),
            ets:insert(State#state.ref_tab, {Key, S}),
            {reply, ok, State};
        Error ->
            {reply, Error, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, bad_call}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,S,_}, State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({tcp_closed,S}, State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({tcp_error,S,_},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({ssl,S,_},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({ssl_closed,S},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({ssl_error,S,_},State) ->
    delete_socket(S, State),
    {noreply,State};
handle_info({timeout,S},State) ->
    delete_socket(S, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


connect([IP | IPS], Port, Transport, Opts, Timeout, _Err) ->
    case hackney_socket:connect(Transport, IP, Port, Opts, Timeout) of
        {ok, HS} ->
            {ok, HS};
        {error, Reason} ->
            connect(IPS, Port, Transport, Opts, Timeout,
                    Reason)
    end;
connect([], _Port, _Transport, _Opts, _Timeout, Reason) ->
    {error, Reason}.


do_checkout([IP | IPs], Port, Transport, State) ->
    Refs = ets:lookup(State#state.ref_tab, {Transport, IP,Port}),
    checkout_ref(Refs, Transport, IPs, Port, State);
do_checkout([], _Port, _Transport, _State) ->
    {error, not_found}.

checkout_ref([], Transport, IPs, Port, State) ->
    do_checkout(IPs, Port, Transport, State);
checkout_ref([{Key, S} | Refs], Transport, IPs, Port, State) ->
    [{_,_,TRef,HS}] = ets:lookup(State#state.sock_tab, S),
    cancel_timer(S, TRef),
    ets:delete_object(State#state.ref_tab, {Key,S}),
    ets:delete(State#state.sock_tab, S),
    hackney_socket:setopts(HS, [{active, false}]),
    case sync_socket(HS) of
        true ->
            {ok, HS};
        false ->
            checkout_ref(Refs, Transport, IPs, Port, State)
    end.

sync_socket(#hackney_socket{sock=S}=HS) ->
    {Msg, MsgClosed, MsgError} = hackney_socket:messages(HS),
    receive
        {Msg, S, _} -> false;
        {MsgClosed, S}  -> false;
        {MsgError, S, _} -> false
    after 0 ->
              true
    end.

delete_socket(S, State) ->
    case ets:lookup(State#state.sock_tab, S) of
        [] ->
            ok;
        [{_,Key,TRef,HS}] ->
            cancel_timer(S, TRef),
            hackney_socket:close(HS),
            ets:delete(State#state.sock_tab, S),
            ets:delete_object(State#state.ref_tab,{Key,S}),
            ok
    end.

cancel_timer(Socket, Timer) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Socket} -> ok
            after 0 ->
                      ok
            end;
        _ -> ok
    end.
