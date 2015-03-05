-module(hackney_spool).

-behaviour(gen_server).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                  code_change/3, terminate/2]).



-include("hackney.hrl").
-include_lib("../hackney_app/hackney_internal.hrl").


-define(SOCKETS, hackney_spool_sockets).
-define(REFS, hackney_spool_refs).

-define(state, {refs, sockets, pool_size}).
-define(hackney_sock, {transport,
                       socket}).



-export([start_link/2]).


checkout(Hosts0, Port, Transport, #client{options=Opts}=Client) ->
    Host = string:to_lower(Host0),
    Pid = self(),
    RequestRef = Client#client.request_ref,
    Name = proplists:get_value(pool, Opts, default),
    Pool = find_pool(Name, Opts),






start() ->
    %% NB this is first called from hackney_sup:start_link
    %%    BEFORE the hackney_pool ETS table exists
    ok.


%% @doc return a child spec suitable for embeding your pool in the
%% supervisor
child_spec(Name, Options0) ->
    Options = [{name, Name} | Options0],
    {Name, {?MODULE, start_link, [Name, Options]},
      permanent, 10000, worker, [?MODULE]}.




%% @doc start a pool
start_pool(Name, Options) ->
    case find_pool(Name, Options) of
        Pid when is_pid(Pid) ->
            ok;
        Error ->
            Error
    end.


%% @doc stop a pool
stop_pool(Name) ->
    case find_pool(Name) of
        undefined ->
            ok;
        _Pid ->
            case supervisor:terminate_child(hackney_sup, Name) of
                ok ->
                    supervisor:delete_child(hackney_sup, Name),
                    ets:delete(hackney_pool, Name),
                    ok;
                Error ->
                    Error
            end
    end.


start_link(Name, Options0) ->
    Options = hackney_util:maybe_apply_defaults([pool_size,  Options0),
    gen_server:start_link(?MODULE, [Name, Options], []).


init([Name, Options]) ->
    Sockets = ets:new(?SOCKETS, [set]),
    Refs = ets:new(?REFS, [bag]),
    PoolSize = proplists:get_value(pool_size, Options),
    {ok, #state{refs = Refs,
                sockets = Sockets,
                pool_size = PoolSize}}.


handle_call({checkout, Transport, IPs, Port, Pid}, _From, State) ->
    case do_checkout(IPs, Transport, Port, Pid, State) of
        {ok, #hackney_sock{socket=Sock}=HS} ->
            Transport:controlling_process(Sock ,Pid),
            {reply, {ok, Sock, HS}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({checkin, HS} _From, State) ->
    #hackney_sock{transport=Transport, socket=Sock}=HS,
    Transport:setopts(Sock, [{active, once}, {packet, 0}]),
    case Transport:peername(Sock) of
        {ok, {Addr, Port}} ->
            Key = {Transport, Addr, Port},
            ets:insert(State#state.sockets, {Sock, Key, HS}),
            ets:insert(State#state.refs, {Key, Sock}),
            {reply, ok, State};
        Error ->
            {reply, Error, State}
    end;
handle_call(stop, _From, State) ->
    {stop, notmal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, bad_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, _}, State) ->
    delete_socket(Sock, State),
    {noreply, State};
andle_info({tcp_closed, Sock}, State) ->
    delete_socket(Sock, State),
    {noreply, State};
handle_info({tcp_error, Sock, _}, State) ->
    delete_socket(Sock, State),
    {noreply, State};
handle_info({ssl, Sock, _}, State) ->
    delete_socket(Sock, State),
    {noreply, State};
handle_info({ssl_closed, Sock}, State) ->
    delete_socket(Sock, State),
    {noreply, State};
handle_info({ssl_error, Sock, _}, State) ->
    delete_socket(Sock, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_checkout([], _Transport, _Port, _State) ->
    {error, no_socket};
do_checkout([IP | Rest], Transport, Port, State) ->
    Refs = ets:lookup(State#state.refs, {Proto, IP, Port}),
    checkout_ref(Refs, Proto, Rest, Port, State).

checkout_ref([], Transport, IPs, Port, State) ->
    do_checkout(IPs, Transport, Port, State);
checkout_ref([{Key, Sock} | Rest], Transport, IPs, Port, State) ->
    [{_, _, HS}] = ets:lookup(State#state.sockets, Sock),
    ets:delete_object(State#state.refs, {Key, Sock}),
    ets:delete(State#state.sockets, Sock),
    Transport:setopts(Sock, [{active, false}]),
    case sync_socket(HS) of
        false ->
            checkout_ref(Rest, Transport, IPs, Port, State);
        true ->
            {ok, Sock, HS}
    end.

sync_socket(#hackney_sock{transport=Transport, socket=Sock}) ->
    {Msg, ClosedMsg, ErrorMsg} = Transport:messages(Sock),
    receive
        {Msg, Sock, _} -> false;
        {MsgClosed, Sock} -> false;
        {MsgError, Sock, _} -> false
    after 0 ->
              true
    end.

delete_socket(Sock, State) ->
    case ets:lookup(State#state.sockets, Sock) of
        [] -> ok;
        [{_, Key, #hackney_sock{transport=Transport}}] ->
            catch Transport:close(Sock),
            ets:delete(State#state.sockets, Sock),
            ets:delete_object(State#state.refs, {Key, Sock}),
            ok
    end.


do_start_pool(Name, Options) ->
    Spec = child_spec(Name, Options),
    case supervisor:start_child(hackney_sup, Spec) of
        {ok, Pid} ->
            Pid;
         {error, {already_started, _}} ->
            find_pool(Name, Options)
    end.


find_pool(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            undefined;
        [{_, Pid}] ->
            Pid
    end.

find_pool(Name, Options) ->
     case ets:lookup(?MODULE, Name) of
        [] ->
            do_start_pool(Name, Options);
        [{_, Pid}] ->
            Pid
    end.

