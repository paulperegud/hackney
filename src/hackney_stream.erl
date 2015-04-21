-module(hackney_stream).


-export([init/4,
         system_continue/3,
         system_terminate/4,
         system_code_change/4]).


-define(DEFAULT_KEEPALIVE, 5000).
-define(DEFAULT_RETRY, 5).
-define(RETRY_TIMEOUT, 5000).
-define(RETRY_START, 200). %% we start to retry the connection after 200ms

-ifdef(no_ssl_name_validation).
-define(VALIDATE_SSL, normal).
-else.
-define(VALIDATE_SSL, host).
-endif.

-record(state, {parent,
                host,
                port,
                insecure,
                ssl_options,
                sock,
                pool,
                type,
                keepalive,
                keepalive_tref,
                retry,
                retry_current,
                retry_timeout,
                protocol,
                protocol_opts,
                protocol_state}).

start_link(Host, Port, Opts) ->
    proc_lib:start_link(?MODULE, init, [self(), Host, Port, Opts]).


init(Parent, Host, Port, Opts) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    Keepalive = set_keepalive(hackney_util:get_value(keepalive, Opts)),
    Type = hackney_util:get_value(type, Opts, default_type(Port)),
    Pool = hackney_util:get_value(pool, Opts, false),
    Retry = hackney_util:get_value(retry, Opts, ?DEFAULT_RETRY),
    RetryTimeout = hackney_util:get_value(retry_timeout, Opts, ?RETRY_TIMEOUT),
    ProtocolOpts = hackney_util:get_value(protocol_opts, Opts, []),
    Insecure = hackney_util:get_value(insecure, Opts, false),
    SSLOpts = hackney_util:get_value(ssl_opts, Opts),
    connect(#state{parent=Parent, host=Host, port=Port, pool=Pool, type=Type,
                   insecure=Insecure, ssl_options=SSLOpts, keepalive=Keepalive,
                   retry=Retry, retry_current=?RETRY_START,
                   retry_timeout=RetryTimeout, protocol_opts=ProtocolOpts}, Retry).


connect(#state{host=Host, port=Port, pool=Poolname, type=ssl}=State, Retries) ->
    Transport = hackney_ssl,
    Opts = [binary, {active, false} | ssl_opts(Host, State)],
    case connect1(Transport, Host, Port, Opts, Poolname) of
        {ok, HS} ->
            Protocol = hackney_http_stream,
            PState = Protocol:init(Host, HS, State#state.protocol_opts),
            before_loop(State#state{sock=HS, retry_current=?RETRY_START,
                                    protocol=Protocol, protocol_state=PState});
        {error, _} ->
            retry(State, Retries - 1)
    end;
connect(#state{host=Host, port=Port, pool=Poolname}=State, Retries) ->
    Transport = hackney_tcp,
    Opts = [binary, {active, false}],
    case connect1(Transport, Host, Port, Opts, Poolname) of
        {ok, HS} ->
            Protocol = hackney_http_stream,
            PState = Protocol:init(Host, HS, State#state.protocol_opts),
            before_loop(State#state{sock=HS, retry_current=?RETRY_START,
                                    protocol=Protocol, protocol_state=PState});
        {error, _} ->
            retry(State, Retries - 1)
    end.

before_loop(#state{keepalive=false}=State) ->
    loop(State);
before_loop(#state{keepalive=Keepalive}=State) ->
    TRef = erlang:send_after(Keepalive, self(), keepalive),
    loop(State#state{keepalive_tref=TRef}).


retry(_State, 0) ->
    ok;
retry(#state{keepalive_tref=TRef}=State, Retries) when is_reference(TRef) ->
    erlang:cancel_timer(TRef),
    receive
        keepalive -> ok
    after 0 ->
              ok
    end,
    retry(State#state{keepalive_tref=undefined}, Retries);
retry(State, Retries) ->
    hackney_util:maybe_seed(),
    retry_loop(State, Retries).


retry_loop(_State, 0) ->
    error(max_retry);
retry_loop(#state{parent=Parent, retry_current=Delay, retry_timeout=Max}, Retries) ->
    NewDelay = hackney_util:rand_increment(Delay, Max),
    erlang:send_after(Delay, self(), retry),
    receive
        retry ->
            connect(State#state{retry_current=NewDelay}, Retries);
        {system, _From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {retry_loop, State, Retries})
    end.


loop(#state{sock=HS, parent=Parent}=State) ->
    {Msg, MsgClosed, MsgErr} = hackney_socket:messages(HS),
    ok.


connect1(Transport, Host, Port, Opts, false) ->
    hackney_socket:connect(Transport, Host, Port, Opts);
connect1(Transport, Host, Port, Opts, PoolName) ->
    hackney_sockets_pool:checkout(PoolName, Transport, Host, Port, Opts).

ssl_opts(Host, #state{insecure=Insecure, ssl_options=undefined}) ->
    case {Insecure, should_validate_ssl()} of
        {true, _} ->
            [{verify, verify_none}, {reuse_session, true}];
        {_, host} ->
            [{verify_fun, {fun ssl_verify_hostname:verify_fun/3,
                           [{check_hostname, Host}]}},
             {cacertfile, cacertfile()},
             {server_name_indication, Host},
             {verify, verify_peer},
             {depth, 99}];
        {_, normal} ->
            [{cacertfile, cacertfile()},
             {verify, verify_peer},
             {depth, 99}]
    end;
ssl_opts(_, #state{ssl_options=SSLOpts}) ->
    SSLOpts.

set_keepalive(true) -> ?DEFAULT_KEEPALIVE;
set_keepalive(T) when is_integer(T) -> T;
set_keepalive(_) ->false.

default_type(80) -> tcp;
default_type(_) -> ssl.

should_validate_ssl() ->
    ?VALIDATE_SSL.

cacertfile() ->
    filename:join(hackney_util:privdir(), "ca-bundle.crt").
