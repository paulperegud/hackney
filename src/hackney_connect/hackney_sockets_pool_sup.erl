%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_sockets_pool_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).



-spec init([]) ->
    {'ok', {{'simple_one_for_one', 4, 3600},
            [{'hackney_sockets_pool',
              {'hackney_sockets_pool', 'start_link', []},
              'temporary', 30000, 'worker', ['hackney_sockets_pool']}]}}.

init([]) ->
    SupFlags = {simple_one_for_one, 4, 3600},
    Child = {hackney_sockets_pool,
             {hackney_sockets_pool, start_link, []},
             temporary, 30000, worker, [hackney_sockets_pool]},
    {ok, {SupFlags, [Child]}}.
