%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    %% start the pool handler
    PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),
    ok = PoolHandler:start(),

    %% finish to start the application
    {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SocketsPoolSup = {hackney_sockets_pool_sup,
                      {hackney_sockets_pool_sup, start_link, []},
                      permanent, infinity, supervisor, [hackney_sockets_pool_sup]},

    SocketsServer = {hackney_sockets_server,
                     {hackney_sockets_server, start_link, []},
                     permanent, 5000, worker, [hackney_sockets_server]},

    Manager = {hackney_manager,
               {hackney_manager, start_link, []},
               permanent, 5000, worker, [hackney_manager]},

    {ok, { {one_for_one, 10, 1}, [SocketsPoolSup, SocketsServer, Manager]}}.
