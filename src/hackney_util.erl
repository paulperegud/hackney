%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_util).

-export([filter_options/3]).
-export([set_option_default/3]).
-export([require/1]).
-export([maybe_apply_defaults/2]).
-export([is_ipv6/1]).
-export([privdir/0]).
-export([mod_metrics/0]).
-export([to_atom/1]).
-export([user_opts/2, user_opts/3]).
-export([valid_opts/2]).
-export([increment/1, increment/2]).
-export([rand_increment/1, rand_increment/2]).
-export([maybe_seed/0]).


-include("hackney.hrl").

%% @doc filter a proplists and only keep allowed keys
-spec filter_options([{atom(), any()} | {raw, any(), any(), any()}],
	[atom()], Acc) -> Acc when Acc :: [any()].
filter_options([], _, Acc) ->
	Acc;
filter_options([Opt = {Key, _}|Tail], AllowedKeys, Acc) ->
	case lists:member(Key, AllowedKeys) of
		true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
		false -> filter_options(Tail, AllowedKeys, Acc)
	end;
filter_options([Opt = {raw, _, _, _}|Tail], AllowedKeys, Acc) ->
	case lists:member(raw, AllowedKeys) of
		true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
		false -> filter_options(Tail, AllowedKeys, Acc)
	end;
filter_options([Opt|Tail], AllowedKeys, Acc) when is_atom(Opt) ->
	case lists:member(Opt, AllowedKeys) of
		true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
		false -> filter_options(Tail, AllowedKeys, Acc)
	end.

%% @doc set the default options in a proplists if not defined
-spec set_option_default(Opts, atom(), any())
	-> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) ->
	case lists:keymember(Key, 1, Opts) of
		true -> Opts;
		false -> [{Key, Value}|Opts]
	end.

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Rest]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Rest).

maybe_apply_defaults([], Options) ->
    Options;
maybe_apply_defaults([OptName | Rest], Options) ->
    case proplists:is_defined(OptName, Options) of
        true ->
            maybe_apply_defaults(Rest, Options);
        false ->
            {ok, Default} = application:get_env(hackney, OptName),
            maybe_apply_defaults(Rest, [{OptName, Default} | Options])
    end.

is_ipv6(Host) ->
    case inet_parse:address(Host) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            true;
        {ok, {_, _, _, _}} ->
            false;
        _ ->
            case inet:getaddr(Host, inet) of
                {ok, _} ->
                    false;
                _ ->
                    case inet:getaddr(Host, inet6) of
                        {ok, _} ->
                            true;
                        _ ->
                            false
                    end
            end
    end.

privdir() ->
    case code:priv_dir(hackney) of
        {error, _} ->
            %% try to get relative priv dir. useful for tests.
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.


mod_metrics() ->
    case application:get_env(hackney, mod_metrics) of
        {ok, folsom} -> hackney_folsom_metrics;
        {ok, exometer} -> hackney_exometer_metrics;
        {ok, dummy} -> hackney_dummy_metrics;
        {ok, Mod} ->
            _ = code:ensure_loaded(Mod),
            case erlang:function_exported(Mod, new, 2) of
                false ->
                    {error, badarg};
                true ->
                    Mod
            end;
        _ -> hackney_dummy_metrics
    end.


to_atom(V) when is_list(V) ->
    try
        list_to_existing_atom(V)
    catch
        _:_ -> list_to_atom(V)
    end;
to_atom(V) when is_binary(V) ->
    to_atom(binary_to_list(V));
to_atom(V) when is_atom(V) ->
    V.

user_opts(Type, Opts) ->
    user_opts(Type, Opts, undefined).

user_opts(Type, Opts, Default) ->
    proplists:get_value(Type, Opts, get_app_env(Type, Default)).

valid_opts(Type, Default) ->
    Opts = get_app_env(Type, Default),
    check_opts(Type, Opts).

check_opts(Type, Opts) when is_list(Opts) ->
    Check = check_option_f(Type),
    lists:map(fun(X) ->
                      case Check(X) of
                          true -> X;
                          false ->
                              erlang:error({illegal_option, X},
                                           [Type,  Opts])
                      end
              end, Opts);
check_opts(Type, Other) ->
    erlang:error(invalid_options, [Type, Other]).

check_option_f(sockets_ets_options)    -> fun check_ets_option/1;
check_option_f(manager_ets_options)    -> fun check_ets_option/1;
check_option_f(sockets_server_options) -> fun check_server_option/1;
check_option_f(manager_options) -> fun check_server_option/1.

check_ets_option({read_concurrency , B}) -> is_boolean(B);
check_ets_option({write_concurrency, B}) -> is_boolean(B);
check_ets_option(_) -> false.

check_server_option({priority, P}) ->
    %% Forbid setting priority to 'low' since that would
    %% surely cause problems. Unsure about 'max'...
    lists:member(P, [normal, high, max]);
check_server_option(_) ->
    %% assume it's a valid spawn option
    true.

get_app_env(Key, Default) ->
    case application:get_env(Key) of
        undefined       -> Default;
        {ok, undefined} -> Default;
        {ok, Value}     -> Value
    end.

%% Just do the increments by hand!
-spec increment(pos_integer()) -> pos_integer().
increment(N) when is_integer(N) -> N bsl 1.

-spec increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
increment(N, Max) -> min(increment(N), Max).

%% Just do the random increments by hand!
%% Algorithm inspired in the Google HTTP Java client implementation of Class ExponentialBackOff.
%% See: http://javadoc.google-http-java-client.googlecode.com/hg/1.18.0-rc/com/google/api/client/util/ExponentialBackOff.html
-spec rand_increment(pos_integer()) -> pos_integer().
rand_increment(N) ->
    RandFactor =  get_app_env(rand_factor, 0.5),
    DefMultiplier = get_app_env(def_multiplier, 1.5),
    Rand = 1 - RandFactor + random:uniform(),
    erlang:round(increment(N) * DefMultiplier * Rand).

-spec rand_increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
rand_increment(N, Max) -> min(rand_increment(N), Max).

maybe_seed() ->
    case erlang:get(random_seed) of
        undefined -> random:seed(erlang:now());
        {X,X,X} -> random:seed(erlang:now());
        _ -> ok
    end.
