-module(acl_simple_sup).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, Args} = application:get_env(poolboy, pools),
    PoolSpecs = lists:map(fun({Name, PoolArgs, WorkerArgs}) ->
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Args),
    SupFlags = {one_for_one, 5, 100},
    Server = {acl_simple_server, {acl_simple_server, start_link, []}, permanent, 1000, worker, []},
    TimerCache = {acl_simple_timer_cache, {acl_simple_timer_cache, start_link, []}, permanent, 1000, worker, []},

    {ok, {SupFlags, PoolSpecs ++ [TimerCache, Server]}}.
