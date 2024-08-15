-module(acl_simple_app).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behaviour(application).

-include("acl_simple.hrl").

-export([start/2, stop/1]).


start(normal, _StartArgs) ->
    ?LOG_INFO("~n~n ========== Application start ==========", []),
    acl_simple = ets:new(acl_simple, [set, public, named_table]),
    true = ets:insert(acl_simple, [{server_cache, #{}}]),
    % ----------
    {ok, Port} = application:get_env(acl_simple, listen_port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/admin", acl_simple_admin_handler, []},
            {"/customer", acl_simple_customer_handler, []},
            {"/queue", acl_simple_queue_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    acl_simple_sup:start_link();

start({takeover, NodeCluster}, StartArg) ->
    _Pid = spawn(NodeCluster, acl_simple_app, stop, [null]), %% delete cluster run before execute main node acl_simple
    ok = timer:sleep(1000),                                  %% wait wile deleted
    start(normal, StartArg).


stop(_State) ->
    ok = cowboy:stop_listener(http).

