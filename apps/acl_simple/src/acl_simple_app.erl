-module(acl_simple_app).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behaviour(application).

-include("acl_simple.hrl").

-export([start/2, stop/1]).

%%noinspection Erlang17Syntax
start(_StartType, _StartArgs) ->
    ?LOG_INFO("~n~n ========== Application start ==========", []),
    acl_simple = ets:new(acl_simple, [set, public, named_table]),

    {ok, Port} = application:get_env(acl_simple, listen_port),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", acl_simple_post_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    acl_simple_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

