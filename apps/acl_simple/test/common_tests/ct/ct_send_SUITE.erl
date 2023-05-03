-module(ct_send_SUITE).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').

-include_lit("common_test/include/ct.hrl").
-include("include/sys_config.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([show_all_users/1]).

-define(URL, "http://127.0.0.1:1913/").
-define(HEADERS, [{"Content-type", "application/json;charset=UTF-8"}]).


%% ==================================
%% Export for Common Tests
%% ==================================

init_per_suite(Config) ->
    ok = application:set_env(?START_ENV),
    ok = acl_simple:start(),
    Config.

end_per_suite(Config) ->
    ok = acl_simple:stop(),
    Config.

all() ->
    [
        show_all_users
    ].


%% ----------------------------------
%% Cases
%% ----------------------------------

show_all_users(Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
                                                 [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := ListUsers} = jsone:decode(RespBody),
    [{show_all_users, ListUsers}|Config].

user_add_mike_test(_Config) ->
    Body = #{<<"method">> => <<"user_add">>,
             <<"user">> => <<"mike_test">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody).

user_add_karl_test(_Config) ->
    Body = #{<<"method">> => <<"user_add">>,
             <<"user">> => <<"karl_test">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody).

show_all_users(Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := ListUsers} = jsone:decode(RespBody),



user_add_mike_test(_Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := _List} = jsone:decode(RespBody).


user_add_mike_test(_Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := _List} = jsone:decode(RespBody).


user_add_mike_test(_Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := _List} = jsone:decode(RespBody).


user_add_mike_test(_Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := _List} = jsone:decode(RespBody).


user_add_mike_test(_Config) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := _List} = jsone:decode(RespBody).