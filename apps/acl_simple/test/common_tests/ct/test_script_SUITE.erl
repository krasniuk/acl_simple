-module(test_script_SUITE).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').

-include_lit("common_test/include/ct.hrl").
-include("include/sys_config.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_script/1]).

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
        test_script
    ].


%% ----------------------------------
%% Cases
%% ----------------------------------

test_script(_Config) ->
    %% ------- Prepare --------
    ListUsers = request(show_all_users, {}),
    IsMike = lists:member(<<"mike_test">>, ListUsers),
    IsKarl = lists:member(<<"karl_test">>, ListUsers),
    case {IsMike, IsKarl} of
        {false, false} ->
            ok;
        {true, true} ->
            ok = request(user_delete, {<<"mike_test">>}),
            ok = request(user_delete, {<<"karl_test">>});
        {_, true} ->
            ok = request(user_delete, {<<"karl_test">>});
        {true, _} ->
            ok = request(user_delete, {<<"mike_test">>})
    end,
    % ------- Begin_test -------
    ok = request(user_add, {<<"mike_test">>}),
    ok = request(user_add, {<<"karl_test">>}),
    Users = request(show_all_users, {}),
    true = lists:member(<<"mike_test">>, Users),
    true = lists:member(<<"karl_test">>, Users),
    ok = request(roles_add, {<<"karl_test">>, [<<"read">>]}),
    ok = request(roles_add, {<<"karl_test">>, [<<"read">>, <<"write">>]}),
    [<<"read">>, <<"write">>] = request(show_roles, {<<"karl_test">>}),
    ok = request(roles_delete, {<<"karl_test">>, [<<"exec">>]}),
    ok = request(roles_delete, {<<"karl_test">>, [<<"exec">>, <<"read">>]}),
    [<<"write">>] = request(show_roles, {<<"karl_test">>}),
    ok = request(roles_delete, {<<"karl_test">>, [<<"write">>]}),
    Roles1 = request(show_roles, {<<"karl_test">>}),
    [] = Roles1,
    ok = request(roles_add, {<<"karl_test">>, [<<"exec">>, <<"write">>]}),
    ok = request(user_delete, {<<"mike_test">>}),
    Users1 = request(show_all_users, {}),
    false = lists:member(<<"mike_test">>, Users1),
    ok = request(user_delete, {<<"kark_test">>}),
    Users1 = request(show_all_users, {}),
    false = lists:member(<<"mike_test">>, Users1),
    ok = request(show_allow_roles, {}).


% -----------------------------------

request(show_allow_roles, {}) ->
    Body = #{<<"method">> => <<"show_allow_roles">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"roles">> := [<<"read">>, <<"write">>, <<"exec">>]} = jsone:decode(RespBody);

request(show_roles, {User}) ->
    Body = #{<<"method">> => <<"show_roles">>,
        <<"user">> => User},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"user">> := <<"mike_test">>,
        <<"roles">> := ListRoles} = jsone:decode(RespBody),
    ListRoles;
request(roles_add, {User, Roles}) ->
    Body = #{<<"method">> => <<"roles_add">>,
             <<"user">> => User,
             <<"roles">> => Roles},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody);
request(roles_delete, {User, Roles}) ->
    Body = #{<<"method">> => <<"roles_delete">>,
        <<"user">> => User,
        <<"roles">> => Roles},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody);

request(user_add, {User}) ->
    Body = #{<<"method">> => <<"user_add">>,
        <<"user">> => User},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody);
request(user_delete, {User}) ->
    Body = #{<<"method">> => <<"user_delete">>, <<"user">> => User},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody);
request(show_all_users, _) ->
    Body = #{<<"method">> => <<"show_all_users">>},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [jsone:decode(RespBody)]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := ListUsers} = jsone:decode(RespBody),
    ListUsers.
