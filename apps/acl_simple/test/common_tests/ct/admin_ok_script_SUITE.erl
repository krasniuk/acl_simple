-module(admin_ok_script_SUITE).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').

-include_lit("common_test/include/ct.hrl").
-include("include/sys_config.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, groups/0]).
-export([test_script/1, show_allow_roles/1]).

-define(URL_ADMIN, "http://127.0.0.1:1913/admin").
-define(HEADERS, [{"Content-type", "application/json;charset=UTF-8"}]).


%% ==================================
%% Export for Common Tests
%% ==================================

init_per_suite(Config) ->
    ok = application:set_env(?START_ENV),
    ok = acl_simple:start(),

    ok = timer:sleep(1000),
    AllowRoles = request(show_allow_roles, {}),
    case length(AllowRoles) of
        0 ->
            ok = request(add_allow_roles, {[<<"read">>, <<"write">>, <<"exec">>]});
        _Other ->
            ok
    end,
    ListUsers = request(show_all_users, {}),
    IsMike = lists:member(<<"mike1913_test">>, ListUsers),
    IsKarl = lists:member(<<"karl1913_test">>, ListUsers),
    case {IsMike, IsKarl} of
        {false, false} ->
            ok;
        {true, true} ->
            ok = request(user_delete, {<<"mike1913_test">>}),
            ok = request(user_delete, {<<"karl1913_test">>});
        {_, true} ->
            ok = request(user_delete, {<<"karl1913_test">>});
        {true, _} ->
            ok = request(user_delete, {<<"mike1913_test">>})
    end,
    Config.

end_per_suite(Config) ->
    ok = acl_simple:stop(),
    Config.

groups() ->
    [].

all() ->
    [
        test_script,
        show_allow_roles
    ].


%% ----------------------------------
%% Cases
%% ----------------------------------

test_script(_Config) ->
    PassHash = binary_to_list(crypto:hash(sha, <<"1234">>)),
    ok = request(user_add, {<<"mike1913_test">>, PassHash}),
    ok = request(user_add, {<<"karl1913_test">>, PassHash}),
    Users = request(show_all_users, {}),
    true = lists:member(<<"mike1913_test">>, Users),
    true = lists:member(<<"karl1913_test">>, Users),
    AllowRoles = request(show_allow_roles, {}),
    Role1 = hd(AllowRoles),
    Role2 = lists:nth(2, AllowRoles),
    Role3 = lists:nth(3, AllowRoles),
    ok = request(roles_add, {<<"karl1913_test">>, [Role1]}),
    ok = request(roles_add, {<<"karl1913_test">>, [Role1, Role2]}),
    [Role1, Role2] = request(show_roles, {<<"karl1913_test">>}),
    ok = request(roles_delete, {<<"karl1913_test">>, [Role3]}),
    ok = request(roles_delete, {<<"karl1913_test">>, [Role3, Role1]}),
    [Role2] = request(show_roles, {<<"karl1913_test">>}),
    ok = request(roles_delete, {<<"karl1913_test">>, [Role2]}),
    Roles1 = request(show_roles, {<<"karl1913_test">>}),
    [] = Roles1,
    ok = request(roles_add, {<<"karl1913_test">>, [Role3, Role2]}),
    ok = request(user_delete, {<<"mike1913_test">>}),
    Users1 = request(show_all_users, {}),
    false = lists:member(<<"mike1913_test">>, Users1),
    ok = request(user_delete, {<<"karl1913_test">>}),
    Users2 = request(show_all_users, {}),
    false = lists:member(<<"mike1913_test">>, Users2).

show_allow_roles(_Config) ->
    AllowRoles = request(show_allow_roles, {}),
    ok = request(add_allow_roles, {[<<"r1913">>, <<"w1913">>, <<"e1913">>]}),
    AllowRoles1 = request(show_allow_roles, {}),
    true = lists:member(<<"r1913">>, AllowRoles1),
    true = lists:member(<<"w1913">>, AllowRoles1),
    true = lists:member(<<"e1913">>, AllowRoles1),
    ok = request(delete_allow_roles, {[<<"r1913">>, <<"w1913">>, <<"e1913">>, hd(AllowRoles)]}),
    AllowRoles2 = request(show_allow_roles, {}),
    false = lists:member(hd(AllowRoles), AllowRoles2),
    ok = request(add_allow_roles, {[hd(AllowRoles)]}),
    AllowRoles = request(show_allow_roles, {}).


% -----------------------------------

-spec request(atom(), tuple()) -> ok | list().
request(show_allow_roles, {}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>, <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
        <<"parameters">> => #{<<"method">> => <<"show_allow_roles">>}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("show_allow_roles ~n RespBody = ~p", [RespBody]),
    #{<<"result">> := <<"ok">>,
        <<"roles">> := AllowRoles} = jsone:decode(RespBody),
    AllowRoles;
request(add_allow_roles, {Roles}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>, <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
        <<"parameters">> => #{<<"method">> => <<"add_allow_roles">>, <<"roles">> => Roles}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("show_allow_roles ~n RespBody = ~p", [RespBody]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody),
    ok;
request(delete_allow_roles, {Roles}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>, <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
        <<"parameters">> => #{<<"method">> => <<"delete_allow_roles">>, <<"roles">> => Roles}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("show_allow_roles ~n RespBody = ~p", [RespBody]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody),
    ok;

request(user_add, {User, PassHash}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>,
                             <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
             <<"parameters">> => #{<<"method">> => <<"user_add">>,
                                   <<"user">> => User,
                                   <<"passhash">> => PassHash}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("user_add ~p ~n RespBody = ~p", [User, RespBody]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody),
    ok;
request(user_delete, {User}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>,
                             <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
             <<"parameters">> => #{<<"method">> => <<"user_delete">>,
                                   <<"user">> => User}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("user_delete ~p ~n RespBody = ~p", [User, RespBody]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody),
    ok;
request(show_all_users, {}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>,
                             <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
             <<"parameters">> => #{<<"method">> => <<"show_all_users">>}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("show_all_users ~n RespBody = ~p", [RespBody]),
    #{<<"result">> := <<"ok">>,
        <<"users">> := ListUsers} = jsone:decode(RespBody),
    ListUsers;

request(show_roles, {User}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>, <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
             <<"parameters">> => #{<<"method">> => <<"show_roles">>, <<"user">> => User}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("show_roles ~p ~n RespBody = ~p", [User, RespBody]),
    #{<<"result">> := <<"ok">>,
        <<"user">> := User,
        <<"roles">> := ListRoles} = jsone:decode(RespBody),
    ListRoles;
request(roles_add, {User, Roles}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>,
                             <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
             <<"parameters">> => #{<<"method">> => <<"roles_add">>,
                                   <<"user">> => User,
                                   <<"roles">> => Roles}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("roles_add ~p ~n RespBody = ~p", [{User, Roles}, RespBody]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody),
    ok;
request(roles_delete, {User, Roles}) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"admin">>,
                             <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
             <<"parameters">> => #{<<"method">> => <<"roles_delete">>,
                                   <<"user">> => User,
                                   <<"roles">> => Roles}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("roles_delete ~p ~n RespBody = ~p", [{User, Roles}, RespBody]),
    #{<<"result">> := <<"ok">>} = jsone:decode(RespBody),
    ok.
