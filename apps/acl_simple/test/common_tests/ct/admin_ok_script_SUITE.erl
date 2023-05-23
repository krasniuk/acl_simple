-module(admin_ok_script_SUITE).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').

-include_lit("common_test/include/ct.hrl").
-include("include/sys_config.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, groups/0]).
-export([compile_pause/1, test_script/1]).

-define(URL_ADMIN, "http://127.0.0.1:1913/admin").
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

groups() ->
    [
         {pause, [{repeat, 1}], [compile_pause]}
    ].

all() ->
    [
        {group, pause},
        test_script
    ].


%% ----------------------------------
%% Cases
%% ----------------------------------

compile_pause(_Config) ->
    ok = timer:sleep(1000).

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
    PassHash = binary_to_list(crypto:hash(sha, <<"1234">>)),
    ok = request(user_add, {<<"mike_test">>, PassHash}),
    ok = request(user_add, {<<"karl_test">>, PassHash}),
    Users = request(show_all_users, {}),
    true = lists:member(<<"mike_test">>, Users),
    true = lists:member(<<"karl_test">>, Users),
    AllowRoles = request(show_allow_roles, {}),
    Role1 = hd(AllowRoles),
    Role2 = lists:nth(2, AllowRoles),
    Role3 = lists:nth(3, AllowRoles),
    ok = request(roles_add, {<<"karl_test">>, [Role1]}),
    ok = request(roles_add, {<<"karl_test">>, [Role1, Role2]}),
    [Role1, Role2] = request(show_roles, {<<"karl_test">>}),
    ok = request(roles_delete, {<<"karl_test">>, [Role3]}),
    ok = request(roles_delete, {<<"karl_test">>, [Role3, Role1]}),
    [Role2] = request(show_roles, {<<"karl_test">>}),
    ok = request(roles_delete, {<<"karl_test">>, [Role2]}),
    Roles1 = request(show_roles, {<<"karl_test">>}),
    [] = Roles1,
    ok = request(roles_add, {<<"karl_test">>, [Role3, Role2]}),
    ok = request(user_delete, {<<"mike_test">>}),
    Users1 = request(show_all_users, {}),
    false = lists:member(<<"mike_test">>, Users1),
    ok = request(user_delete, {<<"karl_test">>}),
    Users2 = request(show_all_users, {}),
    false = lists:member(<<"mike_test">>, Users2).


% -----------------------------------

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
