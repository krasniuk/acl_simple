-module(customer_ok_SUITE).
-author('Mykhailo Krasniuk <mykhailo.krasniuk@privatbank.ua>').

-include_lit("common_test/include/ct.hrl").
-include("include/sys_config.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, groups/0]).
-export([get_roles/1]).

-define(URL_CUSTOMER, "http://127.0.0.1:1913/customer").
-define(HEADERS, [{"Content-type", "application/json;charset=UTF-8"}]).


%% ==================================
%% Export for Common Tests
%% ==================================

init_per_suite(Config) ->
    %  ok = application:set_env(?START_ENV),
    %   ok = acl_simple:start(),
    Config.

groups() ->
    [].

end_per_suite(Config) ->
    %ok = acl_simple:stop(),
    Config.


all() ->
    [
        get_roles
    ].


%% =================================================
%% Cases
%% =================================================

get_roles(_Config) ->
    Body = #{<<"auth">> => #{<<"login">> => <<"mike">>, <<"passhash">> => binary_to_list(crypto:hash(sha, <<"1234">>))},
        <<"parameters">> => #{<<"method">> => <<"get_roles">>}},
    ReqBody = jsone:encode(Body),
    {ok, {_, _, RespBody}} = httpc:request(post, {?URL_CUSTOMER, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("get_roles ~p ~n RespBody = ~p", [<<"mike">>, RespBody]),
    #{<<"result">> := <<"ok">>,
        <<"roles">> := _ListRoles} = jsone:decode(RespBody).

