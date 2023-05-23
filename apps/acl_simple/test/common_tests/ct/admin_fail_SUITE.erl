-module(admin_fail_SUITE).
-author('Mykhailo Krasniuk <mykhailo.krasniuk@privatbank.ua>').

-include_lit("common_test/include/ct.hrl").
-include("include/sys_config.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, groups/0]).

-export([missing_body/1, bad_http_method/1, parse_body_fail/1]).

-define(URL_ADMIN, "http://127.0.0.1:1913/admin").
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
    ok = acl_simple:stop(),
    Config.


all() ->
    [
        missing_body,
        bad_http_method,
        parse_body_fail
    ].


%% =================================================
%% Cases
%% =================================================

missing_body(_Config) ->
    {ok, {{_, 400, _}, _Headers, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ""},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [RespBody]),
    <<"Missing body.">> = RespBody.

bad_http_method(_Config) ->
    {ok, {{_, 405, _}, _Headers, []}} = httpc:request(get, {?URL_ADMIN, []}, [{timeout, 4000}], []).

parse_body_fail(_Config) ->
    ReqBody = "{\"parameters\":{\"method\":\"show_allow_roles\"",
    {ok, {{_, 206, _}, _Headers, RespBody}} = httpc:request(post, {?URL_ADMIN, ?HEADERS, "application/json;charset=UTF-8", ReqBody},
        [{timeout, 4000}], [{body_format, binary}]),
    ok = ct:pal("RespBody = ~p", [RespBody]),
    #{<<"fail">> := <<"Invalid request format">>} = jsone:decode(RespBody).

