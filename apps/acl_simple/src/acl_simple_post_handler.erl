-module(acl_simple_post_handler).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-export([init/2]).
-include("acl_simple.hrl").

init(Req, Opts) ->
    ?LOG_INFO("~nReq = ~p", [Req]),
    Method = cowboy_req:method(Req), %Получение метода
    HasBody = cowboy_req:has_body(Req), % Есть ли тело? true|false
    Resp = handle_post(Method, HasBody, Req),
    {ok, Resp, Opts}.

handle_post(<<"POST">>, true, Req) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    try
        Map = jsone:decode(Body),
        Method = binary_to_atom(maps:get(<<"method">>, Map), latin1),
        [{acl_simple_server, Pid}] = ets:lookup(acl_simple, acl_simple_server),
        Reply = handle_method(Method, Map, Pid),
        case Reply of
            unknownMethod ->
                cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"error">>, Req);
            _ ->
                cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, jsone:encode(Reply), Req)
        end
    catch
        _:_ -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"error in json">>, Req)
    end;
handle_post(_Method, _, Req) ->
    cowboy_req:reply(405, Req).

handle_method(user_add, Map, Pid) ->
    NewUser = maps:get(<<"user">>, Map),
    gen_server:call(Pid, {user_add, NewUser});
handle_method(user_delete, Map, Pid) ->
    User = maps:get(<<"user">>, Map),
    gen_server:call(Pid, {user_delete, User});
handle_method(show_all_users, _Map, Pid) -> gen_server:call(Pid, {show_all_users});
handle_method(roles_add, Map, Pid) ->
    User = maps:get(<<"user">>, Map),
    Roles = maps:get(<<"roles">>, Map),
    gen_server:call(Pid, {roles_add, User, Roles});
handle_method(roles_delete, Map, Pid) ->
    User = maps:get(<<"user">>, Map),
    Roles = maps:get(<<"roles">>, Map),
    gen_server:call(Pid, {roles_delete, User, Roles});
handle_method(show_roles, Map, Pid) ->
    User = maps:get(<<"user">>, Map),
    gen_server:call(Pid, {show_roles, User});
handle_method(show_allow_roles, _Map, Pid) ->
    gen_server:call(Pid, {show_allow_roles});
handle_method(_UnknownMethod, _, _Pid) ->
    unknownMethod.
