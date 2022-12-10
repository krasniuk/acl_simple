-module(acl_simple_post_handler).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').

-export([init/2]).

-include("acl_simple.hrl").

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Resp = handle_post(Method, HasBody, Req),
    {ok, Resp, Opts}.


handle_post(<<"POST">>, true, Req) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    try
        Map = jsone:decode(Body),
        Method = binary_to_atom(maps:get(<<"method">>, Map), latin1),
        [{acl_simple_server, Pid}] = ets:lookup(acl_simple, acl_simple_server),
        handle_method(Method, Map, Pid, Req)
    catch
        exit:{timeout, Other} ->
            ?LOG_ERROR("503; server overloaded (exit : {timeout, ~p})", [Other]),
            cowboy_req:reply(503, #{<<"content-type">> => <<"text/plain">>}, <<"server overloaded">>, Req);
        Exception:Reason ->
            ?LOG_ERROR("400; invalid syntax of json (~p : ~p)", [Exception, Reason]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"invalid syntax of json">>, Req)
    end;
handle_post(Method, _, Req) ->
    ?LOG_ERROR("405; Not used method: ~p", [Method]),
    cowboy_req:reply(405, Req).


handle_method(user_add, Map, Pid, Req) ->
    NewUser = maps:get(<<"user">>, Map),
    Reply = gen_server:call(Pid, {user_add, NewUser}),
    reply_answer(Reply, Req);
handle_method(user_delete, Map, Pid, Req) ->
    User = maps:get(<<"user">>, Map),
    Reply = gen_server:call(Pid, {user_delete, User}),
    reply_answer(Reply, Req);
handle_method(show_all_users, _Map, Pid, Req) ->
    Reply = gen_server:call(Pid, {show_all_users}),
    reply_answer(Reply, Req);
handle_method(roles_add, Map, Pid, Req) ->
    User = maps:get(<<"user">>, Map),
    Roles = maps:get(<<"roles">>, Map),
    Reply = gen_server:call(Pid, {roles_add, User, Roles}),
    reply_answer(Reply, Req);
handle_method(roles_delete, Map, Pid, Req) ->
    User = maps:get(<<"user">>, Map),
    Roles = maps:get(<<"roles">>, Map),
    Reply = gen_server:call(Pid, {roles_delete, User, Roles}),
    reply_answer(Reply, Req);
handle_method(show_roles, Map, Pid, Req) ->
    User = maps:get(<<"user">>, Map),
    Reply = gen_server:call(Pid, {show_roles, User}),
    reply_answer(Reply, Req);
handle_method(show_allow_roles, _Map, Pid, Req) ->
    Reply = gen_server:call(Pid, {show_allow_roles}),
    reply_answer(Reply, Req);
handle_method(_UnknownMethod, _Map, _Pid, Req) ->
    reply_answer(unknownMethod, Req).

reply_answer(unknownMethod, Req) ->
    ?LOG_ERROR("400; unknown method in body", []),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"unknown method in body">>, Req);
reply_answer(Reply, Req) ->
    %?LOG_INFO("200; Reply = ~p",[Reply]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, jsone:encode(Reply), Req)
.
