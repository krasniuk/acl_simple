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
    handle_req(Req);
handle_post(<<"POST">>, false, Req) ->
    ?LOG_ERROR("Missing body ~p~n", [Req]),
    cowboy_req:reply(400, #{}, <<"Missing body.">>, Req);
handle_post(Method, _, Req) ->
    ?LOG_ERROR("Method ~p not allowed ~p~n", [Method, Req]),
    cowboy_req:reply(405, Req).

handle_req(Req) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    ?LOG_DEBUG("Post request ~p~n", [Body]),
    {Status, Json} = handle_body(Body),
    ?LOG_DEBUG("Post reply ~p~n", [Status]),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json; charset=UTF-8">>}, Json, Req).

handle_body(Body) ->
    try jsone:decode(Body) of
        Map ->
            handle_package(Map)
    catch
        exit:{timeout, Other} ->
            ?LOG_ERROR("503; server overloaded (exit : {timeout, ~p})", [Other]),
            {503, <<"server overloaded">>};
        Exception:Reason ->
            ?LOG_ERROR("400; invalid syntax of json (~p : ~p)", [Exception, Reason]),
            {400, <<"invalid syntax of json">>}
    end.

handle_package(Map) ->
    try
        #{<<"auth">> := AuthPack} = Map,
        #{<<"login">> := Login, <<"passhash">> := PassHash} = AuthPack,
        #{<<"parameters">> := ParamPack} = Map,
        handle_parameters({Login, PassHash}, ParamPack)
    catch
        _Class:Reason ->
            ?LOG_ERROR("Parse map error ~p Reason ~p~n", [Map, Reason]),
            {206, jsone:encode(#{<<"fail">> => <<"Invalid request format">>})}
    end.

handle_parameters({Login, PassHash}, #{<<"method">> := Method} = ParamPack) ->
    case auth(Login, PassHash, Method) of
        true ->
            handle_method(Method, ParamPack);
        false ->
            ?LOG_ERROR("Auth error, login ~p method ~p~n", [Login, Method]),
            {206, jsone:encode(#{<<"fail">> => <<"Invalid passhash or role absent">>})}
    end.

auth(_Login, _PassList, _Method) ->
    true.

-spec handle_method(binary(), map()) -> {integer(), binary()}.
handle_method(<<"user_add">>, Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    NewUser = maps:get(<<"user">>, Map),
    Reply = gen_server:call(Pid, {user_add, NewUser}),
    {200, jsone:encode(Reply)};
handle_method(<<"user_delete">>, Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    User = maps:get(<<"user">>, Map),
    Reply = gen_server:call(Pid, {user_delete, User}),
    {200, jsone:encode(Reply)};
handle_method(<<"show_all_users">>, _Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    Reply = gen_server:call(Pid, {show_all_users}),
    {200, jsone:encode(Reply)};
handle_method(<<"roles_add">>, Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    User = maps:get(<<"user">>, Map),
    Roles = maps:get(<<"roles">>, Map),
    Reply = gen_server:call(Pid, {roles_add, User, Roles}),
    {200, jsone:encode(Reply)};
handle_method(<<"roles_delete">>, Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    User = maps:get(<<"user">>, Map),
    Roles = maps:get(<<"roles">>, Map),
    Reply = gen_server:call(Pid, {roles_delete, User, Roles}),
    {200, jsone:encode(Reply)};
handle_method(<<"show_roles">>, Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    User = maps:get(<<"user">>, Map),
    Reply = gen_server:call(Pid, {show_roles, User}),
    {200, jsone:encode(Reply)};
handle_method(<<"show_allow_roles">>, _Map) ->
    [{_, Pid}] = ets:lookup(acl_simple, acl_simple_server),
    Reply = gen_server:call(Pid, {show_allow_roles}),
    {200, jsone:encode(Reply)}.
