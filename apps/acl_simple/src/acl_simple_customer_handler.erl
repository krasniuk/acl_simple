-module(acl_simple_customer_handler).
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
        #{<<"parameters">> := Param} = Map,
        handle_parameters({Login, PassHash}, Param)
    catch
        _Class:Reason ->
            ?LOG_ERROR("Parse map error ~p Reason ~p~n", [Map, Reason]),
            {206, jsone:encode(#{<<"fail">> => <<"Invalid request format">>})}
    end.

handle_parameters({Login, PassHash}, #{<<"method">> := Method} = Param) ->
    case auth(Login, PassHash, Method) of
        true ->
            handle_method(Method, Param#{<<"login">> => Login});
        false ->
            ?LOG_ERROR("Auth error, login ~p method ~p~n", [Login, Method]),
            {206, jsone:encode(#{<<"fail">> => <<"Invalid passhash or role absent">>})}
    end.

-spec auth(binary(), list(), binary()) -> true|false.
auth(Login, Hash, _Method) ->
    case acl_simple_pg:select("get_passhash", [Login]) of
        {ok, _, [{JsonHash}]} ->
            RealHash = jsone:decode(JsonHash),
            RealHash == Hash;
        {ok, _, []} ->
            ?LOG_ERROR("Inditification fail", []),
            false
    end.


%% ================== handle_method ==================

handle_method(<<"get_roles">>, #{<<"login">> := UserName} = _Param) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    Reply = case maps:find(UserName, Cache) of
                {ok, Roles} ->
                    ?JSON_ROLES_OF_USER(UserName, Roles);
                error ->
                    ?JSON_ERROR("User '" ++ binary_to_list(UserName) ++ "' is not exist")
            end,
    {200, jsone:encode(Reply)}.