-module(acl_simple_server).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behavior(gen_server).

-include("acl_simple.hrl").
-define(ROLES, [<<"read">>, <<"write">>, <<"exec">>]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/1]).

% ====================================================
% Users functions
% ====================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).
stop(Pid) ->
    gen_server:call(Pid, terminate).


% ====================================================
% Inverse functions
% ====================================================

%Constructor
init([]) ->
    ?LOG_INFO("-> new Server", []),
    true = ets:insert(acl_simple, [{acl_simple_server, self()}]),
    {ok, []}.


%DeConstructor
terminate(_, _State) ->
    ?LOG_INFO("-> Server stoped", []),
    ok.


% USERS
handle_call({user_add, UserName}, _From, State) ->
    Reply = case get_map_from_db() of
                no_connect ->
                    error_map("no connect with db");
                {error, _} ->
                    error_map("error in reqare db");
                Map ->
                    case maps:find(UserName, Map) of
                        {ok, _} ->
                            error_map("User " ++ binary_to_list(UserName) ++ " exist");
                        error ->
                            Args = [list_to_binary(uuid:to_string(simple, uuid:uuid1())), UserName],
                            case acl_simple_pg:insert("user_add", Args) of
                                {ok, _} ->
                                    NewMap = Map#{UserName => []},
                                    true = ets:insert(acl_simple, [{server_cache, NewMap}]),
                                    #{<<"result">> => <<"ok">>};
                                {error, _} ->
                                    error_map("error in reqare db")
                            end
                    end
            end,
    {reply, Reply, State};
handle_call({user_delete, UserName}, _From, _State) ->
    Reply = case get_map_from_db() of
                no_connect ->
                    error_map("no connect with db");
                {error, _} ->
                    error_map("error in reqare db");
                Map ->
                    case maps:find(UserName, Map) of
                        {ok, _} ->
                            case handler_roles_delete(?ROLES, UserName) of
                                {error, _} ->
                                    error_map("error in reqare db");
                                ok ->
                                    case acl_simple_pg:delete("users_delete_by_name", [UserName]) of
                                        {error, _} ->
                                            error_map("error in reqare db");
                                        {ok, _} ->
                                            NewMap = maps:remove(UserName, Map),
                                            true = ets:insert(acl_simple, [{server_cache, NewMap}]),
                                            #{<<"result">> => <<"ok">>}
                                    end
                            end;
                        error ->
                            error_map("User '" ++ binary_to_list(UserName) ++ "' is not exist")
                    end
            end,
    {reply, Reply, []};
handle_call({show_all_users}, _From, State) ->
    [{_, Map}] = ets:lookup(acl_simple, server_cache),
    List = maps:keys(Map),
    Reply = #{
        <<"result">> => <<"ok">>,
        <<"users">> => List
    },
    {reply, Reply, State};

% ROLES
handle_call({roles_add, UserName, Roles}, _From, _State) ->
    Reply = case get_map_from_db() of
                no_connect -> error_map("no connect with db");
                {error, _} ->
                    error_map("error in reqare db");
                Map ->
                    case maps:find(UserName, Map) of
                        {ok, RolesOld} ->
                            case is_real_roles(Roles) of
                                true ->
                                    RolesAdd = handler_roles_add(UserName, RolesOld, Roles),
                                    case lists:member(error, RolesAdd) of
                                        true ->
                                            RolesAdd1 = lists:delete(error, RolesAdd),
                                            NewMap = Map#{UserName := RolesOld ++ RolesAdd1},
                                            true = ets:insert(acl_simple, [{server_cache, NewMap}]),
                                            error_map("error in reqare db");
                                        false ->
                                            NewMap = Map#{UserName := RolesOld ++ RolesAdd},
                                            true = ets:insert(acl_simple, [{server_cache, NewMap}]),
                                            #{<<"result">> => <<"ok">>}
                                    end;
                                false ->
                                    error_map("roles are not real")
                            end;
                        error ->
                            error_map("user '" ++ binary_to_list(UserName) ++ "' is not exist")
                    end
            end,
    {reply, Reply, []};
handle_call({roles_delete, UserName, Roles}, _From, _State) ->
    Reply = case get_map_from_db() of
                no_connect ->
                    error_map("no connect with db");
                {error, _} ->
                    error_map("error in reqare db");
                Map ->
                    case maps:find(UserName, Map) of
                        {ok, OldRoles} ->
                            case is_real_roles(Roles) of
                                true ->
                                    case handler_roles_delete(Roles, UserName) of
                                        {error, _} ->
                                            error_map("error in reqare db");
                                        ok ->
                                            NewMap = Map#{UserName := OldRoles -- Roles},
                                            true = ets:insert(acl_simple, [{server_cache, NewMap}]),
                                            #{<<"result">> => <<"ok">>}
                                    end;
                                false ->
                                    error_map("roles are not real")
                            end;
                        error ->
                            error_map("user '" ++ binary_to_list(UserName) ++ "' is not exist")
                    end
            end,
    {reply, Reply, []};
handle_call({show_roles, UserName}, _From, _State) ->
    [{_, Map}] = ets:lookup(acl_simple, server_cache),
    Reply = case maps:find(UserName, Map) of
                {ok, List} ->
                    #{
                        <<"result">> => <<"ok">>,
                        <<"user">> => UserName,
                        <<"roles">> => List
                    };
                error ->
                    error_map("user '" ++ binary_to_list(UserName) ++ "' is not exist")
            end,
    {reply, Reply, []};
handle_call({show_allow_roles}, _From, State) ->
    Reply = #{
        <<"result">> => <<"ok">>,
        <<"roles">> => ?ROLES
    },
    {reply, Reply, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
    ?LOG_INFO("unknown_req(handle_call): ~p ", [Msg]),
    {reply, unknown_req, State}.
handle_cast(Msg, State) ->
    ?LOG_INFO("unknown_req(handle_cast): ~p ", [Msg]),
    {noreply, State}.
handle_info({update_cache, NewState}, _State) ->
    {noreply, NewState};
handle_info(Msg, State) ->
    ?LOG_INFO("unknown_req(handle_info): ~p ", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ====================================================
% Help-functions for inverse functions
% ====================================================

is_real_roles([]) -> true;
is_real_roles([H | T]) ->
    case lists:member(H, ?ROLES) of
        true -> is_real_roles(T);
        false -> false
    end.

% -----------------

handler_roles_delete([], _) -> ok;
handler_roles_delete([H | T], Name) ->
    case acl_simple_pg:delete("roles_delete_by_name", [Name, H]) of
        {ok, _} ->
            handler_roles_delete(T, Name);
        {error, Error} ->
            {error, Error}
    end
.

handler_roles_add(_, _RolesOld, []) -> [];
handler_roles_add(UserName, RolesOld, [H | T]) ->
    case lists:member(H, RolesOld) of
        false ->
            case acl_simple_pg:insert("roles_add_by_name", [UserName, H]) of
                {error, _} -> [error];
                {ok, _} -> [H | handler_roles_add(UserName, RolesOld, T)]
            end;
        true ->
            handler_roles_add(UserName, RolesOld, T)
    end.

% -----------------

get_map_from_db() ->
    case acl_simple_pg:select("get_all_users", []) of
        no_connect -> no_connect;
        {ok, _, Users} ->
            Map = convert_to_map(Users, #{}),
            Map;
        {error, Error} -> {error, Error}
    end.

convert_to_map([], Map) -> Map;
convert_to_map([{Name} | T], Map0) ->
    {ok, _, RolesList_Dirty} = acl_simple_pg:select("get_roles_by_name", [Name]),
    RolesList = handler_convert_to_map(RolesList_Dirty),
    Map = Map0#{Name => RolesList},
    convert_to_map(T, Map).

handler_convert_to_map([]) -> [];
handler_convert_to_map([{Role} | T]) ->
    [Role | handler_convert_to_map(T)].

% -----------------

error_map(Disc) ->
    #{
        <<"result">> => <<"error">>,
        <<"discription">> => list_to_binary(Disc)
    }.

