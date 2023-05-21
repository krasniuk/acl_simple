-module(acl_simple_server).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behavior(gen_server).

-include("acl_simple.hrl").

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

init([]) ->
    true = ets:insert(acl_simple, [{acl_simple_server, self()}]),
    {ok, []}.

terminate(_, _State) ->
    ok.

handle_call({user_add, UserName, PassHash}, _From, State) ->
    Reply = user_add_handler(UserName, PassHash),
    {reply, Reply, State};
handle_call({user_delete, UserName}, _From, State) ->
    Reply = user_delete_handler(UserName),
    {reply, Reply, State};
handle_call({show_all_users}, _From, State) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    Users = maps:keys(Cache),
    Reply = ?JSON_USERS(Users),
    {reply, Reply, State};
handle_call({roles_add, UserName, Roles}, _From, _State) ->
    Reply = roles_add_handler(UserName, Roles),
    {reply, Reply, []};
handle_call({roles_delete, UserName, Roles}, _From, State) ->
    Reply = roles_delete_handler(UserName, Roles),
    {reply, Reply, State};
handle_call({show_roles, UserName}, _From, _State) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    Reply = case maps:find(UserName, Cache) of
                {ok, Roles} ->
                    ?JSON_ROLES_OF_USER(UserName, Roles);
                error ->
                    ?JSON_ERROR("User '" ++ binary_to_list(UserName) ++ "' is not exist")
            end,
    {reply, Reply, []};
handle_call({show_allow_roles}, _From, State) ->
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    Reply = ?JSON_SHOW_ALLOW_ROLES(AllowRoles),
    {reply, Reply, State};
handle_call({add_allow_roles, ListRoles}, _From, State) ->
    ok = validation_add_allow_roles(ListRoles),
    Reply = add_allow_roles_handler(ListRoles),
    {reply, Reply, State};
handle_call({delete_allow_roles, ListRoles}, _From, State) ->
    ok = validation_roles(ListRoles),
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    Reply = delete_allow_roles_handler(ListRoles, Cache),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, unknown_req, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(terminate, State) ->
    {stop, normal, ok, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ====================================================
% Help-functions for inverse functions
% ====================================================

-spec add_allow_roles_handler(binary()) -> map().
add_allow_roles_handler([]) ->
    ?JSON_OK;
add_allow_roles_handler([Role|ListRoles]) ->
    {ok, _} = acl_simple_pg:insert("add_allow_role", [Role]),
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    true = ets:insert(acl_simple, [{allow_roles, [Role|AllowRoles]}]),
    add_allow_roles_handler(ListRoles).

-spec delete_allow_roles_handler(list(), list()) -> map().
delete_allow_roles_handler([], _) ->
    ?JSON_OK;
delete_allow_roles_handler([Role|ListRoles], Cache) ->
    UsersList = maps:keys(Cache),
    ok = delete_users_role(Role, Cache, UsersList),
    {ok, _} = acl_simple_pg:delete("delete_allow_role", [Role]),
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    true = ets:insert(acl_simple, [{allow_roles, AllowRoles -- [Role]}]),
    delete_allow_roles_handler(ListRoles, Cache).

-spec user_add_handler(binary(), list()) -> map().
user_add_handler(User, PassHash) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    ok = validation_user_add(User, Cache),
    Uuid = list_to_binary(uuid:to_string(simple, uuid:uuid1())),
    % binary_to_list(crypto:hash(sha, <<"1234">>)).
    {ok, _} = acl_simple_pg:insert("user_add", [Uuid, User, jsone:encode(PassHash)]),
    true = ets:insert(acl_simple, [{server_cache, Cache#{User => []}}]),
    ?JSON_OK.

-spec user_delete_handler(binary()) -> map().
user_delete_handler(User) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    ok = validation_user_delete(User, Cache),
    #{User := RolesList} = Cache,
    ok = delete_roles_in_db(RolesList, User),
    {ok, _} = acl_simple_pg:delete("users_delete_by_name", [User]),
    NewCache = maps:remove(User, Cache),
    true = ets:insert(acl_simple, [{server_cache, NewCache}]),
    ?JSON_OK.

-spec roles_add_handler(binary(), list()) -> map().
roles_add_handler(User, Roles) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    ok = validation_roles_add(User, Cache),
    ok = validation_roles(Roles),
    #{User := RolesOld} = Cache,
    RolesAdd = insert_roles_in_db(User, RolesOld, Roles),
    case lists:member(error, RolesAdd) of
        true ->
            RolesAdd1 = lists:delete(error, RolesAdd),
            NewMap = Cache#{User := RolesOld ++ RolesAdd1},
            true = ets:insert(acl_simple, [{server_cache, NewMap}]),
            ?JSON_ERROR("db error");
        false ->
            NewCache = Cache#{User := RolesOld ++ RolesAdd},
            true = ets:insert(acl_simple, [{server_cache, NewCache}]),
            ?JSON_OK
    end.

-spec roles_delete_handler(binary(), list()) -> map().
roles_delete_handler(UserName, Roles) ->
    case get_map_from_db() of
        no_connect ->
            ?JSON_ERROR("no connect with db");
        {error, Error} ->
            ?LOG_ERROR("Error in reqare db; ~p", [Error]),
            ?JSON_ERROR("Error in reqare db");
        Map ->
            case maps:find(UserName, Map) of
                error ->
                    ?JSON_ERROR("user '" ++ binary_to_list(UserName) ++ "' is not exist");
                {ok, OldRoles} ->
                    case is_real_roles(Roles) of
                        false ->
                            ?JSON_ERROR("roles are not real");
                        true ->
                            case delete_roles_in_db(Roles, UserName) of
                                {error, _} ->
                                    ?JSON_ERROR("error in reqare db");
                                ok ->
                                    NewMap = Map#{UserName := OldRoles -- Roles},
                                    true = ets:insert(acl_simple, [{server_cache, NewMap}]),
                                    ?JSON_OK
                            end
                    end
            end
    end.


% ====================================================

-spec validation_add_allow_roles(list()) -> ok.
validation_add_allow_roles([]) ->
    ok;
validation_add_allow_roles([Role|RoleList]) ->
    LenRole = byte_size(Role),
    {match, [{0, LenRole}]} = re:run(Role, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    false = is_real_roles([Role]),
    validation_add_allow_roles(RoleList).

-spec validation_roles(list()) -> ok.
validation_roles([]) ->
    ok;
validation_roles([Role|RoleList]) ->
    LenRole = byte_size(Role),
    {match, [{0, LenRole}]} = re:run(Role, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    true = is_real_roles([Role]),
    validation_roles(RoleList).

-spec validation_user_add(binary(), map()) -> ok.
validation_user_add(User, Cache) ->
    LenRole = byte_size(User),
    {match, [{0, LenRole}]} = re:run(User, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    error = maps:find(User, Cache),
    ok.

-spec validation_user_delete(binary(), map()) -> ok.
validation_user_delete(User, Cache) ->
    LenRole = byte_size(User),
    {match, [{0, LenRole}]} = re:run(User, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    {ok, _} = maps:find(User, Cache),
    ok.

-spec validation_roles_add(binary(), map()) -> ok.
validation_roles_add(User, Cache) ->
    LenUser = byte_size(User),
    {match, [{0, LenUser}]} = re:run(User, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    {ok, _} = maps:find(User, Cache),
    ok.




-spec delete_users_role(binary(), map(), list()) -> ok.
delete_users_role(_, _, []) ->
    ok;
delete_users_role(Role, Cache, [User|UsersList]) ->
    #{User := RoleList} = Cache,
    case lists:member(Role, RoleList) of
        false ->
            ok;
        true ->
            {ok, _} = acl_simple_pg:delete("roles_delete_by_name", [User, Role]),
            ?LOG_DEBUG("Role ~p was delete in user ~p", [Role, User]),
            NewMap = Cache#{User := RoleList -- [Role]},
            true = ets:insert(acl_simple, [{server_cache, NewMap}])
    end,
    delete_users_role(Role, Cache, UsersList).

-spec is_real_roles(list()) -> true | false.
is_real_roles(List) ->
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    is_real_roles(AllowRoles, List).

-spec is_real_roles(list(), list()) -> true | false.
is_real_roles(_, []) ->
    true;
is_real_roles(AllowRoles, [H|T]) ->
    case lists:member(H, AllowRoles) of
        true ->
            is_real_roles(AllowRoles, T);
        false ->
            false
    end.

-spec delete_roles_in_db(list(), binary()) -> ok | {error, any()}.
delete_roles_in_db([], _) -> ok;
delete_roles_in_db([H | T], Name) ->
    case acl_simple_pg:delete("roles_delete_by_name", [Name, H]) of
        {ok, _} ->
            delete_roles_in_db(T, Name);
        {error, Error} ->
            {error, Error}
    end.

-spec insert_roles_in_db(binary(), list(), list()) -> list().
insert_roles_in_db(_, _RolesOld, []) ->
    [];
insert_roles_in_db(User, RolesOld, [Role|T]) ->
    case lists:member(Role, RolesOld) of
        false ->
            case acl_simple_pg:insert("roles_add_by_name", [User, Role]) of
                {error, Error} ->
                    ?LOG_ERROR("db error ~p", [{error, Error}]),
                    [error];
                {ok, _} ->
                    ?LOG_DEBUG("Add role ~p for user ~p", [Role, User]),
                    [Role | insert_roles_in_db(User, RolesOld, T)]
            end;
        true ->
            insert_roles_in_db(User, RolesOld, T)
    end.


% -----------------

-spec get_map_from_db() -> no_connect | {error, any()} | map().
get_map_from_db() ->
    case acl_simple_pg:select("get_all_users", []) of
        no_connect ->
            no_connect;
        {ok, _, Users} ->
            convert_to_map(Users, #{});
        {error, Error} ->
            {error, Error}
    end.

-spec convert_to_map(list(), map()) -> map().
convert_to_map([], Map) -> Map;
convert_to_map([{Name} | T], Map0) ->
    {ok, _, RolesList_Dirty} = acl_simple_pg:select("get_roles_by_name", [Name]),
    RolesList = handler_convert_to_map(RolesList_Dirty),
    Map = Map0#{Name => RolesList},
    convert_to_map(T, Map).

-spec handler_convert_to_map(list()) -> list().
handler_convert_to_map([]) -> [];
handler_convert_to_map([{Role} | T]) ->
    [Role | handler_convert_to_map(T)].

