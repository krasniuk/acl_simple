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
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    ok = validation_add_allow_roles(ListRoles, AllowRoles),
    Reply = add_allow_roles_handler(ListRoles),
    {reply, Reply, State};
handle_call({delete_allow_roles, ListRoles}, _From, State) ->
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    ok = validation_roles(ListRoles, AllowRoles),
    Reply = delete_allow_roles_handler(ListRoles),
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
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    case lists:member(Role, AllowRoles) of
        true ->
            add_allow_roles_handler(ListRoles);
        false ->
            {ok, _} = acl_simple_pg:insert("add_allow_role", [Role]),
            true = ets:insert(acl_simple, [{allow_roles, [Role|AllowRoles]}]),
            add_allow_roles_handler(ListRoles)
    end.

-spec delete_allow_roles_handler(list()) -> map().
delete_allow_roles_handler([]) ->
    ?JSON_OK;
delete_allow_roles_handler([Role|ListRoles]) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    UsersList = maps:keys(Cache),
    ok = delete_users_role(Role, Cache, UsersList),
    {ok, _} = acl_simple_pg:delete("delete_allow_role", [Role]),
    true = ets:insert(acl_simple, [{allow_roles, AllowRoles -- [Role]}]),
    delete_allow_roles_handler(ListRoles).

-spec user_add_handler(binary(), list()) -> map().
user_add_handler(User, PassHash) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    ok = validation_user_add(User, Cache),
    ok = validation_passhash(PassHash),
    Uuid = list_to_binary(uuid:to_string(simple, uuid:uuid1())),
    % binary_to_list(crypto:hash(sha, <<"1234">>)).
    {ok, _} = acl_simple_pg:insert("user_add", [Uuid, User, jsone:encode(PassHash)]),
    true = ets:insert(acl_simple, [{server_cache, Cache#{User => []}}]),
    ?JSON_OK.

-spec user_delete_handler(binary()) -> map().
user_delete_handler(User) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    ok = validation_user(User, Cache),
    #{User := RolesList} = Cache,
    ok = delete_roles_in_db(RolesList, User),
    {ok, _} = acl_simple_pg:delete("users_delete_by_name", [User]),
    NewCache = maps:remove(User, Cache),
    true = ets:insert(acl_simple, [{server_cache, NewCache}]),
    ?JSON_OK.

-spec roles_add_handler(binary(), list()) -> map().
roles_add_handler(User, Roles) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    ok = validation_user(User, Cache),
    ok = validation_roles(Roles, AllowRoles),
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
roles_delete_handler(User, Roles) ->
    [{_, Cache}] = ets:lookup(acl_simple, server_cache),
    [{_, AllowRoles}] = ets:lookup(acl_simple, allow_roles),
    ok = validation_user(User, Cache),
    ok = validation_roles(Roles, AllowRoles),
    #{User := OldRoles} = Cache,
    ok = delete_roles_in_db(Roles, User),
    NewCache = Cache#{User := OldRoles -- Roles},
    true = ets:insert(acl_simple, [{server_cache, NewCache}]),
    ?JSON_OK.


% ====================================================

-spec validation_add_allow_roles(list(), list()) -> ok.
validation_add_allow_roles([], _) ->
    ok;
validation_add_allow_roles([Role|RoleList], AllowRoles) ->
    LenRole = byte_size(Role),
    {match, [{0, LenRole}]} = re:run(Role, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    validation_add_allow_roles(RoleList, AllowRoles).

-spec validation_roles(list(), list()) -> ok.
validation_roles([], _) ->
    ok;
validation_roles([Role|RoleList], AllowRoles) ->
    LenRole = byte_size(Role),
    {match, [{0, LenRole}]} = re:run(Role, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    true = lists:member(Role, AllowRoles),
    validation_roles(RoleList, AllowRoles).

-spec validation_user_add(binary(), map()) -> ok.
validation_user_add(User, Cache) ->
    LenRole = byte_size(User),
    {match, [{0, LenRole}]} = re:run(User, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    error = maps:find(User, Cache),
    ok.

-spec validation_user(binary(), map()) -> ok.
validation_user(User, Cache) ->
    LenRole = byte_size(User),
    {match, [{0, LenRole}]} = re:run(User, "[a-zA-Z][a-zA-Z_\\d]*", [unicode]),
    {ok, _} = maps:find(User, Cache),
    ok.

-spec validation_passhash(list()) -> ok.
validation_passhash(PassHash) ->
    20 = length(PassHash),
    PassHash = lists:map(fun(Num) -> true = Num > 0, true = Num < 256, Num end, PassHash),
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

-spec delete_roles_in_db(list(), binary()) -> ok | {error, any()}.
delete_roles_in_db([], _) -> ok;
delete_roles_in_db([Role|T], User) ->
    case acl_simple_pg:delete("roles_delete_by_name", [User, Role]) of
        {ok, _} ->
            ?LOG_DEBUG("Role ~p was delete in user ~p", [Role, User]),
            delete_roles_in_db(T, User);
        {error, Error} ->
            ?LOG_ERROR("db error ~p", [Error]),
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
