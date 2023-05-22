-module(acl_simple_pg).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behavior(gen_server).

-include("acl_simple.hrl").

-export([start_link/1]). % Export for poolboy
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % Export for gen_server
-export([select/2, insert/2, delete/2]).

% ====================================================
% Clients functions
% ====================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

select(Statement, Args) ->
    case poolboy:checkout(pg_pool) of
        full ->
            ?LOG_ERROR("All workers are busy. Pool(~p)", [Pool]),
            error;
        WorkerPid ->
            gen_server:call(WorkerPid, {select, Statement, Args}),
            ok = poolboy:checkin(pg_pool, WorkerPid)
    end.

insert(Statement, Args) ->
    select(Statement, Args).

delete(Statement, Args) ->
    select(Statement, Args).


% ====================================================
% Inverse functions
% ====================================================

init(Args) ->
    self() ! connect,
    State = Args,
    {ok, State}.

terminate(_, _State) ->
    ok.

handle_call({insert, Statement, Args}, _From, State) ->
    Conn = proplists:get_value(connection, State),
    Reply = send_to_bd(Conn, Statement, Args),
    {reply, Reply, State};
handle_call({select, Statement, Args}, _From, State) ->
    Conn = proplists:get_value(connection, State),
    Reply = send_to_bd(Conn, Statement, Args),
    {reply, Reply, State};
handle_call({delete, Statement, Args}, _From, State) ->
    Conn = proplists:get_value(connection, State),
    Reply = send_to_bd(Conn, Statement, Args),
    {reply, Reply, State};
handle_call(Data, _From, State) ->
    ?LOG_INFO("-> (call) unknown_req: ~p", [Data]),
    {reply, {unknown_req, Data}, State}.

handle_cast(Data, State) ->
    ok = io:format("~n~w -> (cast) grt reqaure: ~p", [self(), Data]),
    {noreply, State}.

handle_info(connect, State) -> % initialization
    Arg = proplists:delete(connection, State),
    Conn = case epgsql:connect(Arg) of
               {ok, Pid} ->
                   ok = parse(Pid),
                   Pid;
               {error, _} ->
                   ok = timer:sleep(1000),
                   self() ! connect,
                   undefined
           end,
    {noreply, [{connection, Conn} | State]};
handle_info(_Data, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ====================================================
% Help-functions for inverse functions
% ====================================================

parse(Conn) ->
    ?LOG_DEBUG("Parse OK", []),
    {ok, _} = epgsql:parse(Conn, "get_allow_roles", "SELECT role FROM allow_roles", []),
    {ok, _} = epgsql:parse(Conn, "add_allow_role", "INSERT INTO allow_roles (role) VALUES ($1)", [varchar]),
    {ok, _} = epgsql:parse(Conn, "delete_allow_role", "DELETE FROM allow_roles WHERE role = $1", [varchar]),
    {ok, _} = epgsql:parse(Conn, "user_add", "INSERT INTO users (id, name, passhash) VALUES ($1, $2, $3)", [varchar, varchar, json]),
    {ok, _} = epgsql:parse(Conn, "get_passhash", "SELECT passhash FROM users WHERE name = $1", [varchar]),
    {ok, _} = epgsql:parse(Conn, "get_admin_passhash", "SELECT passhash FROM admins WHERE login = $1", [varchar]),
    {ok, _} = epgsql:parse(Conn, "roles_add_by_name", "INSERT INTO roles (user_id, role) VALUES ((SELECT id FROM users WHERE name = $1), $2)", [varchar, varchar]),
    {ok, _} = epgsql:parse(Conn, "get_all_users", "SELECT name FROM users", []),
    {ok, _} = epgsql:parse(Conn, "get_roles_by_name", "SELECT role FROM roles WHERE user_id = (SELECT id FROM users WHERE name = $1)", [varchar]),
    {ok, _} = epgsql:parse(Conn, "users_delete_by_name", "DELETE FROM users WHERE name = $1", [varchar]),
    {ok, _} = epgsql:parse(Conn, "roles_delete_by_name", "DELETE FROM roles WHERE user_id = (SELECT id FROM users WHERE name = $1) AND role = $2", [varchar, varchar]),
    ok.

send_to_bd(Conn, Statement, Args) -> % INTERFACE between prepared_query of DB, and handle_call...
    case epgsql:prepared_query(Conn, Statement, Args) of
        {error, Error} ->
            ?LOG_ERROR("PostgreSQL prepared_query error(~p): ~p~n", [Statement, Error]),
            {error, Error};
        Other -> Other
    end.
