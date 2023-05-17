-module(acl_simple_pg).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behavior(gen_server).

-include("acl_simple.hrl").

-export([select/2, insert/2, delete/2]).

% Export for poolboy
-export([start_link/1]).

% Export for gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% ====================================================
% Clients functions
% ====================================================

start_link(Args) -> % Exports for boolboy
    gen_server:start_link(?MODULE, Args, []).

select(Statement, Args) ->
    WorkerPid = get_worker_of_pool(pg_pool),
    Return = case check_connect(WorkerPid) of
                 no_connect ->
                     no_connect;
                 ok ->
                     gen_server:call(WorkerPid, {select, Statement, Args})
             end,
    ok = poolboy:checkin(pg_pool, WorkerPid),
    Return.

insert(Statement, Args) ->
    WorkerPid = get_worker_of_pool(pg_pool),
    Return = case check_connect(WorkerPid) of
                 no_connect -> no_connect;
                 ok ->
                     gen_server:call(WorkerPid, {insert, Statement, Args})
             end,
    ok = poolboy:checkin(pg_pool, WorkerPid),
    Return.

delete(Statement, Args) ->
    WorkerPid = get_worker_of_pool(pg_pool),
    Return = case check_connect(WorkerPid) of
                 no_connect -> no_connect;
                 ok ->
                     gen_server:call(WorkerPid, {delete, Statement, Args})
             end,
    ok = poolboy:checkin(pg_pool, WorkerPid),
    Return.

%------ help -------

check_connect(WorkerPid) ->
    WorkerPid ! {is_connect, self()},
    receive
        ok ->
            ok;
        no_connect ->
            ?LOG_ERROR("no connect with db", []),
            no_connect
    after 500 ->
        ?LOG_ERROR("no connect with db", []),
        no_connect
    end.


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

handle_info({is_connect, Pid}, State) -> % check connect on client size
    Conn = proplists:get_value(connection, State),
    case Conn of
        undefined -> Pid ! no_connect;
        _ -> Pid ! ok
    end,
    {noreply, State};
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

% ---- HANDLERS USER ----
get_worker_of_pool(Pool) ->
    ClonePid = poolboy:checkout(Pool),
    case ClonePid of
        full ->
            ?LOG_ERROR("All workers are busy. Pool(~p)", [Pool]),
            error;
        ClonePid ->
            ClonePid
    end.

