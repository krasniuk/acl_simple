-module(acl_simple_timer_cache).
-author('Mykhailo Krasniuk <miha.190901@gmail.com>').
-behavior(gen_server).

-include("acl_simple.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% ====================================================
% Users functions
% ====================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


% ====================================================
% Inverse functions
% ====================================================

init([]) ->
    {ok, PauseTime} = application:get_env(acl_simple, timer_cache),
    {ok, PauseAllowRoles} = application:get_env(acl_simple, timer_allow_roles),
    TCache = erlang:send_after(200, self(), {timer_cache, PauseTime}),
    TAllowRoles = erlang:send_after(200, self(), {timer_allow_roles, PauseAllowRoles}),
    {ok, #{timer_cache => TCache,
           timer_allow_roles => TAllowRoles}}.

terminate(_, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_Data, _From, State) ->
    {reply, unknown_req, State}.
handle_cast(_Data, State) ->
    {noreply, State}.

handle_info({timer_cache, PauseTime}, #{timer_cache := T} = State) ->
    _ = erlang:cancel_timer(T),
    Timer = case get_map_from_db() of
                no_connect ->
                    erlang:send_after(200, self(), {timer_cache, PauseTime});
                Map ->
                    true = ets:insert(acl_simple, [{server_cache, Map}]),
                    erlang:send_after(PauseTime, self(), {timer_cache, PauseTime})
            end,
    {noreply, State#{timer_cache := Timer}};
handle_info({timer_allow_roles, PauseTime}, #{timer_allow_roles := T} = State) ->
    _ = erlang:cancel_timer(T),
    Timer = case allow_roles_handler() of
                no_connect ->
                    erlang:send_after(200, self(), {timer_allow_roles, PauseTime});
                List ->
                    true = ets:insert(acl_simple, [{allow_roles, List}]),
                    erlang:send_after(PauseTime, self(), {timer_allow_roles, PauseTime})
            end,
    {noreply, State#{timer_allow_roles := Timer}};
handle_info(_Data, State) ->
    {noreply, State}.


% ====================================================
% Help-functions for inverse functions
% ====================================================

-spec allow_roles_handler() -> list().
allow_roles_handler() ->
    case acl_simple_pg:select("get_allow_roles", []) of
        no_connect ->
            no_connect;
        {ok, _, AllowRoles} ->
            ?LOG_INFO("AllowRoles = ~p", [AllowRoles]),
            handler_convert_to_map(AllowRoles)
    end.

get_map_from_db() ->
    case acl_simple_pg:select("get_all_users", []) of
        no_connect -> no_connect;
        {ok, _, Users} ->
            Map = convert_to_map(Users, #{}),
            Map
    end.

convert_to_map([], Map) -> Map;
convert_to_map([{Name} | T], Map0) ->
    {ok, _, RolesList_Dirty} = acl_simple_pg:select("get_roles_by_name", [Name]),
    RolesList = handler_convert_to_map(RolesList_Dirty),
    Map = Map0#{Name => RolesList},
    convert_to_map(T, Map).


%% ------------------------------------------

-spec handler_convert_to_map(list()) -> list().
handler_convert_to_map([]) -> [];
handler_convert_to_map([{Role} | T]) ->
    [Role | handler_convert_to_map(T)].
