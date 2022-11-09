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
    ?LOG_INFO("-> timer_cache was created", []),
    true = ets:insert(acl_simple, [{server_cache, #{}}]),
    {ok, PauseTime} = application:get_env(acl_simple, timer_cache),
    State = erlang:send_after(1, self(), {timer_cache, PauseTime}),
    {ok, State}.

terminate(_, _State) ->
    ?LOG_INFO("-> timer_cache was dead", []),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_Data, _From, State) ->
    {reply, unknown_req, State}.
handle_cast(_Data, State) ->
    {noreply, State}.

handle_info({timer_cache, PauseTime}, State0) -> %State = OldTimers
    _l = erlang:cancel_timer(State0),
    NewTimer = case get_map_from_db() of
                   no_connect ->
                       erlang:send_after(200, self(), {timer_cache, PauseTime});
                   Map ->
                       true = ets:insert(acl_simple, [{server_cache, Map}]),
                       erlang:send_after(PauseTime, self(), {timer_cache, PauseTime})
               end,
    {noreply, NewTimer};
handle_info(_Data, State) ->
    {noreply, State}.


% ====================================================
% Help-functions for inverse functions
% ====================================================


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

handler_convert_to_map([]) -> [];
handler_convert_to_map([{Role} | T]) ->
    [Role | handler_convert_to_map(T)].
