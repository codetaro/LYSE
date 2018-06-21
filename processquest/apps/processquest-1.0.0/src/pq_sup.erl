-module(pq_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Name, Info) ->
  supervisor:start_link(?MODULE, {Name, Info}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% It is important that pq_events is started before
%% pq_player, otherwise we might create race conditions
%% when starting a player and then quickly generating events to
%% an event manager that doesn't exist.
init({Name, Info}) ->
  RestartStrategy = one_for_all,
  MaxRestarts = 2,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  AChild = {events, {pq_events, start_link, [Name]},
    permanent, 5000, worker, [dynamic]},

  BChild = {player, {pq_player, start_link, [Name, Info]},
    permanent, 2000, worker, [pq_player]},

  {ok, {SupFlags, [AChild, BChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
