-module(pq_supersup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_player/2, stop_player/1]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% using a simple-one-for-one strategy because we
%% get to have many supervisees of the same type
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 60000,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = infinity,
  Type = supervisor,

  AChild = {sup, {pq_sup, start_link, []},
    Restart, Shutdown, Type, [pq_sup]},

  {ok, {SupFlags, [AChild]}}.

%% starts an individual player
start_player(Name, Info) ->
  supervisor:start_child(?MODULE, [Name, Info]).

%% stops a player
stop_player(Name) ->
  supervisor:terminate_child(?MODULE, regis:whereis(Name)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
