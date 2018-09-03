-module(m8ball_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 5000,
  Type = worker,

  AChild = {m8ball, {m8ball_server, start_link, []},
    Restart, Shutdown, Type, [m8ball_server]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
