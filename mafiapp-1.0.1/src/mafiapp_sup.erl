-module(mafiapp_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

%%  This does absolutely nothing, only there to
%%  allow to wait for tables.

  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
