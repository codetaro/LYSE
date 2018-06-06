-module(ppool_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(MFA = {_, _, _}) ->
  supervisor:start_link(?MODULE, MFA).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({M, F, A}) ->
  MaxRestart = 5,
  MaxTime = 3600,
  {ok, {{simple_one_for_one, MaxRestart, MaxTime},
    [{ppool_worker,
      {M, F, A},
      temporary, 5000, worker, [M]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
