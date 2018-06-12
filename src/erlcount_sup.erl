%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2018 11:24 AM
%%%-------------------------------------------------------------------
-module(erlcount_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 100,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = transient,
  Shutdown = 60000,
  Type = worker,

  AChild = {dispatch, {erlcount_dispatch, start_link, []},
    Restart, Shutdown, Type, [erlcount_dispatch]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
