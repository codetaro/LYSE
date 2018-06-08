%%% start the supervisor of a pool when required

-module(ppool_supersup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_pool/3, stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% start the whole application
start_link() ->
  supervisor:start_link({local, ppool}, ?MODULE, []).

%% stop the whole application
%%stop() ->
%%  case whereis(ppool) of
%%    P when is_pid(P) ->
%%      exit(P, kill);
%%    _ -> ok
%%  end.

%% create a specific pool
start_pool(Name, Limit, MFA) ->
  ChildSpec = {Name,
    {ppool_sup, start_link, [Name, Limit, MFA]},
    permanent, 10500, supervisor, [ppool_sup]},
  supervisor:start_child(ppool, ChildSpec).

%% remove a specific pool
stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool, Name).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
