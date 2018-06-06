-module(ppool_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Name, Limit, MFA) ->
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({Name, Limit, MFA}) ->
  MaxRestart = 1,
  MaxTime = 3600,
  {ok, {{one_for_all, MaxRestart, MaxTime},
    [{serv,
      {ppool_serv, start_link, [Name, Limit, self(), MFA]},
      permanent,
      5000,  % shutdown time
      worker,
      [ppool_serv]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
