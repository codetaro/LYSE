%%%-------------------------------------------------------------------
%%% @doc
%%% The top-level supervisor of the registration
%%% @end
%%%-------------------------------------------------------------------
-module(regis_sup).
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
  {ok, {{one_for_one, 1, 3600},
    [{server,
      {regis_server, start_link, []},
      permanent,
      500,
      worker,
      [regis_server]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
