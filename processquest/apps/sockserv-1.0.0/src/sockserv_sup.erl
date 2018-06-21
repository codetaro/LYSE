-module(sockserv_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_socket() ->
  supervisor:start_child(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  {ok, Port} = application:get_env(port),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
  spawn_link(fun empty_listeners/0),

  RestartStrategy = simple_one_for_one,
  MaxRestarts = 60,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 1000,
  Type = worker,

  AChild = {socket, {sockserv_serv, start_link, [ListenSocket]},
    Restart, Shutdown, Type, [sockserv_serv]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
empty_listeners() ->
  [start_socket() || _ <- lists:seq(1, 20)],
  ok.