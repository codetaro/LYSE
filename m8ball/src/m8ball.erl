-module(m8ball).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Interface
-export([ask/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, []) ->
  m8ball_sup:start_link();
start({takeover, _OtherNode}, []) ->
  m8ball_sup:start_link().

stop(_State) ->
  ok.

%%%===================================================================
%%% Interface
%%%===================================================================

ask(Question) ->
  m8ball_server:ask(Question).