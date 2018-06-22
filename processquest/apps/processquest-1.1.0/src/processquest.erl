-module(processquest).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% User interface
-export([start_player/2, stop_player/1, subscribe/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, []) ->
  pq_supersup:start_link().

stop(_State) ->
  ok.

%%%===================================================================
%%% User interface
%%%===================================================================

start_player(Name, Info) ->
  pq_supersup:start_player(Name, Info).

stop_player(Name) ->
  pq_supersup:stop_player(Name).

subscribe(Name, Handler, Args) ->
  pq_events:add_handler(Name, Handler, Args).