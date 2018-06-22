-module(sockserv).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, []) ->
  sockserv_sup:start_link().

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
