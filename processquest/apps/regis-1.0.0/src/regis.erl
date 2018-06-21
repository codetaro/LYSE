-module(regis).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([register/2, unregister/1, whereis/1, get_names/0]).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, []) ->
  regis_sup:start_link().

stop(_State) ->
  ok.

%%%===================================================================
%%% A wrapper around the two other modules
%%%===================================================================
register(Name, Pid) -> regis_server:register(Name, Pid).

unregister(Name) -> regis_server:unregister(Name).

whereis(Name) -> regis_server:whereis(Name).

get_names() -> regis_server:get_names().