%%%-------------------------------------------------------------------
%%% @doc
%%% the application callback module
%%% @end
%%% Created : 12. Jun 2018 11:23 AM
%%%-------------------------------------------------------------------
-module(erlcount).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, _Args) ->
  erlcount_sup:start_link().

%% gets called after the application has terminated
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
