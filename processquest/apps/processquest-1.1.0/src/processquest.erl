%%%-------------------------------------------------------------------
%%% @doc
%%% ProcessQuest's main wrapping module.
%%% Start ProcessQuest by calling application:start(processquest).
%%% Create a player by calling processquest:start_player(Name, Info).
%%% - Name is any term to identify the player
%%% - Info is additional information to configure the player. Consult
%%%   the pq_player module for more info.
%%%
%%% You can subscribe to the player events by calling
%%% processquest:subscribe(Name, Handler, Args).
%%% The handler is a regular event handler. See test/pq_events_handler.erl.
%%% @end
%%%-------------------------------------------------------------------
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

%% Starts a player
start_player(Name, Info) ->
  pq_supersup:start_player(Name, Info).

%% Stops a player
stop_player(Name) ->
  pq_supersup:stop_player(Name).

%% Subscribe to user events
subscribe(Name, Handler, Args) ->
  pq_events:add_handler(Name, Handler, Args).