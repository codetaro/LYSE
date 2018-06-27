%%%-------------------------------------------------------------------
%%% @doc
%%% Converts events from a player's event manager into a
%%% cast sent to the sockserv socket gen_server.
%%% @end
%%%-------------------------------------------------------------------
-module(sockserv_pq_events).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init(Parent) ->
  {ok, Parent}.

handle_event(Event, Pid) ->
  gen_server:cast(Pid, Event),
  {ok, Pid}.

handle_call(Request, Pid) ->
  Pid ! Request,
  {ok, ok, Pid}.

handle_info(E, Pid) ->
  Pid ! E,
  {ok, Pid}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, Pid, _Extra) ->
  {ok, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
