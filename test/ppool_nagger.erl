-module(ppool_nagger).
-behaviour(gen_server).

%% API
-export([start_link/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Task, Delay, Max, SendTo) ->
  gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Task, Delay, Max, SendTo}) ->
  {ok, {Task, Delay, Max, SendTo}, Delay}.
%% Delay is passed as a third element of the tuple
%% means timeout will be sent to handle_info after
%% [Delay] milliseconds

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, {Task, Delay, Max, SendTo}) ->
  SendTo ! {self(), Task},
  if Max =:= infinity ->
    {noreply, {Task, Delay, Max, SendTo}, Delay};
    Max =< 1 ->
      {stop, normal, {Task, Delay, 0, SendTo}};
    Max > 1 ->
      {noreply, {Task, Delay, Max - 1, SendTo}, Delay}
  end.
%%handle_info(_Msg, State) ->
%%  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
