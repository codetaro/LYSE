%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2018 6:19 PM
%%%-------------------------------------------------------------------
-module(erlcount_counter).
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {dispatcher, ref, file, re}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(DispatcherPid, Ref, FileName, Regex) ->
  gen_server:start_link(?MODULE, [DispatcherPid, Ref, FileName, Regex], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([DispatcherPid, Ref, FileName, Regex]) ->
  self() ! start,  % order ourselves to start
  {ok, #state{dispatcher = DispatcherPid,
    ref = Ref,
    file = FileName,
    re = Regex}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(start, S = #state{re = Re, ref = Ref}) ->
  {ok, Bin} = file:read_file(S#state.file),
  Count = erlcount_lib:regex_count(Re, Bin),
  erlcount_dispatch:complete(S#state.dispatcher, Re, Ref, Count),
  {stop, normal, S}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
