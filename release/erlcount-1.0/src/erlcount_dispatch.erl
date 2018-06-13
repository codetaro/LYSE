%%%-------------------------------------------------------------------
%%% @doc
%%% a single server in charge of browsing the directories,
%%% asking ppool to schedule workers and compiling the results
%%% @end
%%% Created : 12. Jun 2018 3:52 PM
%%%-------------------------------------------------------------------
-module(erlcount_dispatch).
-behaviour(gen_fsm).

%% API
-export([start_link/0, complete/4]).

%% gen_fsm callbacks
-export([init/1,
  dispatching/2,
  listening/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(POOL, erlcount).

-record(data, {regex = [], refs = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
  gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
  %% move the get_env stuff to the supervisor's init
  {ok, Re} = application:get_env(regex),
  {ok, Dir} = application:get_env(directory),
  {ok, MaxFiles} = application:get_env(max_files),
  ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
  case lists:all(fun valid_regex/1, Re) of
    true ->
      self() ! {start, Dir},
      {ok, dispatching, #data{regex = [{R, 0} || R <- Re]}};
    false ->
      {stop, invalid_regex}
  end.

dispatching({continue, File, Continuation}, Data = #data{regex = Re, refs = Refs}) ->
  F = fun({Regex, _Count}, NewRefs) ->
    Ref = make_ref(),                                      % create a unique reference
    ppool:async_queue(?POOL, [self(), Ref, File, Regex]),  % schedule a ppool worker knows this reference
    [Ref | NewRefs]                                        % store this reference
      end,
  NewRefs = lists:foldl(F, Refs, Re),
  gen_fsm:send_event(self(), Continuation()),
  {next_state, dispatching, Data#data{refs = NewRefs}};
dispatching(done, Data) ->
  listening(done, Data).

listening(done, #data{regex = Re, refs = []}) ->           % all received, if no refs are left
  [io:format("Regex ~s has ~p results~n", [R, C]) || {R, C} <- Re],
  {stop, normal, done};
listening(done, Data) ->                                   % entries still missing
  {next_state, listening, Data}.

handle_event({complete, Regex, Ref, Count}, State, Data = #data{regex = Re, refs = Refs}) ->
  {Regex, OldCount} = lists:keyfind(Regex, 1, Re),                   % extract the old count of just completed regex
  NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}), % update the value with the new count
  NewData = Data#data{regex = NewRe, refs = Refs--[Ref]},            % update the data record while removing the Ref of the worker
  case State of                                                      % move to the next state
    dispatching ->
      {next_state, dispatching, NewData};
    listening ->
      listening(done, NewData)                                       % force to do result detection to make sure everything has been received
  end.

handle_sync_event(Event, _From, State, Data) ->
  io:format("Unexpected event: ~p~n", [Event]),
  {next_state, State, Data}.

handle_info({start, Dir}, State, Data) ->
  gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
  {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
  init:stop().

code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

valid_regex(Re) ->
  try re:run("", Re) of
    _ -> true
  catch
    error:badarg -> false
  end.