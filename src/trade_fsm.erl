-module(trade_fsm).
-bahaviour(gen_fsm).
-record(state, {name = "",
  other,
  ownitems = [],
  otheritems = [],
  monitor,
  from}).

%% public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
  make_offer/2, retract_offer/2, ready/1, cancel/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
  terminate/3, code_change/4,
  % custom state names
  idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
  negotiate/3, wait/2, ready/2, ready/3]).


%%% public API
start(Name) ->
  erlang:error(not_implemented).

start_link(Name) ->
  erlang:error(not_implemented).

trade(OwnPid, OtherPid) ->        %% ask for a begin session. Returns when/if the other accepts
  erlang:error(not_implemented).

accept_trade(OwnPid) ->           %% accept someone's trade offer
  erlang:error(not_implemented).

make_offer(OwnPid, Item) ->       %% send an item on the table to be traded
  erlang:error(not_implemented).

retract_offer(OwnPid, Item) ->    %% cancel trade offer
  erlang:error(not_implemented).

ready(OwnPid) ->                  %% mention that you're ready for a trade. when the other player also declares being ready, the trade is done
  erlang:error(not_implemented).

cancel(OwnPid) ->                 %% cancel the transaction
  erlang:error(not_implemented).


%%% FSM to FSM functions
ask_negotiate(OtherPid, OwnPid) ->          %% ask the other FSM's Pid for a trade session
  gen_fsm:send_event(otherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->       %% forward the client message accepting the transaction
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->                 %% forward a client's offer
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->               %% forward a client's offer cancellation
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->                  %% ask the other side if he's ready to trade
  gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->                        %% reply that the side is not ready to trade (i.e. is not in 'wait' state)
  gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->                       %% tells the other fsm that the user is currently waiting for the ready state (state should transition to 'ready')
  gen_fsm:send_event(OtherPid, 'ready!').

negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiation, S#state{ownitems = add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S = #state{ownitems = OwnItems}) ->          %% own side retracting an item offer
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S = #state{otheritems = OtherItems}) ->           %% other side offering an item
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiation, S#state{otheritems = add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->         %% other side retracting an item
  notice(S, "other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};

negotiate(are_you_ready, S = #state{other = OtherPid}) ->
  io:format("Other user ready to trade.~n"),
  notice(S,
    "Other user ready to transfer goods:~n"
    "You get ~p, The other side gets ~p",
    [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

negotiate(ready, From, S = #state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting", []),
  {next_state, wait, S#state{from = From}};
negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, S}.

wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
wait({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "Other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}}.

wait(are_you_ready, S = #state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready, and I am. Waiting for same reply", []),
  {next_state, wait, S};
wait(not_yet, S = #state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};
wait('ready!', S = #state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from, ok),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->              %% don't care about these!
  unexpected(Event, wait),
  {next_state, wait, Data}.

%%%
ack_trans(OtherPid) ->            %% acknowledge that the fsm is in a ready state
  gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->           %% ask if ready to commit
  gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->            %% begin the synchronous commit
  gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
  gen_fsm:sync_send_all_state_event(OtherPid, cancel).


%%% gen_fsm callbacks
init(Name) ->
  {ok, idle, #state{name = Name}}.

idle({ask_negotiate, OtherPid}, S = #state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S = #state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->      %% the other side has accepted our offer. move to negotiate state
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, neogtiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S = #state{other = OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting negotiation", []),
  {reply, ok, negotiation, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

ready(ack, S = #state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S}
  end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

ready(ask_commit, _From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  notice(S, "committing...", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

commit(S = #state{}) ->
  io:format("Transaction completed for ~s. "
  "Items sent are:~n~p,~n received are:~n~p.~n"
  "This operation should have some atomic save "
  "in a database.~n",
    [S#state.name, S#state.ownitems, S#state.otheritems]).

handle_event(cancel, _StateName, S = #state{}) ->       %% the other player has sent this cancel event, stop whatever we're doing and shut down!
  notice(S, "received cancel event", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S = #state{}) ->   %% this cancel event comes from the client. we must warn the other player that we have a quitter!
  notify_cancel(S#state.other),
  notice(S, "cancelling trade, sending cancel event", []),
  {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->             %% do not reply to unexpected calls. let the call-maker crash!
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S = #state{other = Pid, monitor = Ref}) ->
  notice(S, "Other side dead", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.


%%%
code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(normal, ready, S = #state{}) ->
  notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
  ok.


%%% utility functions
notice(#state{name = N}, Str, Args) ->        %% send players a notice
  io:format("~s: " ++ Str ++ "~n", [N | Args]).

unexpected(Msg, State) ->                   %% unexpected allows to log unexpected messages
  io:format("~p received unknown event ~p while in state ~p~n",
    [self(), Msg, State]).

add(Item, Items) ->                         %% adds an item to an item list
  [Item | Items].

remove(Item, Items) ->                      %% remove an item from an item list
  Items -- [Item].

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

