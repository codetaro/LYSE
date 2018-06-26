%%%-------------------------------------------------------------------
%%% @doc
%%% The core of ProcessQuest -- the player FSM,
%%% acting for each of the players in the game.
%%%
%%% The FSM actually depends on no external events and only sends messages
%%% to itself to prompt state changes. This is somewhat unusual as far as
%%% gen_fsm usages go, but it's pretty useful when needing to work with
%%% a standalone process.
%%% @end
%%%-------------------------------------------------------------------
-module(pq_player).
-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, market/2, killing/2, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-record(state, {name, stats, exp=0, lvlexp=1000, lvl=1,
                equip=[], money=0, loot=[], bought=[], time=0}).


%%% Possible states & events
%%
%      sell  buy
%     /   | |  \
%     \   ^ ^  /
%      [market]<--,
%      |          |
% done buying     |
%      |     bag full
%      v         /
%  [killing fields]
%   /   V   V   |
%   \   /   |   |
%   kill    lvl up

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Opts) ->
  gen_fsm:start_link(?MODULE, {Name, Opts}, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init({Name, Opts}) ->
  %% Properly seeding stuff. If not doing this, the random module will
  %% seed it based on a slightly unique time value. However, when starting
  %% many processes at about the same time, the seeds can be very close
  %% and give barely random results. The crypto:rand_bytes/1 function
  %% allows for much better seeding.
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  %% The first event, to start the FSM
  gen_fsm:send_event(self(), kill),
  case regis:register(Name, self()) of
    {error, _} ->
      {stop, name_taken};
    ok ->
      %% Use proplists with default values to let the user configure
      %% all parts of the FSM's state by using the Opts proplist.
      S = #state{
        name=Name,
        stats=proplists:get_value(stats, Opts, pq_stats:initial_roll()),
        exp=proplists:get_value(exp, Opts, 0),
        lvlexp=proplists:get_value(lvlexp, Opts, 1000),
        lvl=proplists:get_value(lvl, Opts, 1),
        equip=proplists:get_value(equip, Opts, []),
        money=proplists:get_value(money, Opts, 0),
        loot=proplists:get_value(loot, Opts, []),
        bought=proplists:get_value(bought, Opts, []),
        time=proplists:get_value(time, Opts, 0)
      },
      {ok, market, S}
  end.

%% Done selling. Switch to the event where we head to the killing fields
market(sell, S = #state{loot=[]}) ->
  gen_fsm:send_event(self(), buy),
  {next_state, market, S};
%% Selling an Item we have looted to the market, for whatever value it has
market(sell, S = #state{loot=[H={_X,Val}|T], money=M}) ->
  pq_events:sell(S#state.name, H, S#state.time),
  gen_fsm:send_event(self(), sell),
  {next_state, market, S#state{loot=T, money=M+Val}};
%% When done selling, buy items with your money
market(buy, S = #state{equip=Equip, money=Money, bought=Bought}) ->
  %% we have slots of equipment. It's useless to buy the same
  %% kind of item time after time, so we must select one we haven't observed yet
  case next_slot(Equip, Bought) of
    undefined ->
      %% when no slot is found, go to the killing field
      gen_fsm:send_event(self(), kill),
      {next_state, market, S#state{bought=[]}};
    OldItem = {Slot, {_Name, Modifier, Lvl, _Price}} ->
      %% Replace the item by a slightly better one if possible.
      case pq_market:Slot(Modifier+Lvl, Money) of
        undefined ->
          market(buy, S#state{bought=[Slot|Bought]});
        NewItem = {_, _, _, Price} ->
          pq_events:buy(S#state.name, Slot, NewItem, S#state.time),
          gen_fsm:send_event(self(), buy),
          NewEquip = [{Slot, NewItem} | Equip -- [OldItem]],
          {next_state, market, S#state{equip=NewEquip,
            money=Money-Price,
            bought=[Slot|Bought]}}
      end
  end;
market(_Event, State) ->
  {next_state, state_name, State}.

%% Killing an enemy on the killing field. Taking its drop and keeping it
%% in our loot.
killing(kill, S = #state{loot=Loot, stats=Stats, exp=Exp, lvlexp=LvlExp}) ->
  MaxSize = proplists:get_value(strength, Stats)*2,
  {EnemyName, Props} = pq_enemy:fetch(),
  pq_events:killed(S#state.name, {EnemyName, Props}, S#state.time),
  Drop = {_N, _V} = proplists:get_value(drop, Props),
  KillExp = proplists:get_value(experience, Props),
  NewLoot = [Drop|Loot],
  if length(NewLoot) =:= MaxSize ->
    gen_fsm:send_event(self(), market);
    Exp+KillExp >= LvlExp ->
      gen_fsm:send_event(self(), lvl_up);
    true ->
      gen_fsm:send_event(self(), kill)
  end,
  {next_state, killing, S#state{loot=NewLoot, exp=Exp+KillExp}};
%% If we just leveled up, the stats get updated before we keep killing.
killing(lvl_up, S = #state{stats=Stats, lvl=Lvl, lvlexp=LvlExp}) ->
  NewStats = [
    {charisma, proplists:get_value(charisma, Stats)+pq_stats:roll()},
    {constitution, proplists:get_value(constitution, Stats)+pq_stats:roll()},
    {dexterity, proplists:get_value(dexterity, Stats)+pq_stats:roll()},
    {intelligence, proplists:get_value(intelligence, Stats)+pq_stats:roll()},
    {strength, proplists:get_value(strength, Stats)+pq_stats:roll()},
    {wisdom, proplists:get_value(wisdom, Stats)+pq_stats:roll()}],
  gen_fsm:send_event(self(), kill),
  pq_events:lvl_up(S#state.name, NewStats, Lvl+1, LvlExp*2, S#state.time),
  {next_state, killing, S#state{stats=NewStats, lvl=Lvl+1, lvlexp=LvlExp*2}};
%% Heading to the market state transition
killing(market, S) ->
  pq_events:location(S#state.name, market, S#state.time),
  gen_fsm:send_event(self(), sell),
  {next_state, market, S}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Picks a slot based on what has been seen so far, combined with the
%% current weakest item.
next_slot(Equip, Bought) ->
  L = expand(Equip),
  case lists:sort([{Mod+Lvl, Entry} || Entry = {Slot, {_, Mod, Lvl, _}} <- L,
    not lists:member(Slot, Bought)]) of
    [] -> undefined;
    [{_, Entry}|_] -> Entry
  end.

expand(L) ->
  [
    expand_field(armor, L),
    expand_field(helmet, L),
    expand_field(shield, L),
    expand_field(weapon, L)].

expand_field(F, L) ->
  {F, proplists:get_value(F, L, {undefined,0,0,0})}.