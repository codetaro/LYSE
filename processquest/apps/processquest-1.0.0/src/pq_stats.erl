-module(pq_stats).

%% API
-export([initial_roll/0, roll/0]).

%% first roll, when setting the stats up for the first time
initial_roll() ->
  [
    {charisma, roll(3)},
    {constitution, roll(3)},
    {dexterity, roll(3)},
    {intelligence, roll(3)},
    {strength, roll(3)},
    {wisdom, roll(3)}
  ].

%% rolls a single die. used when leveling up
roll() -> roll(1).

%% rolls num 6-faced dice
roll(Num) ->
  lists:sum([rand:uniform(6) || _ <- lists:seq(1, Num)]).
