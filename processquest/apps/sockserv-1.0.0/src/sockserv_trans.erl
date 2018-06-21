-module(sockserv_trans).

%% API
-export([to_str/1]).


%% the player killed something
to_str({_User, killed, Time, {EnemyName, Props}}) ->
  {Drop, _} = proplists:get_value(drop, Props),
  [
    ["Executing a ", EnemyName, "..."],
    {wait, Time},  % take a pause between the output values
    ["Obtained ", Drop, "."]
  ];
%% changing locations
to_str({_Name, heading, _Time, Loc}) ->
  [
    [
      "Heading to ",
      case Loc of
        market -> "the marketplace to sell loot...";
        killing -> "the killing fields..."
      end
    ]
  ];
%% leveling up
to_str({_Name, lvl_up, _, NewStats, NewLvl, _NewExp}) ->
  [
    [
      "Leveled up to level ", integer_to_list(NewLvl),
      " Here are your new stats:", $\n,
      io_lib:format(
        "  Charisma: ~B~n"
        "  Constitution: ~B~n"
        "  Dexterity: ~B~n"
        "  Intelligence: ~B~n"
        "  Strength: ~B~n"
        "  Wisdom: ~B~n~n",
        [Points || {_, Points} <- lists:sort(NewStats)]
      )
    ]
  ];
%% bought an item
to_str({_Name, buy, Time, Slot, {Item, _, _, _}}) ->
  SlotTxt = case Slot of
              armor -> " armor";
              weapon -> "";
              helmet -> " helmet";
              shield -> " shield"
            end,
  [
    [
      "Negotiating purchase of better equipment...",
      {wait, Time},
      ["Bought a ", Item, SlotTxt]
    ]
  ];
%% sold an item
to_str({_Name, sell, Time, {Item, Val}}) ->
  [
    ["Selling ", Item],
    {wait, Time},
    ["Got ", integer_to_list(Val), " bucks."]
  ];
%% completed a quest
to_str({_Name, quest, 0, Completed, New}) ->
  [
    ["Completed quest: ", Completed, "..."],
    ["Obtained new quest: ", New, "."]
  ].
