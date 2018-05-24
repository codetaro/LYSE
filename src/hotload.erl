-module(hotload).
-export([server/1, upgrade/1]).


server(State) ->
  receive
    update ->
      NewState = ?MODULE:upgrade(State),
      ?MODULE:server(NewState);  %% loop in the new version of the module
    SomeMessage ->
      server(State)  %% stay in the same version no matter what.
  end.

upgrade(OldState) ->
  {}.  %% transform and return the state here