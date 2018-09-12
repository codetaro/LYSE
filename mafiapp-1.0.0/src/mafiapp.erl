-module(mafiapp).
-behaviour(application).

-record(mafiapp_friends, {name,
  contact = [],
  info = [],
  expertise}).
-record(mafiapp_services, {from,
  to,
  date,
  description}).
%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  case 'TopSupervisor':start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
