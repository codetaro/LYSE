-module(event).
-export([loop/1, normalize/1]).
-record(state, {server,  %% rr(event,state). load record definition from module
                name="",
                to_go=0}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
    if Next =:= [] ->
      Server ! {done, S#state.name};
      Next =/= [] ->
        loop(S#state{to_go = Next})
    end
  end.

normalize(N) ->
  Limit = 49 * 24 * 60 * 60,  %% Erlang is limited to about 49 days in milliseconds
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, Delay) ->
  loop(#state{server=Server,
              name=EventName,
              to_go=normalize(Delay)}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.