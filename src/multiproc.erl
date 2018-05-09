-module(multiproc).
-export([important/0, normal/0, optimized/1]).


important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
  after 0 ->
    normal()
  end.

normal() ->
  receive
    {_, Message} ->
      [Message | normal()]
  after 0 ->
    []
  end.

%% optimized in R14A
optimized(Pid) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, hello},
  receive
    {Pid, Ref, Msg} ->
      io:format("~p~n", [Msg])
  end.