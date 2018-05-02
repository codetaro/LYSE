-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
%%-author("dyuan").
%%-compile([debug_info, export_all]).

add(A,B) ->
  A + B.

hello() ->
  io:format("Hello, world!~n").

greet_and_add_two(X) ->
  hello(),
  add(X,2).