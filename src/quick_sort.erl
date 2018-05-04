-module(quick_sort).
-export([]).
-compile([export_all]).


qsort([]) -> [];
qsort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot,Rest,[],[]),
  qsort(Smaller) ++ [Pivot] ++ qsort(Larger).

partition(_,[],Smaller,Larger) -> {Smaller,Larger};
partition(Pivot,[H|T],Smaller,Larger) ->
  if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
     H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
  end.
