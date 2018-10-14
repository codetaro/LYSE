-module(discrep1).

%% API
-export([run/0]).


run() -> some_op(5, you).

some_op(A, B) -> A + B.