-module(my_behaviour).
-export([behaviour_info/1]).


%% init/1, some_fun/0 and other/3 are now expected callbacks
behaviour_info(callbacks) -> [{init,1},{some_fun,0},{other,3}];
behaviour_info(_) -> undefined.