-module(kitty_gen_server).
-behaviour(gen_server).
-record(cat, {name, color = green, description}).
-compile(export_all).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).

%% Asynchronous call
return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
  gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->  %% work with synchronous messages
  if Cats =:= [] ->
    {reply, make_cat(Name, Color, Description), Cats};
    Cats =/= [] ->
      {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->  %% handle asynchronous calls
  {noreply, [Cat | Cats]}.

handle_info(Msg, Cats) ->  %% deal with messages that do not fit our interface
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name = Name, color = Col, description = Desc}.
