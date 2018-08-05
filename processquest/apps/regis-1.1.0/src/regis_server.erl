%%%-------------------------------------------------------------------
%%% @doc
%%% The core of the app: the server in charge of tracking processes.
%%% @end
%%%-------------------------------------------------------------------
-module(regis_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, register/2, unregister/1, whereis/1,
         get_names/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% We have two indexes: one by name and one by pid, for
%% MAXIMUM SPEED (not actually measured).
-record(state, {pid, name}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

%% Give a name to a process
register(Name, Pid) when is_pid(Pid) ->
  gen_server:call(?MODULE, {register, Name, Pid}).

%% Remove the name from a process
unregister(Name) ->
  gen_server:call(?MODULE, {unregister, Name}).

%% Find the pid associated with a process
whereis(Name) -> ok.

%% Find all the names currently registered.
get_names() -> ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
  {ok, ?MODULE}.

handle_call({register, Name, Pid}, _From, Tid) ->
  %%  Neither the name or the pid can already be in the table
  %%  so we match for both of them in a table-long scan using this.
  MatchSpec = ets:fun2ms(fun({N, P, _Ref}) when N == Name; P == Pid -> {N, P} end),
  case ets:select(Tid, MatchSpec) of
    [] -> % free to insert
      Ref = erlang:monitor(process, Pid),
      ets:insert(Tid, {Name, Pid, Ref}),
      {reply, ok, Tid};
    [{Name, _} | _] -> % maybe more then one result, but Name matches
      {reply, {error, name_taken}, Tid};
    [{_, Pid} | _] -> % maybe more then one result, but Pid matches
      {reply, {error, already_named}, Tid}
  end;
handle_call({unregister, Name}, _From, S = #state{pid=P, name=N}) ->
  case gb_trees:lookup(Name, N) of
    {value, {Pid,Ref}} ->
      erlang:demonitor(Ref, [flush]),
      {reply, ok, S#state{pid=gb_trees:delete(Pid, P),
        name=gb_trees:delete(Name, N)}};
    none ->
      {reply, ok, S}
  end;
handle_call({whereis, Name}, _From, S = #state{name=N}) ->
  case gb_trees:lookup(Name, N) of
    {value, {Pid,_}} ->
      {reply, Pid, S};
    none ->
      {reply, undefined, S}
  end;
handle_call(get_names, _From, S = #state{name=N}) ->
  {reply, gb_trees:keys(N), S};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, S = #state{pid=P,name=N}) ->
  {value, {Name, Ref}} = gb_trees:lookup(Pid, P),
  {noreply, S#state{pid = gb_trees:delete(Pid, P),
                    name = gb_trees:delete(Name, N)}};
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

