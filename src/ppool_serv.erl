-module(ppool_serv).
-behaviour(gen_server).

%% API
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SPEC(MFA),
        {worker_sup,
          {ppool_worker_sup, start_link, [MFA]},
          temporary,
          10000,
          supervisor,
          [ppool_worker_sup]}).

-record(state, {limit=0,
                sup,
                refs,  % keep all the monitor references in memory
                queue=queue:new()}).

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

%% running a task in the pool and telling you it can't
%% be started if the pool if full
run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

%% running a task in the pool if there's place, otherwise keep
%% the calling process waiting while the task is in the queue,
%% until it can be run
sync_queue(Name, Args) ->
  gen_server:call(Name, {sync, Args}, infinity).

%% running a task asynchronously in the pool, as soon as possible.
%% if no place is available, queue it up and run it whenever
async_queue(Name, Args) ->
  gen_server:cast(Name, {async, Args}).

stop(Name) ->
  gen_server:call(Name, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Limit, MFA, Sup}) ->
  self() ! {start_worker_supervisor, Sup, MFA},
  {ok, #state{limit = Limit, refs=gb_sets:empty()}}.

handle_call({run, Args}, _From, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};
handle_call({run, Args}, _From, S = #state{limit = N}) when N =< 0 ->
  {reply, noalloc, S};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
  link(Pid),
  {noreply, S#state{sup = Pid}};
handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
