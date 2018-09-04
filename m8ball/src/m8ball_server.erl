-module(m8ball_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, ask/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call({global, ?MODULE}, stop).

ask(_Question) -> % the question doesn't matter!
  gen_server:call({global, ?MODULE}, question).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed(A, B, C),
  {ok, []}.

handle_call(question, _From, State) ->
  {ok, Answers} = application:get_env(m8ball, answers),
  Answer = element(random:uniform(tuple_size(Answers)), Answers),
  {reply, Answer, State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
