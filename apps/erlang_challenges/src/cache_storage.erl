-module(cache_storage).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  get/1,
  set/2
]).

%% gen_server call backs
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

%% Macros
-define(TABLE, cache_storage).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(any()) -> any().
get(Key) ->
  case ets:lookup(?TABLE, Key) of
    []      -> undefined;
    [Value] -> maybe_expired(Value)
  end.

-spec set(any(), any()) -> any().
set(Key, Value) ->
  #{expiration := Expiration} = econfig:get(erlang_challenges, cache_storage),
  TTL = erlang:system_time(second) + Expiration,
  true = ets:insert(?TABLE, {Key, Value, TTL}), %% TTL in seconds
  Value.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  #{gc_interval := Interval} = econfig:get(erlang_challenges, cache_storage),
  ok = create_table(),
  ok = schedule_to_flush_keys(Interval),
  {ok, #{interval => Interval}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(flush_keys, #{interval := Interval} = State) ->
  ok = flush_keys(),
  ok = schedule_to_flush_keys(Interval),
  {noreply, State};
handle_info(Message, State) ->
  ok = lager:error("Unhandled message ~p", [Message]),
  {noreply, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

create_table() ->
  ?TABLE = case ets:info(?TABLE, name) of
    undefined ->
      ets:new(?TABLE, [named_table, set, public]);
    ?TABLE ->
      ?TABLE
  end,
  ok.

schedule_to_flush_keys(Interval) ->
  _Ref = erlang:send_after(Interval * 1000, self(), flush_keys),
  ok.

flush_keys() ->
  Now = erlang:system_time(second),
  _ = ets:select_delete(?TABLE, [{{'$1', '$2', '$3'}, [{'=<', '$3', Now}], [true]}]),
  ok.

maybe_expired({Key, Value, TTL}) ->
  case TTL =< erlang:system_time(second) of
    true  ->
      true = ets:delete(?TABLE, Key),
      undefined;
    false ->
      Value
  end.
