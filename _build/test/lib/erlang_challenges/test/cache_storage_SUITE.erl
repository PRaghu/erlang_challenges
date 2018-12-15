-module(cache_storage_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  set_get/1
  ]).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%%%===================================================================
%%% Common Test
%%%===================================================================

-spec all() -> [atom()].
all() -> [set_get].

init_per_suite(Config) ->
  _ = application:ensure_all_started(erlang_challenges),
  #{expiration := Exp, gc_interval := Interval} = econfig:get(erlang_challenges, cache_storage),
  ok = econfig:set(erlang_challenges, cache_storage, #{expiration => 1, gc_interval => 2}),
  [{expiration, Exp}, {gc_interval, Interval} | Config].

end_per_suite(Config) ->
  Interval = ?config(gc_interval, Config),
  Exp      = ?config(expiration, Config),
  Value    = #{expiration => Interval, gc_interval => Exp},
  ok = econfig:set(erlang_challenges, cache_storage, Value).


%%%===================================================================
%%% Tests Cases
%%%===================================================================

-spec set_get(eutils_ct:config()) -> any().
set_get(_Config) ->
  undefined = cache_storage:get(one),

  %% Entry one
  value1 = cache_storage:set(one, value1),
  value1 = cache_storage:get(one),

  %% Entry two
  timer:sleep(1000),
  value2 = cache_storage:set(two, value2),
  value2 = cache_storage:get(two),

  undefined = cache_storage:get(one),
  value2 = cache_storage:get(two),

  timer:sleep(1000),
  %% cache_gc starts
  undefined = cache_storage:get(two).