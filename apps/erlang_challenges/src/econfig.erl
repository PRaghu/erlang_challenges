-module(econfig).

%% API
-export([
	init/2,
	all/1,
	get/2,
	get/3,
	set/2,
	set/3
]).

-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type key()   :: atom().
-type value() :: term().
-type entry() :: {key(), value()} | list().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create an ets table and insert the entries
%% if the table is already present delete the table and
%% create new table with the entries.
-spec init(atom(), entry() | map()) -> ok.
init(Name, Entries) when is_map(Entries) ->
	init(Name, maps:to_list(Entries));
init(Name, Entries) when is_list(Entries) ->
	case ets:info(Name) of
		undefined ->
	      do_init(Name, Entries);
	    _ ->
	      true = ets:delete(Name),
	      do_init(Name, Entries)
	end.

%% @doc return all the records in the ets table.
-spec all(atom()) -> [entry()].
all(Name) ->
	MS = ets:fun2ms(fun({X, Y}) -> {X, Y} end),
	ets:select(Name, MS).

%% @doc return the record with particular key in the ets table
%% if key not found return undefined
-spec get(atom(), key()) -> value().
get(Name, Key) when is_atom(Key) ->
	get(Name, Key, undefined).

%% @doc return the record with particular key in the ets table
%% if key not found return the Default value passed
-spec get(atom(), key(), value()) -> value().
get(Name, Key, Default) when is_atom(Key) ->
	lookup_element(Name, Key, 2, Default).

%% @doc insert the records in the ets table.
-spec set(atom(), [entry()]) -> ok.
set(Name, Entries) when is_list(Entries) ->
	true = ets:insert(Name, Entries),
	ok.

%% @doc insert the record in the ets table.
-spec set(atom(), key(), value()) -> ok.
set(Name, Key, Value) ->
	set(Name, [{Key, Value}]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
do_init(Name, Entries) ->
	Name = ets:new(Name, [
		named_table,
		public,
		{read_concurrency, true}
  	]),
  	true = ets:insert(Name, Entries),
  	ok.

%% @private
lookup_element(Name, Key, Pos, Default) ->
	try
		ets:lookup_element(Name, Key, Pos)
	catch
		_:_ -> Default
	end.
