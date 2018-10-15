%%%-------------------------------------------------------------------
%% @doc erlang_challenges public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_challenges_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlang_challenges_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
