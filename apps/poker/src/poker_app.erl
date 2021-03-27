%%%-------------------------------------------------------------------
%% @doc poker public API
%% @end
%%%-------------------------------------------------------------------

-module(poker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    poker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
