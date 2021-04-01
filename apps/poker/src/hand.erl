%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 01. Apr 2021 7:59 AM
%%%-------------------------------------------------------------------
-module(hand).
-author("Aaron Lelevier").
-vsn(1.0).
-include("poker.hrl").
-export([
  rank/1
]).

rank(Hand = #hand{}) ->
  Hand#hand.rank.
