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
  init/1,
  rank/1
]).

%%%===================================================================
%% Hand
%%%===================================================================
-spec init([#card{}]) -> #hand{}.
init(Cards) ->
  #hand{
    rank = ranker:rank(Cards),
    rank_cards = ranker:rank_cards(Cards),
    cards = ranker:cards(Cards),
    all_cards = Cards
  }.

%%%===================================================================
%%% API
%%%===================================================================
rank(Hand = #hand{}) ->
  Hand#hand.rank.
