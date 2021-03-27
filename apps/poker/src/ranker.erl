%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 27. Mar 2021 10:14 AM
%%%-------------------------------------------------------------------
-module(ranker).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).
-compile(export_all).


%% @doc Returns the hand name, and the best 5 cards from the
%% list that make up the Hand
rank(Cards) -> Cards.


-spec has_flush(binary()) -> boolean().
has_flush(Cards) ->
  M = #{
    "h" => 0, "d" => 0, "c" => 0, "s" => 0
  },
  has_flush(Cards, M).

has_flush([], M) ->
  length(lists:filter(fun({_K,V}) -> V == 5 end, maps:to_list(M))) > 0;
has_flush([Card|T], M) ->
  Suit = binary_to_list(card:suit(Card)),
  has_flush(T, M#{Suit := maps:get(Suit, M) + 1}).