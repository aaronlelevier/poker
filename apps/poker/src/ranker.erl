%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% # References
%%% Hand rankings: https://www.cardplayer.com/rules-of-poker/hand-rankings
%%% @end
%%% Created : 27. Mar 2021 10:14 AM
%%%-------------------------------------------------------------------
-module(ranker).
-author("Aaron Lelevier").
-vsn(1.0).
-include("poker.hrl").
-export([]).
-compile(export_all).


%% @doc Returns the hand name, and the best 5 cards from the
%% list that make up the Hand
rank(Cards) -> Cards.


-spec has_4_of_a_kind(cards()) -> boolean().
has_4_of_a_kind(Cards) ->
  Sorted = desc_rank_values(Cards),
  has_N_of_a_kind(Sorted, 4, length(Sorted)).


-spec has_3_of_a_kind(cards()) -> boolean().
has_3_of_a_kind(Cards) ->
  Sorted = desc_rank_values(Cards),
  has_N_of_a_kind(Sorted, 3, length(Sorted)).


-spec has_two_pair(cards()) -> boolean().
has_two_pair(Cards) ->
  Sorted = desc_rank_values(Cards),
  has_two_pair(Sorted, []).

has_two_pair(L, Acc) when length(L) == 1 ->
  lists:all(fun(X) -> X == true end, Acc) andalso length(Acc) == 2;
has_two_pair([A,B|T], Acc) ->
  case A == B of
    true ->
      has_two_pair(T, [true|Acc]);
    false ->
      has_two_pair([B|T], Acc)
  end.


-spec has_pair(cards()) -> boolean().
has_pair(Cards) ->
  Sorted = desc_rank_values(Cards),
  has_N_of_a_kind(Sorted, 2, length(Sorted)).


-spec has_flush(cards()) -> boolean().
has_flush(Cards) ->
  M = #{
    <<"h">> => 0,
    <<"d">> => 0,
    <<"c">> => 0,
    <<"s">> => 0
  },
  has_flush(Cards, M).

has_flush([], M) ->
  length(lists:filter(fun({_K, V}) -> V == 5 end, maps:to_list(M))) > 0;
has_flush([Card | T], M) ->
  Suit = card:suit(Card),
  has_flush(T, M#{Suit := maps:get(Suit, M) + 1}).


-spec has_straight(cards()) -> boolean().
has_straight(Cards) ->
  Sorted = desc_rank_values(Cards),
  has_straight(Sorted, length(Sorted)).

has_straight(_, 4) ->
  false;
has_straight(L, Size) ->
  case lists:nth(Size, L) - lists:nth(Size - 4, L) == -4 of
    true ->
      true;
    _ ->
      has_straight(L, Size - 1)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
desc_rank_values(Cards) ->
  RankValues = [card:rank_value(C) || C <- Cards],
  lists:reverse(lists:sort(RankValues)).


%% @doc Returns bool if 'N' consecutive values in a sorted list 'L' are the same
has_N_of_a_kind(_L, N, X) when N - 1 == X ->
  false;
has_N_of_a_kind(L0, N, Size) ->
  L = lists:sublist(L0, 1, N),
  H = lists:nth(1, L),
  IsMatch = lists:all(fun(X) -> X =:= H end, L),
  lager:debug("L:~w L0:~w, N:~p, Size:~p IsMatch:~p", [L, L0, N, Size-1, IsMatch]),
  case IsMatch of
    true ->
      true;
    _ ->
      L2 = lists:sublist(L0, 2, Size),
      lager:debug("L:~w, L2:~w, N:~p, Size:~p", [L0, L2, N, Size-1]),
      has_N_of_a_kind(lists:sublist(L0, 2, Size), N, Size - 1)
  end.
