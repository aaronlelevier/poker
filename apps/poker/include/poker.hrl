%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 30. Mar 2021 5:06 AM
%%%-------------------------------------------------------------------
-author("Aaron Lelevier").

-type card() :: binary().

-type cards() :: [card()].

-record(hand, {
  % enum - one of the possible hands in poker. i.e. flush, full_house, two_pair, ...
  rank,
  % the cards that make up the 'rank' only, so for a 'pair' this is 2 cards
  rank_cards,
  % the 5 best cards that make up the poker hand
  cards,
  % up to the 7 max cards available if NL Holdem for example that the player
  % has available to them
  all_cards
}).

-record(card, {
  % initial value of the card, i.e. <<"As">>
  value,
  % card rank, i.e. <<"A">>
  rank,
  % integer card rank, i.e. an <<"2">> is 2
  % special case is that <<"A">> can be a 1 or 14, to support wheel straight
  int_rank,
  % card suit, i.e. <<"s">>
  suit
}).