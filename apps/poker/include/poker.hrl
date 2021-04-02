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
  rank
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