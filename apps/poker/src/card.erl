%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 27. Mar 2021 8:12 AM
%%%-------------------------------------------------------------------
-module(card).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).
-compile(export_all).


%% @doc Returns a Poker Hand
-spec hand(binary(), binary()) -> [binary()].
hand(Card1, Card2) ->
  [Card1, Card2].


%% @doc Returns the 'Card' rank
%% ex: Card <<"As">> returns <<"A">>
-spec rank(binary()) -> binary().
rank(Card) ->
  binary:part(Card, {0, 1}).


%% @doc Returns the integer value of the Card rank
-spec rank_value(binary()) -> integer().
rank_value(Card) ->
  maps:get(rank(Card), rank_value_map()).


%% @doc A mapping from the Card rank to integer values
-spec rank_value_map() -> #{string() => integer()}.
rank_value_map() ->
  #{
    <<"2">> => 2,
    <<"3">> => 3,
    <<"4">> => 4,
    <<"5">> => 5,
    <<"6">> => 6,
    <<"7">> => 7,
    <<"8">> => 8,
    <<"9">> => 9,
    <<"T">> => 10,
    <<"J">> => 11,
    <<"Q">> => 12,
    <<"K">> => 13,
    <<"A">> => 14
  }.


%% @doc Returns the 'Card' suit
%% ex: Card <<"As">> returns <<"s">>
-spec suit(binary()) -> binary().
suit(Card) ->
  binary:part(Card, {1, 1}).


%% @doc Returns a random 'Card' from the default list of all 'Cards'
-spec random_card() -> binary().
random_card() -> random_card(all()).


%% @doc Returns a random 'Card' from the list of 'Cards'
-spec random_card([binary()]) -> binary().
random_card(Cards) ->
  lists:nth(rand:uniform(length(Cards)), Cards).


%% @doc Returns all possible 'Card's
-spec all() -> [binary()].
all() -> [
  <<"As">>,
  <<"Ah">>,
  <<"Ad">>,
  <<"Ac">>,
  <<"2s">>,
  <<"2h">>,
  <<"2d">>,
  <<"2c">>,
  <<"3s">>,
  <<"3h">>,
  <<"3d">>,
  <<"3c">>,
  <<"4s">>,
  <<"4h">>,
  <<"4d">>,
  <<"4c">>,
  <<"5s">>,
  <<"5h">>,
  <<"5d">>,
  <<"5c">>,
  <<"6s">>,
  <<"6h">>,
  <<"6d">>,
  <<"6c">>,
  <<"7s">>,
  <<"7h">>,
  <<"7d">>,
  <<"7c">>,
  <<"8s">>,
  <<"8h">>,
  <<"8d">>,
  <<"8c">>,
  <<"9s">>,
  <<"9h">>,
  <<"9d">>,
  <<"9c">>,
  <<"Ts">>,
  <<"Th">>,
  <<"Td">>,
  <<"Tc">>,
  <<"Js">>,
  <<"Jh">>,
  <<"Jd">>,
  <<"Jc">>,
  <<"Qs">>,
  <<"Qh">>,
  <<"Qd">>,
  <<"Qc">>,
  <<"Ks">>,
  <<"Kh">>,
  <<"Kd">>,
  <<"Kc">>].
