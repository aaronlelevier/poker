%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 30. Mar 2021 4:13 AM
%%%-------------------------------------------------------------------
-module(ranker_test).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


has_4_of_a_kind_true_test() ->
  Cards = [
    <<"Ah">>,
    <<"Ad">>,
    <<"Ac">>,
    <<"As">>,
    <<"Qh">>
  ],
  ?assert(ranker:has_4_of_a_kind(Cards)).


has_4_of_a_kind_false_test() ->
  Cards = [
    <<"Ah">>,
    <<"Kd">>,
    <<"Ac">>,
    <<"As">>,
    <<"Qh">>
  ],
  ?assertNot(ranker:has_4_of_a_kind(Cards)).


has_full_house_true_test() ->
  % AAAQQ
  Cards = [
    <<"Ah">>,
    <<"Ad">>,
    <<"Ac">>,
    <<"2c">>,
    <<"Qs">>,
    <<"Qh">>,
    <<"3h">>
  ],
  ?assert(ranker:has_full_house(Cards)).


has_full_house_false_test() ->
  %% AAQQ only, no full house
  Cards = [
    <<"3h">>,
    <<"Kd">>,
    <<"Ac">>,
    <<"As">>,
    <<"Qh">>,
    <<"2h">>,
    <<"Qs">>
  ],
  ?assertNot(ranker:has_full_house(Cards)).


has_full_house_false_3_of_a_kind_only_test() ->
  %% AAA only, no full house
  Cards = [
    <<"3h">>,
    <<"Kd">>,
    <<"Ac">>,
    <<"As">>,
    <<"Ah">>,
    <<"2h">>,
    <<"Qs">>
  ],
  ?assertNot(ranker:has_full_house(Cards)).


has_3_of_a_kind_true_test() ->
  Cards = [
    <<"Ah">>,
    <<"Ad">>,
    <<"Ac">>
  ],
  ?assert(ranker:has_3_of_a_kind(Cards)).


has_3_of_a_kind_false_test() ->
  Cards = [
    <<"Ah">>,
    <<"Td">>,
    <<"Ac">>
  ],
  ?assertNot(ranker:has_3_of_a_kind(Cards)).


has_pair_true_test() ->
  Cards = [
    <<"Kh">>,
    <<"Td">>,
    <<"Ac">>,
    <<"Ac">>,
    <<"2c">>
  ],
  ?assert(ranker:has_pair(Cards)).


has_two_pair_true_test() ->
  Cards = [
    <<"Kh">>,
    <<"Th">>,
    <<"Kd">>,
    <<"Ac">>,
    <<"Ac">>,
    <<"2c">>
  ],
  ?assert(ranker:has_two_pair(Cards)).


has_two_pair_false_test() ->
  Cards = [
    <<"Kh">>,
    <<"Th">>,
    <<"Qd">>,
    <<"Ac">>, % pair but NO two pair
    <<"Ac">>,
    <<"2c">>
  ],
  ?assertNot(ranker:has_two_pair(Cards)).


has_pair_false_test() ->
  Cards = [
    <<"Kh">>,
    <<"Td">>,
    <<"Ac">>,
    <<"3c">>,
    <<"2c">>
  ],
  ?assertNot(ranker:has_pair(Cards)).


has_flush_true_test() ->
  Cards = [
    <<"Ah">>,
    <<"2h">>,
    <<"5h">>,
    <<"Kh">>,
    <<"Qh">>
  ],
  ?assert(ranker:has_flush(Cards)).


has_flush_false_test() ->
  Cards = [
    <<"Ah">>,
    <<"2d">>, % not a heart
    <<"5h">>,
    <<"Kh">>,
    <<"Qh">>
  ],
  ?assertNot(ranker:has_flush(Cards)).


has_straight_true_test() ->
  Cards = [
    <<"2h">>,
    <<"3h">>,
    <<"4h">>,
    <<"5h">>,
    <<"6h">>
  ],
  ?assert(ranker:has_straight(Cards)).


has_straight_false_test() ->
  Cards = [
    <<"2h">>,
    <<"3h">>,
    <<"4h">>,
    <<"5h">>,
    <<"7h">> % not consecutive
  ],
  ?assertNot(ranker:has_straight(Cards)).
