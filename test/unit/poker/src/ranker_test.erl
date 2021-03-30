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
