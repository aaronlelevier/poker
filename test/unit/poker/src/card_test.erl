%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 02. Apr 2021 5:56 AM
%%%-------------------------------------------------------------------
-module(card_test).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

init_test() ->
  ?assertEqual(
    {card, <<"Ts">>, <<"T">>, [10], <<"s">>},
    card:init(<<"Ts">>)
  ).

init_ace_int_rank_is_a_list_test() ->
  ?assertEqual(
    {card, <<"As">>, <<"A">>, [1, 14], <<"s">>},
    card:init(<<"As">>)
  ).

rank_test() ->
  C = <<"Ks">>,
  ?assertEqual(<<"K">>, card:rank(C)).
