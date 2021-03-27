%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 27. Mar 2021 8:12 AM
%%%-------------------------------------------------------------------
-module(cards).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).
-compile(export_all).


%% @doc Returns the 'Card' rank
%% ex: Card <<"As">> returns <<"A">>
-spec rank(binary()) -> binary().
rank(Card) ->
  binary:part(Card, {0, 1}).


%% @doc Returns the 'Card' suit
%% ex: Card <<"As">> returns <<"s">>
-spec suit(binary()) -> binary().
suit(Card) ->
  binary:part(Card, {1, 1}).


%% @doc Returns all possible 'Card's
-spec all() -> [binary()].
all() -> [
  <<"As">>,
  <<"Ah">>,
  <<"Ad">>,
  <<"Ac">>,
  <<"1s">>,
  <<"1h">>,
  <<"1d">>,
  <<"1c">>,
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
