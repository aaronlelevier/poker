%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 02. Apr 2021 6:28 AM
%%%-------------------------------------------------------------------
-module(dealer_test).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

deal_test() ->
  {ok, Pid} = dealer:start_link(),

  Ret = dealer:deal_card(Pid),

  ?assert(is_binary(Ret)),
  ?assertEqual(2, byte_size(Ret)).
