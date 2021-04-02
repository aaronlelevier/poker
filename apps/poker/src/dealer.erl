%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(dealer).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-compile(export_all).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

deal_card(Pid) ->
  gen_server:call(Pid, deal).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, #{cards => card:all()}}.

handle_call(deal, _From, State) ->
  {Hand, State2} = deal_card0(State),
  {reply, Hand, State2};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
deal_card0(#{cards := Cards}) ->
  Card = random_card(Cards),
  Remaining = lists:delete(Card, Cards),
  {Card, #{cards => Remaining}}.


%% @doc Returns a random 'Card' from the default list of all 'Cards'
-spec random_card() -> binary().
random_card() -> random_card(card:all()).


%% @doc Returns a random 'Card' from the list of 'Cards'
-spec random_card([binary()]) -> binary().
random_card(Cards) ->
  lists:nth(rand:uniform(length(Cards)), Cards).
