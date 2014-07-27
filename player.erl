-module(player).
-export([start/2, stop/1]).
-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

start(Name, Symbol) ->
  spawn(?MODULE, init, [Name, Symbol]).

init(Name, Symbol) ->
  loop(Name, Symbol).

stop(Pid) ->
  call(Pid, stop).


loop(Name, Symbol) ->
  receive
    {request, {Ref, Pid}, {play, Board}} ->
      io:format("Player ~p/~p is going to move~n", [Name, Symbol]),
      {Row, Column} = choose_one_of(board:available_moves(Board)),
      io:format("Put ~p to [~p,~p]~n", [Symbol, Row, Column]),
      reply(Pid, Ref, {move, {Row, Column, Symbol}}),
      loop(Name, Symbol);
    {request, {Ref, Pid}, stop} ->
      io:format("Player ~p is going down~n", [Name]),
      reply(Pid, Ref, ok)
  end.

choose_one_of(AvailableMoves) ->
  <<A1, A2, A3>> = crypto:strong_rand_bytes(3),
  random:seed(A1, A2, A3),
  lists:nth(random:uniform(length(AvailableMoves)), AvailableMoves).

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {request, {Ref, self()}, Msg},
  receive
    {reply, Ref, Reply} ->
      erlang:demonitor(Ref),
      Reply;
    {'DOWN', Ref, process, Pid, _Reason} ->
      error(player_dead)
  end.

reply(Pid, Ref, Reply) ->
  Pid ! {reply, Ref, Reply}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
