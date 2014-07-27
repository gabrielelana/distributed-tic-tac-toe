-module(game).
-export([start/2, start/3, stop/1]).
-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

start(PlayerOne, PlayerTwo) ->
  spawn(?MODULE, init, [PlayerOne, PlayerTwo]).

start(PlayerOne, PlayerTwo, Clock) ->
  spawn(?MODULE, init, [PlayerOne, PlayerTwo, Clock]).

init(PlayerOne, PlayerTwo) ->
  Clock = erlang:start_timer(1000, self(), tick),
  init(PlayerOne, PlayerTwo, Clock).

init(PlayerOne, PlayerTwo, Clock) ->
  % TODO: choose the moving player
  loop(PlayerOne, PlayerTwo, board:create(), Clock).

stop(Pid) ->
  call(Pid, stop).

loop(MovingPlayer, WaitingPlayer, Board, Clock) ->
  io:format("Board: ~p~n", [Board]),
  receive
    {timeout, Clock, tick} ->
      case play(MovingPlayer, Board) of
        {open, BoardWithLatestMove} ->
          NextClock = erlang:start_timer(1000, self(), tick),
          loop(WaitingPlayer, MovingPlayer, BoardWithLatestMove, NextClock);
        {win, _BoardWithLatestMove} ->
          win(MovingPlayer, WaitingPlayer);
        {tie, _BoardWithLatestMove} ->
          tie(MovingPlayer, WaitingPlayer)
      end;
    {request, {Ref, Pid}, stop} ->
      call(MovingPlayer, stop),
      call(WaitingPlayer, stop),
      reply(Pid, Ref, ok);
    UnknownMessage ->
      io:format("!!!~p~n", [UnknownMessage])
  end.

play(MovingPlayer, Board) ->
  {move, Move} = call(MovingPlayer, {play, Board}),
  BoardAfterMove = board:move(Board, Move),
  case board:check(BoardAfterMove) of
    {win, _} -> {win, BoardAfterMove};
    open -> {open, BoardAfterMove};
    tie -> {tie, BoardAfterMove}
  end.

win(_WinningPlayer, _LosingPlayer) ->
  todo.

tie(_PlayerOne, _PlayerTwo) ->
  todo.

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
