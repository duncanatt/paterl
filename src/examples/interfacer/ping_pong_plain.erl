%%%-------------------------------------------------------------------
%%% Plain Interfacer version of Savina ping_pong.
%%%
%%% Adapted from Savina/pingpong.
%%% Two actors repeatedly send and reply to Ping messages. A ping count is
%%% maintained and, once exhausted, causes the system to terminate.
%%%
%%% Compared with the Mailboxer example, this file keeps the message and
%%% checks tag and payload conformance, not message ordering.
%%%-------------------------------------------------------------------
-module(ping_pong_plain).

-import(io, [format/2]).

-export([main/0]).
-export([ping/2, pong/0]).
-export_type([start/0, pong/0, ping/0, stop/0, ping_in/0, pong_in/0]).

%%% Messages.
-type start() :: {start}.
-type pong() :: {pong}.
-type ping() :: {ping, pid(ping_in())}.
-type stop() :: {stop}.

%%% Process interfaces.
-type ping_in() :: start() | pong().
-type pong_in() :: ping() | stop().

%% @doc Ping process handling the launching of its main loop.
-interface ping :: ping_in().
-spec ping(pid(pong_in()), integer()) -> stop().
ping(Pong, PingsLeft) ->
  receive
    {start} ->
      ping_loop(Pong, PingsLeft)
  end.

%% @doc Ping process main loop issuing ping requests.
-spec ping_loop(pid(pong_in()), integer()) -> stop().
ping_loop(Pong, PingsLeft) ->
  if PingsLeft > 0 ->
      format("Pinging ~p...~n", [PingsLeft]),
      %% Issue ping and await reply.
      Self = self(),
      Pong ! {ping, Self},
      receive
        {pong} ->
          ping_loop(Pong, PingsLeft - 1)
      end;
    true ->
      %% No more pings to issue: notify ponger to stop.
      Pong ! {stop}
  end.

%% @doc Pong process loop issuing pong replies.
-interface pong :: pong_in().
-spec pong() -> ok.
pong() ->
  pong_loop().

%% @doc Pong process loop issuing pong replies.
-spec pong_loop() -> ok.
pong_loop() ->
  receive
    {ping, Ping} ->
      format("Ponging~n", []),
      Ping ! {pong},
      pong_loop();
    {stop} ->
      pong_exit()
  end.

%% @doc Pong process exit procedure that flushes potential residual messages.
-spec pong_exit() -> ok.
pong_exit() ->
  receive
    {ping, _Ping} ->
      pong_exit();
    {stop} ->
      pong_exit()
  after 0 ->
    format("Ponger exited.~n", [])
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Pong = spawn(?MODULE, pong, []),
  Ping = spawn(?MODULE, ping, [Pong, 5]),
  Ping ! {start}.

% NOTE: this file uses the proposed notation, pid(I) and -interface, neither of
% which currently parses. See encoded/ for the runnable form.
