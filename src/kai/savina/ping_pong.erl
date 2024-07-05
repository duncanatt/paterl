%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 6月 2024 00:26
%%%-------------------------------------------------------------------
-module(ping_pong).
-author("walker").


-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([pong/0, ping/2]).

%% Mailbox interface-function associations.
-new({ping_mb, [ping/2]}).
-use({ping_mb, [ping_loop/2]}).
-new({pong_mb, [pong/0]}).
-use({pong_mb, [pong_exit/0]}).
-new({main_mb, [main/0]}).

%% PingMb's message types
-type start() :: {start}.
-type pong() :: {pong}.

%% PongMb's message types
-type ping() :: {ping, ping_mb()}.
-type stop() :: {stop}.

%% Interface PingMb {
%%    Start(),
%%    Pong()
%% }
%%
%% Interface PongMb {
%%    Ping(PingMb!),
%%    Stop()
%% }
-type ping_mb() :: pid() | start() | pong().
-type pong_mb() :: pid() | ping() | stop().
-type main_mb() :: pid().

%%  def ping(self: PingMb?, pongMb: PongMb!, pingsLeft: Int): Unit {
%%   guard self: Start {
%%     receive Start() from self ->
%%       ping_loop(self, pongMb, pingsLeft)
%%    }
%%  }
-spec ping(pong_mb(), integer()) -> no_return().
ping(Pong, Pings_left) ->
  ?mb_assert_regex("Start"),
  receive
    {start} ->
      ping_loop(Pong, Pings_left)
  end.

%% def ping_loop(self: PingMb?, pongMb: PongMb!, pingsLeft: Int): Unit {
%%
%%  if (pingsLeft > 0) {
%%    pongMb ! Ping(self);
%%    guard self: Pong + 1 {
%%      free ->
%%        ()
%%      receive Pong() from self ->
%%        ping_loop(self, pongMb, pingsLeft - 1)
%%    }
%%  }
%%  else {
%%    pongMb ! Stop();
%%    free(self)
%%  }
%% }
-spec ping_loop(pong_mb(), integer()) -> no_return().
ping_loop(Pong, Pings_left) ->
  Self = self(),
  if Pings_left > 0 ->
    %% Issue ping and await reply
    Pong ! {ping, Self},
    ?mb_assert_regex("Pong + 1"),
    receive
      {pong} ->
        ping_loop(Pong, Pings_left - 1)
    end;
    true ->
      %% No more pings to issue: notify ponger to stop
      Pong ! {stop}
  end.

%% def pong(self: PongMb?): Unit {
%%  guard self: *(Ping + Stop) {
%%    free -> ()
%%    receive Ping(pingMb) from self ->
%%      pingMb ! Pong();
%%      pong(self)
%%    receive Stop() from self ->
%%      pong_exit(self)
%%  }
%% }
-spec pong() -> no_return().
pong() ->
  ?mb_assert_regex("*(Ping + Stop)"),
  receive
    {ping, Ping} ->
      Ping ! {pong},
      pong();
    {stop} ->
      pong_exit()
  end.

%% def pong_exit(self: PongMb?): Unit {
%%  guard self: *(Ping + Stop) {
%%    free -> ()
%%    receive Ping(pingMb) from self ->
%%      pong_exit(self)
%%    receive Stop() from self ->
%%      pong_exit(self)
%%  }
%% }
-spec pong_exit() -> no_return().
pong_exit() ->
  ?mb_assert_regex("*(Ping + Stop)"),
  receive
    {ping, Ping} ->
      pong_exit();
    {stop} ->
      pong_exit()
  end.

%% def main(): Unit {
%%
%%  let pongMb = new [PongMb] in
%%  spawn { pong(pongMb) };
%%
%%  let pingMb = new [PingMb] in
%%  spawn { ping(pingMb, pongMb, 5) };
%%
%%  pingMb ! Start()
%%}
%%
%% main()
-spec main() -> no_return().
main() ->
  ?mb_new(pong_mb),
  Pong = spawn(?MODULE, pong, []),

  ?mb_new(ping_mb),
  Ping = spawn(?MODULE, ping, [Pong, 5]),

  Ping ! {start}.

