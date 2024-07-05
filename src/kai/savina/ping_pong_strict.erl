%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 22:38
%%%-------------------------------------------------------------------
-module(ping_pong_strict).
-author("walker").


-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([ping/1, pong/0]).

%% Mailbox interface-function associations.
-new({ping_mb, [ping/1]}).
-use({ping_mb, [ping_loop/1]}).
-new({pong_mb, [pong/0]}).
-main({main_mb, [main/0]}).

%% PingMb's message types
-type start() :: {start, pong_mb()}.
-type pong() :: {pong, pong_mb()}.

%% PongMb's message types
-type ping() :: {ping, ping_mb()}.
-type stop() :: {stop}.

%% Interface PingMb {
%%    Start(PongMb!),
%%    Pong(PongMb!)
%% }
%%
%% Interface PongMb {
%%    Ping(PingMb!),
%%    Stop()
%% }
-type ping_mb() :: pid() | start() | pong().
-type pong_mb() :: pid() | ping() | stop().
-type main_mb() :: pid().

%%  def ping(self: PingMb?, pingsLeft: Int): Unit {
%%    guard self: Start {
%%      receive Start(pongMb) from self ->
%%        send_ping(self, pongMb, pingsLeft)
%%    }
%%  }
-spec ping(integer()) -> no_return().
ping(Pings_left) ->
  ?mb_assert_regex("Start"),
  receive
    {start, Pong} ->
      send_ping(Pong, Pings_left)
  end.

%%  def send_ping(self: PingMb?, pongMb: PongMb!, pingsLeft: Int): Unit {
%%    if (pingsLeft > 0) {
%%      pongMb ! Ping(self);
%%      ping_loop(self, pingsLeft - 1)
%%    }
%%    else {
%%      pongMb ! Stop();
%%      free(self)
%%    }
%%  }
-spec send_ping(pong_mb(), integer()) -> no_return().
send_ping(Pong, Pings_left) ->
  if Pings_left > 0 ->
    Self = self(),
    Pong ! {ping, Self},
    ping_loop(Pings_left - 1);
    true ->
      Pong ! {stop}
  end.

%%  def ping_loop(self: PingMb?, pingsLeft: Int): Unit {
%%
%%    guard self: Pong {
%%      receive Pong(pongMb) from self ->
%%        send_ping(self, pongMb, pingsLeft)
%%    }
%%  }
-spec ping_loop(integer()) -> no_return().
ping_loop(Pings_left) ->
  ?mb_assert_regex("Pong"),
  receive
    {pong, Pong} ->
      send_ping(Pong, Pings_left)
  end.

%%  def pong(self: PongMb?): Unit {
%%    guard self: Ping + Stop {
%%      receive Ping(pingMb) from self ->
%%        pingMb ! Pong(self);
%%        pong(self)
%%      receive Stop() from self ->
%%        free(self)
%%    }
%%  }
-spec pong() -> no_return().
pong() ->
  ?mb_assert_regex("Ping + Stop"),
  receive
    {ping, Ping} ->
      Self = self(),
      Ping ! {pong, Self},
      pong();
    {stop} ->
%%      TODO: Not sure about free
      ok
  end.

%%  def main(): Unit {
%%
%%    let pongMb = new [PongMb] in
%%    spawn { pong(pongMb) };
%%
%%    let pingMb = new [PingMb] in
%%    spawn { ping(pingMb, 5) };
%%
%%    pingMb ! Start(pongMb)
%%  }
-spec main() -> no_return().
main() ->
  ?mb_new(pong_mb),
  Pong = spawn(?MODULE, pong, []),

  ?mb_new(ping_mb),
  Ping = spawn(?MODULE, ping, [5]),

  Ping ! {start, Pong}.
