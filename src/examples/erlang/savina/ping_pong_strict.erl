%%%-----------------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%  Adapted from Savina/pingpong
%%%
%%% Two actors that repeatedly send and reply a Ping message back and forth.
%%% A ping count is maintained, which, once exhausted, induces the system to
%%% terminate.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-----------------------------------------------------------------------------
-module(ping_pong_strict).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([ping/1, pong/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Ping.
-type start() :: {start, pong_mb()}.
-type pong() :: {pong, pong_mb()}.

%% Pong.
-type ping() :: {ping, ping_mb()}.
-type stop() :: {stop}.

%%% Interfaces.

%% Ping.
-type ping_mb() :: pid() | start() | pong().

%% Pong.
-type pong_mb() :: pid() | ping() | stop().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Ping.
-new({ping_mb, [ping/1]}).
-use({ping_mb, [send_ping/2, ping_loop/1]}).

%% Pong.
-new({pong_mb, [pong/0]}).
-use({pong_mb, [pong_loop/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Ping process handling launching of main loop.
-spec ping(integer()) -> no_return().
ping(Pings_left) ->
  ?mb_assert_regex("Start"),
  receive
    {start, Pong} ->
      send_ping(Pong, Pings_left)
  end.

%% @doc Issues a ping to the ponger process and loops or exits if no pings left.
-spec send_ping(pong_mb(), integer()) -> no_return().
send_ping(Pong, Pings_left) ->
  if Pings_left > 0 ->
    format("Pinging ~p...~n", [Pings_left]),

    Self = self(),
    Pong ! {ping, Self},
    ping_loop(Pings_left - 1);
    true ->
      Pong ! {stop}
  end.

%% @doc Ping process main loop issuing ping requests.
-spec ping_loop(integer()) -> no_return().
ping_loop(Pings_left) ->
  ?mb_assert_regex("Pong"),
  receive
    {pong, Pong} ->
      send_ping(Pong, Pings_left)
  end.

%% @doc Pong process loop issuing pong replies.
-spec pong() -> no_return().
pong() ->
  pong_loop().

%% @doc Pong process loop issuing pong replies.
-spec pong_loop() -> any().
pong_loop() ->
  ?mb_assert_regex("Ping + Stop"),
  receive
    {ping, Ping} ->
      format("Ponging~n", []),
      Self = self(),
      Ping ! {pong, Self},
      pong_loop();
    {stop} ->
      ok
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ?mb_new(pong_mb),
  Pong = spawn(?MODULE, pong, []),

  ?mb_new(ping_mb),
  Ping = spawn(?MODULE, ping, [5]),

  Ping ! {start, Pong}.

%% ./src/paterl src/examples/erlang/savina/ping_pong_strict.erl -v all -I include