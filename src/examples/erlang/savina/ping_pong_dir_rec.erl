%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/pingpong
%%%
%%% Two actors that repeatedly send and reply a Ping message back and forth.
%%% A ping count is maintained, which, once exhausted, induces the system to
%%% terminate.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(ping_pong_dir_rec).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([pong/0, ping/2]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Ping.
-type start() :: {start}.
-type pong() :: {pong}.

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
-new({ping_mb, [ping/2]}).
-use({ping_mb, [ping_loop/2]}).

%% Pong.
-new({pong_mb, [pong/0]}).
-use({pong_mb, [pong_exit/0]}).

% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Ping process handling the launching of main loop.
-spec ping(pong_mb(), integer()) -> no_return().
ping(Pong, Pings_left) ->
  ?expects("Start"),
  receive
    {start} ->
      ping_loop(Pong, Pings_left)
  end.

%% @doc Ping process main loop issuing ping requests.
-spec ping_loop(pong_mb(), integer()) -> no_return().
ping_loop(Pong, Pings_left) ->
  if Pings_left > 0 ->
    format("Pinging ~p...~n", [Pings_left]),

    %% Issue ping and await reply
    Self = self(),
    Pong ! {ping, Self},
    ?expects("Pong + 1"),
    receive
      {pong} ->
        ping_loop(Pong, Pings_left - 1)
    end;
    true ->
      %% No more pings to issue: notify ponger to stop
      Pong ! {stop}
  end.

%% @doc Pong process loop issuing pong replies.
-spec pong() -> no_return().
pong() ->
  ?expects("*(Ping + Stop)"),
  receive
    {ping, Ping} ->

      format("Ponging~n", []),
      Ping ! {pong},
      pong();
    {stop} ->
      pong_exit()
  end.

%% @doc Pong process exit procedure that flushes potential residual messages.
-spec pong_exit() -> no_return().
pong_exit() ->
  ?expects("*(Ping + Stop)"),
  receive
    {ping, Ping} ->
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

%% ./src/paterl src/examples/erlang/savina/ping_pong_dir_rec.erl -v all -I include