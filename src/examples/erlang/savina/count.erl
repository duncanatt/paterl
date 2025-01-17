%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/count
%%%
%%% A generator actor sends messages to a receiving actor who increments a
%%% counter upon receiving a message. The generator actor retrieves the total.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(count).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([producer/2, counter/1]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Producer.
-type inc() :: {inc}.
-type total() :: {total, integer()}.

%% Counter.
-type get() :: {get, producer_mb()}.

%%% Interfaces.

%% Producer.
-type producer_mb() :: pid() | inc() | total().

%% Counter.
-type counter_mb() :: pid() | inc() | get().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Producer.
-new({producer_mb, [producer/2]}).
-use({producer_mb, [producer_loop/2, producer_exit/0]}).

%% Counter.
-new({counter_mb, [counter/1]}).
-use({counter_mb, [counter_loop/1, counter_exit/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Producer process handling the launching of main loop.
-spec producer(counter_mb(), integer()) -> no_return().
producer(Counter, NumMessages) ->
  ?expects("Inc"),
  receive
    {inc} ->
      producer_loop(Counter, NumMessages)
  end.

%% @doc Producer process main loop issuing increment requests.
-spec producer_loop(counter_mb(), integer()) -> no_return().
producer_loop(Counter, NumMessages) ->
  if NumMessages =< 0 ->
    Self = self(),
    Counter ! {get, Self},
    producer_exit();
    true ->
      Counter ! {inc},
      producer_loop(Counter, NumMessages - 1)
  end.

%% @doc Producer process exit procedure handling the final Total message.
-spec producer_exit() -> no_return().
producer_exit() ->
  ?expects("Total"),
  receive
    {total, Total} ->
      format("Total: ~p.~n", [Total])
  end.

%% @doc Counter process main loop handling increment requests.
-spec counter(integer()) -> no_return().
counter(Total) ->
  counter_loop(Total).

%% @doc Counter process main loop counting increment requests.
-spec counter_loop(integer()) -> no_return().
counter_loop(Total) ->
  ?expects("*Inc . Get"),
  receive
    {inc} ->
      counter_loop(Total + 1);
    {get, Producer} ->
      Producer ! {total, Total},
      counter_exit()
  end.

%% @doc Counter process exit procedure that flushes potential residual messages.
-spec counter_exit() -> no_return().
counter_exit() ->
  ?expects("*Inc"),
  receive
    {inc} ->
      counter_exit()
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Counter = spawn(?MODULE, counter, [0]),
  Producer = spawn(?MODULE, producer, [Counter, 16]),
  Producer ! {inc}.


%% ./src/paterl src/examples/erlang/savina/count.erl -v all -I include