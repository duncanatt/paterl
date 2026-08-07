%%%-------------------------------------------------------------------
%%% Plain Interfacer version of Savina count.
%%%
%%% Adapted from Savina/count.
%%% A generator actor sends messages to a receiving actor who increments a
%%% counter upon receiving a message. The generator actor retrieves the total.
%%%-------------------------------------------------------------------
-module(count_plain).

-include("paterl.hrl").

-import(io, [format/2]).

-export([main/0]).
-export([producer/2, counter/1]).

%%% Messages.

%% Producer.
-type inc() :: {inc}.
-type total() :: {total, integer()}.

%% Counter.
-type get() :: {get, producer_if()}.

%%% Interfaces.

%% Producer.
-type producer_if() :: pid() | inc() | total().
%% Counter.
-type counter_if() :: pid() | inc() | get().

%%% ?as(Interface) annotates a pid-valued expression, such as self() or
%%% spawn(...), when Interfacer cannot otherwise resolve its process interface.

%%% ?as before a statically named spawn can be inferred
%%% from the call graph of the spawned function. Kept for explicitness
%%% Expect only ?as before self() is required.

%% @doc Producer process handling the launching of the main loop.
-spec producer(counter_if(), integer()) -> no_return().
producer(Counter, NumMessages) ->
  receive
    {inc} ->
      producer_loop(Counter, NumMessages)
  end.

%% @doc Producer process main loop issuing increment requests.
-spec producer_loop(counter_if(), integer()) -> no_return().
producer_loop(Counter, NumMessages) ->
  if NumMessages =< 0 ->
      ?as(producer_if),
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
  receive
    {inc} ->
      counter_exit()
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ?as(counter_if),
  Counter = spawn(?MODULE, counter, [0]),
  ?as(producer_if),
  Producer = spawn(?MODULE, producer, [Counter, 16]),
  Producer ! {inc}.

% erlc -I include -o ebin src/examples/interfacer/count_plain.erl
% erl -pa ebin -noshell -eval 'count_plain:main().' -s init stop
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize count_plain
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check this file with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/count_plain.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/count_plain.erl
