%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../count_plain.erl for the proposed notation.
%%%
%%% Plain Interfacer version of Savina count.
%%%
%%% Adapted from Savina/count.
%%% A generator actor sends messages to a receiving actor who increments a
%%% counter upon receiving a message. The generator actor retrieves the total.
%%%-------------------------------------------------------------------
-module(count_plain_encoded).

-include("interfacer.hrl").
-import(io, [format/2]).

-export([main/0]).
-export([producer/2, counter/1]).

-interface([{producer, producer_in},
            {counter, counter_in}]).

%%% Messages.

%% Producer.
-type inc() :: {inc}.
-type total() :: {total, integer()}.

%% Counter.
-type get() :: {get, pid_of(producer_in())}.

%%% Interfaces.

%% Producer.
-type producer_in() :: inc() | total().
%% Counter.
-type counter_in() :: inc() | get().

%% @doc Producer process handling the launching of the main loop.
-spec producer(pid_of(counter_in()), integer()) -> ok.
producer(Counter, NumMessages) ->
  receive
    {inc} ->
      producer_loop(Counter, NumMessages)
  end.

%% @doc Producer process main loop issuing increment requests.
-spec producer_loop(pid_of(counter_in()), integer()) -> ok.
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
-spec producer_exit() -> ok.
producer_exit() ->
  receive
    {total, Total} ->
      format("Total: ~p.~n", [Total])
  end.

%% @doc Counter process main loop handling increment requests.
-spec counter(integer()) -> ok.
counter(Total) ->
  counter_loop(Total).

%% @doc Counter process main loop counting increment requests.
-spec counter_loop(integer()) -> ok.
counter_loop(Total) ->
  receive
    {inc} ->
      counter_loop(Total + 1);
    {get, Producer} ->
      Producer ! {total, Total},
      counter_exit()
  end.

%% @doc Counter process exit procedure that flushes potential residual messages.
%% The after 0 belongs here, as in kfork_plain and ping_pong_plain: without it
%% the counter blocks on an empty mailbox instead of exiting once drained.
-spec counter_exit() -> ok.
counter_exit() ->
  receive
    {inc} ->
      counter_exit()
  after 0 ->
    ok
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Counter = spawn(?MODULE, counter, [0]),
  Producer = spawn(?MODULE, producer, [Counter, 16]),
  Producer ! {inc}.

%% Encoded form of ../count_plain.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/count_plain_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'count_plain_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize count_plain_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/count_plain_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/count_plain_encoded.erl
