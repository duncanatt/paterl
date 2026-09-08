%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../kfork_plain.erl for the proposed notation.
%%%
%%% Plain Interfacer version of Savina kfork.
%%%
%%% Adapted from Savina/fjthrput.
%%% Models the creation of n actors that are given a number of messages, for
%%% each of which a computation is performed. The benchmark is parameterized by
%%% the number of actor processes. Since the array type is not available in
%%% Pat, the original fixes the number of actor processes to 3.
%%%-------------------------------------------------------------------
-module(kfork_plain_encoded).

-include("interfacer.hrl").
-import(io, [format/2]).

-export([main/0]).
-export([actor/0]).
-export_type([packet/0, actor_in/0]).

-interface([{actor, actor_in}]).

%%% Messages.
-type packet() :: {packet}.
-type stop() :: {stop}.

%%% Interfaces.
-type actor_in() :: packet() | stop().

%% @doc Actor process entry point for handling packet requests.
-spec actor() -> ok.
actor() ->
  actor_loop().

%% @doc Actor process loop handling packet requests.
-spec actor_loop() -> ok.
actor_loop() ->
  receive
    {packet} ->
      Self = self(),
      format("~p Received packet.~n", [Self]),
      actor_loop();
    {stop} ->
      actor_exit()
  end.

%% @doc Flushes any packets left in the mailbox and exits.
%% The after 0 belongs here, not in actor_loop/0. With it in the loop the actor
%% almost always finds an empty mailbox on entry, since main/0 spawns before it
%% floods, and exits before any packet arrives. ping_pong_plain and
%% master_worker_plain separate the two for the same reason.
-spec actor_exit() -> ok.
actor_exit() ->
  receive
    {packet} ->
      actor_exit();
    {stop} ->
      actor_exit()
  after 0 ->
    format("Actor exited.~n", [])
  end.

%% @doc Sends the given number of messages to the specified actor process.
-spec flood(integer(), pid_of(actor_in())) -> ok.
flood(NumMessages, Actor) ->
  if NumMessages =< 0 ->
      Actor ! {stop},
      ok;
    true ->
      Actor ! {packet},
      flood(NumMessages - 1, Actor)
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Actor1 = spawn(?MODULE, actor, []),
  Actor2 = spawn(?MODULE, actor, []),
  Actor3 = spawn(?MODULE, actor, []),
  flood(5, Actor1),
  flood(10, Actor2),
  flood(15, Actor3).

%% Encoded form of ../kfork_plain.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/kfork_plain_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'kfork_plain_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize kfork_plain_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/kfork_plain_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/kfork_plain_encoded.erl
