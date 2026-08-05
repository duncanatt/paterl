%%%-------------------------------------------------------------------
%%% @author laura
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% Port of the pat-lang flip example. A producer sends a stream of
%%% messages to a consumer, flipping a coin before each send to decide
%%% whether to continue.
%%% @end
%%%-------------------------------------------------------------------
-module(flip).

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).
-import(rand, [uniform/1]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([consumer/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Consumer.
-type msg() :: {msg}.

%%% Interfaces.

%% Consumer.
-type consumer_mb() :: pid() | msg().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Consumer.
-new({consumer_mb, [consumer/0]}).
-use({consumer_mb, [consumer_loop/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Consumer process draining the messages sent by the producer.
-spec consumer() -> no_return().
consumer() ->
  consumer_loop().

%% @doc Consumer loop receiving the remaining messages.
-spec consumer_loop() -> no_return().
consumer_loop() ->
  ?expects("Msg*"),
  receive
    {msg} ->
      consumer_loop()
  after 0 ->
    format("Consumer exited.~n", [])
  end.

%% @doc Flips a coin and either sends a message to the consumer and continues,
%% or stops.
-spec producer(consumer_mb()) -> no_return().
producer(ConsumerRef) ->
  Flip = uniform(2),
  if Flip == 1 ->
    format("heads~n", []),
    ConsumerRef ! {msg},
    producer(ConsumerRef);
    true ->
      format("tails~n", [])
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ConsumerMb = spawn(?MODULE, consumer, []),
  producer(ConsumerMb).


%% ./src/paterl src/examples/erlang/other/flip.erl -v all -I include
