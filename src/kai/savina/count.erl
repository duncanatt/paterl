%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 14:52
%%%-------------------------------------------------------------------
-module(count).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([producer/2, counter/1]).

%% Mailbox interface-function associations.
-new({producer_mb, [producer/2]}).
-use({producer_mb, [producer_loop/2, producer_exit/1]}).
-new({counter_mb, [counter/1]}).
-use({counter_mb, [counter_exit/0]}).
-new({main_mb, [main/0]}).

%% ProducerMb's message types
-type inc() :: {inc}.
-type total() :: {total, integer()}.

%% CounterMb's message types
-type get() :: {get, producer_mb()}.

%% Interface ProducerMb {
%%    Inc(),
%%    Total(Int)
%% }
%% Interface CounterMb {
%%    Inc(),
%%    Get(ProducerMb!)
%% }
-type producer_mb() :: pid() | inc() | total().
-type counter_mb() :: pid() | inc() | get().
-type main_mb() :: pid().

%%  def producer(self: ProducerMb?, counterMb: CounterMb!, numMessages: Int): Unit {
%%    guard self: Inc {
%%      receive Inc() from self ->
%%        producer_loop(self, counterMb, numMessages)
%%    }
%%  }
-spec producer(counter_mb(), integer()) -> no_return().
producer(Counter, Num_messages) ->
  ?mb_assert_regex("Inc"),
  receive
    {inc} ->
      producer_loop(Counter, Num_messages)
  end.

%%  def producer_loop(self: ProducerMb?, counterMb: CounterMb!, numMessages: Int): Unit {
%%    if (numMessages <= 0) {
%%      counterMb ! Get(self);
%%      producer_exit(self, numMessages)
%%    }
%%    else {
%%      counterMb ! Inc();
%%      producer_loop(self, counterMb, numMessages - 1)
%%    }
%%  }
-spec producer_loop(counter_mb(), integer()) -> no_return().
producer_loop(Counter, Num_messages) ->
  Self = self(),
  if Num_messages =< 0 ->
    Counter ! {get, Self},
    producer_exit(Num_messages);
    true ->
      Counter ! {inc},
      producer_loop(Counter, Num_messages - 1)
  end.

%%  def producer_exit(self: ProducerMb?, numMessages: Int): Unit {
%%    guard self: Total {
%%      receive Total(total) from self ->
%%        print(concat("Total: ", intToString(total)));
%%        free(self)
%%    }
%%  }
-spec producer_exit(integer()) -> no_return().
producer_exit(Num_messages) ->
  ?mb_assert_regex("Total"),
  receive
    {total, Total} ->
      format("Total: ~p~n", [Total])
  end.

%%  def counter(self: CounterMb?, total: Int): Unit {
%%    guard self: (*Inc) . Get {
%%      free -> ()
%%      receive Inc() from self ->
%%        counter(self, total + 1)
%%      receive Get(producerMb) from self ->
%%        producerMb ! Total(total);
%%        counter_exit(self)
%%    }
%%  }
-spec counter(integer()) -> no_return().
counter(Total) ->
  ?mb_assert_regex("(*Inc) . Get"),
  receive
    {inc} ->
      counter(Total + 1);
    {get, Producer} ->
      Producer ! {total, Total},
      counter_exit()
  end.

%%  def counter_exit(self: CounterMb?): Unit {
%%    guard self: *Inc {
%%      free -> ()
%%      receive Inc() from self ->
%%        counter_exit(self)
%%    }
%%  }
-spec counter_exit() -> no_return().
counter_exit() ->
  ?mb_assert_regex("*Inc"),
  receive
    {inc} ->
      counter_exit()
  end.

%% Launcher.
%%  def main(): Unit {
%%    let counterMb = new [CounterMb] in
%%    spawn { counter(counterMb, 0) };
%%
%%    let producerMb = new [ProducerMb] in
%%    spawn { producer(producerMb, counterMb, 5) };
%%
%%    producerMb ! Inc()
%%  }
-spec main() -> no_return().
main() ->
  ?mb_new(counter_mb),
  Counter = spawn(?MODULE, counter, [0]),

  ?mb_new(producer_mb),
  Producer = spawn(?MODULE, producer, [Counter, 5]),

  Producer ! {inc}.
