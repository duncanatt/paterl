%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 22:08
%%%-------------------------------------------------------------------
-module(philosopher).
-author("walker").


-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/1]).

-export([arbiter/3,philosopher/3]).

%% Mailbox interface-function associations.
-new({philosopher_mb, [philosopher/3]}).
-use({philosopher_mb, [philosopher_loop/3]}).
-new({arbiter_mb, [arbiter/3]}).
-use({arbiter_mb, [arbiter_exit/0]}).
-new({main_mb, [main/1]}).

%% PhilosopherMb's message types
-type start() :: {start}.
-type denied() :: {denied}.
-type eat() :: {eat}.

%% ArbiterMb's message types
-type hungry() :: {hungry, philosopher_mb(), integer()}.
-type done() :: {done, integer()}.
-type exit() :: {exit}.

%% Interface PhilosopherMb {
%%    Start(),
%%    Denied(),
%%    Eat()
%% }
%%
%% Interface ArbiterMb {
%%    Hungry(PhilosopherMb!, Int),
%%    Done(Int),
%%    Exit()
%% }
-type philosopher_mb() :: pid() | start() | denied() | eat().
-type arbiter_mb() :: pid() | hungry() | done() | exit().
-type main_mb() :: pid().

%%  def philosopher(self: PhilosopherMb?, id: Int, numRounds: Int, arbiterMb: ArbiterMb!): Unit {
%%    guard self: Start {
%%      receive Start() from self ->
%%        philosopher_loop(self, id, numRounds, arbiterMb)
%%    }
%%  }
-spec philosopher(integer(), integer(), arbiter_mb()) -> no_return().
philosopher(Id, Num_rounds, Arbiter) ->
  ?mb_assert_regex("Start"),
  receive
    {start} ->
      philosopher_loop(Id, Num_rounds, Arbiter)
  end.

%%  def philosopher_loop(self: PhilosopherMb?, id: Int, numRounds: Int, arbiterMb: ArbiterMb!): Unit {
%%
%%    arbiterMb ! Hungry(self, id);
%%
%%    guard self: Denied + Eat + 1 {
%%      free -> ()
%%      receive Denied() from self ->
%%        philosopher_loop(self, id, numRounds, arbiterMb)
%%      receive Eat() from self ->
%%        arbiterMb ! Done(id);
%%
%%        if (numRounds <= 0) {
%%          arbiterMb ! Exit();
%%          free(self)
%%        }
%%        else {
%%          philosopher_loop(self, id, numRounds - 1, arbiterMb)
%%        }
%%    }
%%  }
-spec philosopher_loop(integer(), integer(), arbiter_mb()) -> no_return().
philosopher_loop(Id, Num_rounds, Arbiter) ->
  Self = self(),
  Arbiter ! {hungry, Self, Id},
  ?mb_assert_regex("Denied + Eat + 1"),
  receive
    {denied} ->
      philosopher_loop(Id, Num_rounds, Arbiter);
    {eat} ->
      Arbiter ! {done, Id},
      if
        Num_rounds =< 0 ->
          Arbiter ! {exit};
        true ->
          philosopher_loop(Id, Num_rounds - 1, Arbiter)
      end
  end.

%%  def arbiter(self: ArbiterMb?, numExitedPhilosophers: Int, fork1: Bool, fork2: Bool): Unit {
%%    guard self: *(Hungry + Done + Exit) {
%%      free ->
%%        ()
%%      receive Hungry(philosopherMb, philosopherId) from self ->
%%
%%        if (forks_available(philosopherId, fork1, fork2)) {
%%          philosopherMb ! Eat();
%%          allocate_forks(philosopherId, fork1, fork2);
%%          arbiter(self, numExitedPhilosophers, fork1, fork2)
%%        }
%%        else {
%%          philosopherMb ! Denied();
%%          arbiter(self, numExitedPhilosophers, fork1, fork2)
%%        }
%%
%%      receive Done(philosopherId) from self ->
%%        deallocate_forks(philosopherId, fork1, fork2);
%%        arbiter(self, numExitedPhilosophers, fork1, fork2)
%%
%%      receive Exit() from self ->
%%        if (numExitedPhilosophers <= 0) {
%%          arbiter_exit(self)
%%        }
%%        else {
%%          arbiter(self, numExitedPhilosophers - 1, fork1, fork2)
%%        }
%%    }
%%  }
-spec arbiter(integer(), boolean(), boolean()) -> no_return().
arbiter(Num_exited_philosophers, Fork1, Fork2) ->
  ?mb_assert_regex("*(Hungry + Done + Exit)"),
  receive
    {hungry, Philosopher, Philosopher_id} ->
      Available = forks_available(Philosopher_id, Fork1, Fork2),
      if
        Available ->
          Philosopher ! {eat},
          allocate_forks(Philosopher_id, Fork1, Fork2),
          arbiter(Num_exited_philosophers, Fork1, Fork2);
        true ->
          Philosopher ! {denied},
          arbiter(Num_exited_philosophers, Fork1, Fork2)
      end;
    {done, Philosopher_id} ->
      deallocate_forks(Philosopher_id, Fork1, Fork2),
      arbiter(Num_exited_philosophers, Fork1, Fork2);
    {exit} ->
      if
        Num_exited_philosophers =< 0 ->
          arbiter_exit();
        true ->
          arbiter(Num_exited_philosophers - 1, Fork1, Fork2)
      end
  end.

%%  def forks_available(id: Int, fork1: Bool, fork2: Bool): Bool {
%%    true
%%  }
-spec forks_available(integer(), boolean(), boolean()) -> boolean().
forks_available(Id, Fork1, Fork2) ->
  true.

%%  def allocate_forks(id: Int, fork1: Bool, fork2: Bool): Unit {
%%    ()
%%  }
-spec allocate_forks(integer(), boolean(), boolean()) -> no_return().
allocate_forks(Id, Fork1, Fork2) ->
  ok.

%% Stub. Toggles the Boolean values of the fork variables to indicate that they
%% are relinquished by the philosopher with the specified ID.
%%  def deallocate_forks(id: Int, fork1: Bool, fork2: Bool): Unit {
%%    ()
%%  }
-spec deallocate_forks(integer(), boolean(), boolean()) -> no_return().
deallocate_forks(Id, Fork1, Fork2) ->
  ok.

%%  def arbiter_exit(self: ArbiterMb?): Unit {
%%    guard self: *(Hungry + Done + Exit) {
%%      free -> ()
%%      receive Hungry(philosopherMb, philosopherId) from self ->
%%        arbiter_exit(self)
%%      receive Done(id) from self ->
%%        arbiter_exit(self)
%%      receive Exit() from self ->
%%        arbiter_exit(self)
%%    }
%%  }
-spec arbiter_exit() -> no_return().
arbiter_exit() ->
  ?mb_assert_regex("*(Hungry + Done + Exit)"),
  receive
    {hungry, Philosopher, Philosopher_id} ->
      arbiter_exit();
    {done, Id} ->
      arbiter_exit();
    {exit} ->
      arbiter_exit()
  end.

%% Launcher.
%%  def main(numRounds: Int): Unit {
%%
%%    let arbiterMb = new [ArbiterMb] in
%%    spawn { arbiter(arbiterMb, 2, false, false) };
%%
%%    let philosopherMb1 = new [PhilosopherMb] in
%%    spawn { philosopher(philosopherMb1, 0, numRounds, arbiterMb) };
%%    philosopherMb1 ! Start();
%%
%%    let philosopherMb2 = new [PhilosopherMb] in
%%    spawn { philosopher(philosopherMb2, 0, numRounds, arbiterMb) };
%%    philosopherMb2 ! Start()
%%  }
-spec main(integer()) -> no_return().
main(Num_rounds) ->
  ?mb_new(arbiter_mb),
  Arbiter = spawn(?MODULE, arbiter, [2, false, false]),

  ?mb_new(philosopher_mb),
  Philosopher1 = spawn(?MODULE, philosopher, [0, Num_rounds, Arbiter]),
  Philosopher1 ! {start},

  ?mb_new(philosopher_mb),
  Philosopher2 = spawn(?MODULE, philosopher, [1, Num_rounds, Arbiter]),
  Philosopher2 ! {start}.

