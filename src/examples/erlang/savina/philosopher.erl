%%% ----------------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/philosopher
%%%
%%% Dining philosophers problem.
%%% @end
%%% Created : 14. May 2024 18:02
%%% ----------------------------------------------------------------------------
-module(philosopher).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([arbiter/3, philosopher/3]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Philosopher.
-type start() :: {start}.
-type denied() :: {denied}.
-type eat() :: {eat}.

%% Arbiter.
-type hungry() :: {hungry, philosopher_mb(), integer()}.
-type done() :: {done, integer()}.
-type exit() :: {exit}.

%%% Interfaces.

%% Philosopher.
-type philosopher_mb() :: pid() | start() | denied() | eat().

%% Arbiter.
-type arbiter_mb() :: pid() | hungry() | done() | exit().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Philosopher.
-new({philosopher_mb, [philosopher/3]}).
-use({philosopher_mb, [philosopher_loop/3]}).

%% Arbiter.
-new({arbiter_mb, [arbiter/3]}).
-use({arbiter_mb, [arbiter_loop/3, arbiter_exit/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Philosopher process handling the launching of main loop.
-spec philosopher(integer(), integer(), arbiter_mb()) -> no_return().
philosopher(Id, NumRounds, Arbiter) ->
  ?mb_assert_regex("Start"),
  receive
    {start} ->
      philosopher_loop(Id, NumRounds, Arbiter)
  end.

%% @doc Philosopher process main loop issuing hunger requests.
-spec philosopher_loop(integer(), integer(), arbiter_mb()) -> no_return().
philosopher_loop(Id, NumRounds, Arbiter) ->
  Self = self(),
  Arbiter ! {hungry, Self, Id},

  ?mb_assert_regex("Denied + Eat + 1"),
  receive
    {denied} ->
      philosopher_loop(Id, NumRounds, Arbiter);
    {eat} ->
      Arbiter ! {done, Id},
      if
        NumRounds =< 0 ->
          Arbiter ! {exit};
        true ->
          philosopher_loop(Id, NumRounds - 1, Arbiter)
      end
  end.

%% @doc Arbiter process managing the allocation and deallocation of forks to
%% philosopher processes, as well as coordinating their termination.
-spec arbiter(integer(), boolean(), boolean()) -> no_return().
arbiter(NumExitedPhilosophers, Fork1, Fork2) ->
  arbiter_loop(NumExitedPhilosophers, Fork1, Fork2).

%% @doc Arbiter process managing the allocation and deallocation of forks to
%% philosopher processes, as well as coordinating their termination.
-spec arbiter_loop(integer(), boolean(), boolean()) -> no_return().
arbiter_loop(NumExitedPhilosophers, Fork1, Fork2) ->
  ?mb_assert_regex("*(Hungry + Done + Exit)"),
  receive
    {hungry, Philosopher, PhilosopherId} ->
      Available = forks_available(PhilosopherId, Fork1, Fork2),
      if
        Available ->
          % Notify philosopher and update fork allocation for specified
          % philosopher ID.
          Philosopher ! {eat},
          allocate_forks(PhilosopherId, Fork1, Fork2),
          arbiter_loop(NumExitedPhilosophers, Fork1, Fork2);
        true ->
          % One or both forks occupied.
          Philosopher ! {denied},
          arbiter_loop(NumExitedPhilosophers, Fork1, Fork2)
      end;
    {done, PhilosopherId} ->
      % Reset fork allocation.
      deallocate_forks(PhilosopherId, Fork1, Fork2),
      arbiter_loop(NumExitedPhilosophers, Fork1, Fork2);
    {exit} ->
      if
        NumExitedPhilosophers =< 0 ->
          arbiter_exit();
        true ->
          arbiter_loop(NumExitedPhilosophers - 1, Fork1, Fork2)
      end
  end.

%% @doc Stub. Checks whether the forks for the specified philosopher ID are
%% available.
-spec forks_available(integer(), boolean(), boolean()) -> boolean().
forks_available(Id, Fork1, Fork2) ->
  true.

%% @doc Stub. Toggles the Boolean values of the fork variables to indicate that they
%% are in use by the philosopher with the specified ID.
-spec allocate_forks(integer(), boolean(), boolean()) -> any().
allocate_forks(Id, Fork1, Fork2) ->
  ok.

%% @doc Stub. Toggles the Boolean values of the fork variables to indicate that they
%% are relinquished by the philosopher with the specified ID.
-spec deallocate_forks(integer(), boolean(), boolean()) -> any().
deallocate_forks(Id, Fork1, Fork2) ->
  % Cannot express because I need to return a tuple, which is currently not
  % supported in Erlang and Pat.
  ok.

%% @doc Arbiter process exit procedure flushing potential residual messages.
-spec arbiter_exit() -> no_return().
arbiter_exit() ->
  ?mb_assert_regex("*(Hungry + Done + Exit)"),
  receive
    {hungry, Philosopher, PhilosopherId} ->
      arbiter_exit();
    {done, Id} ->
      arbiter_exit();
    {exit} ->
      arbiter_exit()
  after 0 ->
    format("Arbiter exited.~n", [])
  end.



%% @doc Launcher.
-spec main() -> any().
main() ->
  NumRounds = 5,

  ?mb_new(arbiter_mb),
  Arbiter = spawn(?MODULE, arbiter, [2, false, false]),

  ?mb_new(philosopher_mb),
  Philosopher1 = spawn(?MODULE, philosopher, [0, NumRounds, Arbiter]),
  Philosopher1 ! {start},

  ?mb_new(philosopher_mb),
  Philosopher2 = spawn(?MODULE, philosopher, [1, NumRounds, Arbiter]),
  Philosopher2 ! {start}.


%% ./src/paterl src/examples/erlang/savina/philosopher.erl -v all -I include