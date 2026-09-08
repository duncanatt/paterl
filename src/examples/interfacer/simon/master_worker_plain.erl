%%%-------------------------------------------------------------------
%%% Plain Interfacer version of master_worker.
%%%
%%% Master-worker set-up.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_plain).

-include("paterl.hrl").

-import(io, [format/2]).

-export([main/0]).
-export([master/0, worker/0, client/2]).

%%% Messages.
-type task() :: {task, pid(client_if()), integer()}.
-type result() :: {result, integer()}.
-type work() :: {work, pid(pool_if()), integer()}.

%%% Interfaces.
%%% pool/1 is called from the master loop, so the pool protocol runs in the
%%% master process: one mailbox, one interface. The master's interface is
%%% therefore the union of the two protocols it engages in, written here as a
%%% union of the types. pool_if remains a name for the harvesting facet,
%%% used where only that half is required.

-type pool_if() :: result().
-type task_if() :: task().
-type master_if() :: pool_if() | task_if().
-type worker_if() :: work().
-type client_if() :: result().

%%% ?as before a statically named spawn can be inferred
%%% from the call graph of the spawned function. It is kept below for
%%% explicitness. Only ?as before self() should be required.

%% @doc Master server loop handling incoming client tasks.
-spec master() -> no_return().
-interface master:master_if() %% Processes running this function have interface master_if().
master() ->
  master_loop().

%% @doc Master server loop handling incoming client tasks.
-spec master_loop() -> no_return().
master_loop() ->
  receive
    {task, ReplyTo, N} -> %% Check that ReplyTo is used with type pid(client_if()).
      format("Received task to compute ~b from client ~p.~n", [N, ReplyTo]),
      Result = pool(N),
      format("Received result ~b from workers.~n", [Result]),
      ReplyTo ! {result, Result}, %% Check that {result, Result} has type result().
      master_loop()
  end.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
-spec pool(integer()) -> integer().
pool(Chunks) ->
  Self = self(), %% This function is called from master_loop, which is called from master, so self() has type pid(master_if()).
  farm(0, Chunks, Self), %% Check that Self has type pid(pool_if()).
                         %% Here we use subtyping: pool_if() <: master_if(), therefore pid(master_if()) <: pid(pool_if()).
                         %% More explanation below, in the main function.
  harvest(0, Chunks, 0).

%% @doc Worker computing assigned task by master.
-spec worker() -> no_return().
-interface worker:worker_if(). %% Processes running this function have interface worker_if().
worker() ->
  receive
    {work, ReplyTo, Task} -> %% Check that ReplyTo is used with type pid(pool_if()).
      Result = compute(Task),
      ReplyTo ! {result, Result} %% Check that {result, Result} has type pool_if().
  end.

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), pid(pool_if())) -> ok.
farm(Count, Chunks, Pool) ->
  if Count == Chunks ->
      ok;
    true ->
      Task = Count + 1,
      Worker = spawn(?MODULE, worker, []), %% Worker has type pid(worker_if()).
      Worker ! {work, Pool, Task}, %% Check that {work, Pool, Task} has type worker_if().
      format("Farmed chunk ~b to worker ~p.~n", [Task, Worker]),
      farm(Task, Chunks, Pool)
  end.

%% @doc Collects and sums the individual results of the tasks assigned to
%% workers.
-spec harvest(integer(), integer(), integer()) -> integer().
harvest(Count, Chunks, Acc) ->
  format("Count ~p and chunks ~p~n", [Count, Chunks]),
  if Count == Chunks ->
      % Consume possibly unconsumed Result messages and balance out the mailbox.
      harvest_exit(),
      Acc;
    true ->
      receive
        {result, Result} ->
          Count0 = Count + 1,
          format("Harvested chunk ~b with result ~b.~n", [Count0, Result]),
          harvest(Count0, Chunks, Acc + Result)
      end
  end.

%% @doc Flushes the pool mailbox.
-spec harvest_exit() -> ok.
harvest_exit() -> %% This function is called from harvest, which is called from pool, so the interface is pool_if().
  % This kind of mailbox flushing function must avoid blocking forever, hence
  % the after 0 timeout below.
  receive
    {result, _Result} ->
      harvest_exit()
  after 0 ->
    ok
  end.

%% @doc Models a complex computation that a worker performs.
-spec compute(integer()) -> integer().
compute(N) ->
  N * N.

%% @doc Client issuing one numerical task to the master.
-spec client(integer(), pid(task_if())) -> any(). %% client uses interface task_if() to interact with master.
-interface client:client_if() %% Processes running this function have interface client_if().
                              %% Within this function, self() has type pid(client_if()).
client(N, Master) ->
  Self = self(), %% Self has type pid(client_if()).
  Master ! {task, Self, N}, %% Check that Self has type pid(client_if()). 
  format("Client ~p sent task ~b to master ~p.~n", [Self, N, Master]),
  receive
    {result, Result} ->
      format("Result from master: ~b.~n", [Result]) %% Check that Result is used with type integer().
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Master = spawn(?MODULE, master, []), %% Master has type pid(master_if()).
  spawn(?MODULE, client, [5, Master]), %% Check that Master has type pid(task_if()) to be a correct second argument of client.
                                       %% Here we use subtyping:  pid(master_if) <: pid(task_if()) 
                                       %% This is because task_if() <: master_if()  (subset of values)
                                       %% and pid(X) is contravariant in X because it's a send capability and send is contravariant (cf. pi-calculus).
  ok.
