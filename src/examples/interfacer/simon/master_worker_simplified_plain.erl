%%%-------------------------------------------------------------------
%%% Plain Interfacer version of master_worker (simplified).
%%%
%%% Master-worker set-up.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_simplified_plain).

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

%% @doc Master server loop handling incoming client tasks.
-spec master() -> no_return().
-interface master:master_if() %% Processes running this function have interface master_if().
master() ->
  receive %% Interfacer checks that all of the message patterns within the receive are in master_if(), otherwise there is dead code.
    {task, ReplyTo, N} -> %% Interfacer checks that ReplyTo is used with type pid(client_if()).
      Result = pool(N),
      ReplyTo ! {result, Result}, %% Interfacer checks that {result, Result} has type result() as required by the type of ReplyTo.
      master()
  end.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
-spec pool(integer()) -> integer().
-interface pool:pool_if(). %% Processes running this function have interface pool_if().
                           %% pool is called from master, so its interface is master_if() by default,
                           %% but this declaration changes it to pool_if().
                           %% Interfacer checks that pool_if() <: master_if(), so that subsequent checking of message patterns in receives is correct
                           %% and so that other processes can't use the pid of pool to violate the interface of master.
pool(Chunks) ->
  Self = self(), %% self() has type pid(pool_if).
  farm(0, Chunks, Self), %% Interfacer checks that Self has type pid(pool_if()) as required by the spec of farm.
  harvest(0, Chunks, 0).

%% @doc Worker computing assigned task by master.
-spec worker() -> no_return().
-interface worker:worker_if(). %% Processes running this function have interface worker_if().
worker() ->
  receive %% Interfacer checks that all of the message patterns within the receive are in worker_if(), otherwise there is dead code.
    {work, ReplyTo, Task} -> %% Interfacer checks that ReplyTo is used with type pid(pool_if()).
      Result = compute(Task),
      ReplyTo ! {result, Result} %% Interfacer checks that {result, Result} has type pool_if().
  end.

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), pid(pool_if())) -> ok. %% farm is called from pool, so its interface is pool_if().
                                                        %% There is nothing for Interfacer to check though, because there is no receive.
farm(Count, Chunks, Pool) ->
  if Count == Chunks ->
       ok;
     true ->
       Task = Count + 1,
       Worker = spawn(?MODULE, worker, []), %% Worker has type pid(worker_if()).
       Worker ! {work, Pool, Task}, %% Interfacer checks that {work, Pool, Task} has type worker_if().
       farm(Task, Chunks, Pool)
  end.

%% @doc Collects and sums the individual results of the tasks assigned to workers.
-spec harvest(integer(), integer(), integer()) -> integer(). %% harvest is called from pool, so its interface is pool_if().
harvest(Count, Chunks, Acc) ->
  if Count == Chunks ->
       Acc;
     true ->
       receive %% Interfacer checks that all of the message patterns are in pool_if().
         {result, Result} ->
           Count0 = Count + 1,
           harvest(Count0, Chunks, Acc + Result)
       end
  end.

%% @doc Models a complex computation that a worker performs.
-spec compute(integer()) -> integer(). %% Processes running this function have interface worker_if() because it is called by worker.
                                       %% There is nothing for Interfacer to check though, because there is no receive.
compute(N) ->
  N * N.

%% @doc Client issuing one numerical task to the master.
-spec client(integer(), pid(task_if())) -> any(). %% client uses interface task_if() to interact with master.
-interface client:client_if() %% Processes running this function have interface client_if().
client(N, Master) ->
  Self = self(), %% Self has type pid(client_if()).
  Master ! {task, Self, N}, %% Interfacer checks that Self has type pid(client_if()), as required by the type of Master. 
  receive
    {result, Result} ->
      format("Result from master: ~b.~n", [Result]) %% Interfacer checks that Result is used with type integer().
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Master = spawn(?MODULE, master, []), %% Master has type pid(master_if()).
  spawn(?MODULE, client, [5, Master]), %% Interfacer checks that Master has type pid(task_if()) to be a correct second argument of client.
                                       %% Here we use subtyping:  pid(master_if) <: pid(task_if()) 
                                       %% This is because task_if() <: master_if()  (subset of values)
                                       %% and pid(X) is contravariant in X because it's a send capability and send is contravariant (cf. pi-calculus).
  ok.
