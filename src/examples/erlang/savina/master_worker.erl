%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2023 12:21
%%%-------------------------------------------------------------------
-module(master_worker).
-author("duncan").

%% API
-export([]).

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([master/0, worker/0, client/2]).

%%% Type definitions.

%% Master interface.
%% interface MasterMb { Task(ClientMb!, Int) }
%% @type master_mb() :: {task, client_mb(), integer()}

%% Pool interface.
%% interface PoolMb { Result(Int) }
%% @type pool_mb() :: {result, integer() }

%% Worker interface.
%% interface WorkerMb { Work(PoolMb!, Int) }
%% @type worker_mb() :: {work, pool_mb(), integer()}

%% Client interface.
%% interface ClientMb { Result(Int) }
%% @type client_mb() :: {result, integer()}

%% def master(self: MasterMb?): Unit {
%%   guard self: *Task {
%%     free -> () # No more tasks to handle.
%%     receive Task(replyTo, n) from self ->
%%       # Create a local throwaway mailbox used by the master to farm tasks
%%       # and collect results.
%%       let pool = new[PoolMb] in
%%       farm(0, n, pool);
%%
%%       # Block until all the results are computed by each worker and
%%       # communicate result to client.
%%       let result = harvest(0, pool) in
%%       replyTo ! Result(result);
%%       master(self)
%%   }
%% }
%% @spec master() -> none()
%% @new master_mb()
master() ->
  %% @mb master_mb()
  %% @assert task*
  receive
    {task, ReplyTo, Task} ->
      format("= Master received task '~p' from client ~n", [Task]),
      _Dummy =
        %% @new pool_mb()
        farm(Task),

      Result =
        %% @use pool_mb()
        harvest(Task, 0),
      ReplyTo ! {result, Result},

      %% @use master_mb() %% Can be detected by a fixed point computation..maybe.
      master()
  end.

%% def farm(count: Int, chunks: Int, pool: PoolMb!): Unit {
%%   if (count == chunks) {
%%     ()
%%   }
%%   else {
%%     let task = count + 1 in
%%     let workerMb = new[WorkerMb] in
%%     spawn { worker(workerMb) };
%%     workerMb ! Work(pool, task);
%%
%%     farm(task, chunks, pool)
%%   }
%% }
%% @spec farm(integer()) -> integer()
%% @new pool_mb()
farm(Chunk) ->
  if Chunk == 0 ->
    format("= Farming complete~n", []),
    Chunk;
    true ->
      format("= Farming chunk ~p~n", [Chunk]),
      WorkerPid =
        %% @new worker_mb()
        spawn(?MODULE, worker, []),

      Self =
        %% @mb pool_mb()
        self(),
      WorkerPid ! {work, Self, Chunk},
      Next = Chunk - 1,

      %% @use pool_mb()
      farm(Next)
  end.

%% def harvest(acc: Int, pool: PoolMb?): Int {
%%   guard pool: *Result {
%%     free ->
%%       acc
%%     receive Result(n) from pool ->
%%       harvest(acc + n, pool)
%%   }
%% }
%% @spec harvest(integer(), integer()) -> integer()
%% @use pool_mb()
harvest(Chunk, Acc) ->
  if Chunk == 0 ->
    format("= Harvesting complete~n", []),
    Acc;
    true ->
      %% @mb pool_mb
      %% @assert result*
      receive
        {result, N} ->
          format("= Harvested task ~p with result '~p'~n", [Chunk, N]),
          Next = Chunk - 1,

          %% @use pool_mb()
          harvest(Next, Acc + N)
      end
  end.

%% def compute(n: Int): Int {
%%   n * n
%% }
%% @spec compute(integer()) -> integer()
compute(N) ->
  N * N.

%% def worker(self: WorkerMb?): Unit {
%%   guard self: work {
%%     receive Work(replyTo, n) from self ->
%%       replyTo ! Result(compute(n));
%%       free(self)
%%   }
%% }
%% @spec worker() -> none()
%% @new worker_mb()
worker() ->
  %% @mb worker_mb()
  %% @assert Work
  receive
    {work, ReplyTo, Chunk} ->
      format("= Worker computing chunk ~p~n", [Chunk]),
      Result = compute(Chunk),
      ReplyTo ! {result, Result}
  end.

%% def client(n: Int, self: ClientMb?, masterMb: MasterMb!): Unit {
%%   masterMb ! Task(self, n);
%%   guard self: Result {
%%     receive Result(result) from self ->
%%       free(self);
%%       print(intToString(result))
%%   }
%% }
%% @spec client(integer(), master_mb()) -> none()
%% @new client_mb()
client(Task, MasterPid) ->
  format("= Started client~n", []),
  Self =
    %% @mb client_mb()
    self(),
  MasterPid ! {task, Self, Task},

  %% @mb client_mb()
  %% @assert result
  receive
    {result, Result} ->
      format("Result: '~p'~n", [Result])
  end.

%% def main(): Unit {
%%   let masterMb = new[MasterMb] in
%%   spawn { master(masterMb) };
%%
%%   let client1 = new[ClientMb] in
%%   spawn { client(5, client1, masterMb) };
%%
%%   let client2 = new[ClientMb] in
%%   spawn { client(10, client2, masterMb) }
%% }
%% @spec main() -> none()
%% @new client_mb()
main() ->
  MasterPid =
    %% @new master_mb()
    spawn(?MODULE, master, []),
  %% @new client_mb()
  spawn(?MODULE, client, [5, MasterPid]),
  %% @new client_mb()
  spawn(?MODULE, client, [6, MasterPid]).



