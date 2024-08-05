%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 7月 2024 10:26
%%%-------------------------------------------------------------------
-module(master_worker).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([master/0, worker/0, farm/3, harvest/3, client/2]).

%% Mailbox interface-function associations
-new({master_mb, [master/0]}).
-use({master_mb, [master_loop/0]}).

-new({pool_mb, [pool/1]}).
-use({pool_mb, [harvest/3, harvest_exit/0]}).


-new({worker_mb, [worker/0]}).


-new({client_mb, [client/2]}).
-new({main_mb, [main/0]}).

%% MasterMb's message types
-type task() :: {task, client_mb(), integer()}.

%% PoolMb's message types
-type result() :: {result, integer()}.

%% WorkerMb's message types
-type work() :: {work, pool_mb(), integer()}.

%% # Mailbox message interfaces.
%% interface MasterMb {
%%    Task(ClientMb!, Int)
%% }
%%
%% interface PoolMb {
%%    Result(Int)
%% }
%%
%% interface WorkerMb {
%%    Work(PoolMb!, Int)
%% }
%%
%% interface ClientMb {
%%    Result(Int)
%% }
-type master_mb() :: pid() | task().
-type pool_mb() :: pid() | result().
-type worker_mb() :: pid() | work().
-type client_mb() :: pid() | result().
-type main_mb() :: pid().

%% def master(self: MasterMb?): Unit {
%%   guard self: *Task {
%%     free -> () # No more tasks to handle.
%%     receive Task(replyTo, n) from self ->
%%
%%       let pool = new[PoolMb] in
%%
%%       farm(0, n, pool);
%%
%%       let result = harvest(0, pool) in
%%       replyTo ! Result(result);
%%
%%       master(self)
%%   }
%% }
-spec master() -> no_return().
master() ->
  master_loop().

-spec master_loop() -> no_return().
master_loop() ->
  ?mb_assert_regex("*Task"),
  receive
    {task, ReplyTo, N} ->
      Result = pool(N),
      ReplyTo ! {result, Result},
      master_loop()
  end.

% New
-spec pool(integer()) -> integer().
pool(Chunks) ->
  Self = self(),
  farm(0, Chunks, Self),
%%  Self = self(),
  harvest(0, Chunks, 0).



%% def worker(self: WorkerMb?): Unit {
%%   guard self: Work {
%%     receive Work(replyTo, n) from self ->
%%       replyTo ! Result(compute(n));
%%       free(self)
%%   }
%% }
-spec worker() -> no_return().
worker() ->
  ?mb_assert_regex("Work"),
  receive
    {work, ReplyTo, Task} ->
      Result = compute(Task),
      ReplyTo ! {result, Result}
  end.

%% def farm(count: Int, chunks: Int, pool: PoolMb!): Unit {
%%   if (count == chunks) {
%%     ()
%%   }
%%   else {
%%     let task = count + 1 in
%%
%%     let workerMb = new[WorkerMb] in
%%     spawn { worker(workerMb) };
%%     workerMb ! Work(pool, task);
%%
%%     farm(task, chunks, pool)
%%   }
%% }
-spec farm(integer(), integer(), pool_mb()) -> no_return().
farm(Count, Chunks, Pool) ->
  if Count == Chunks ->
    ok;
    true ->
      Task = Count + 1,
      ?mb_new(worker_mb),
      WorkerMb = spawn(?MODULE, worker, []),
      WorkerMb ! {work, Pool, Task},
      farm(Task, Chunks, Pool)
  end.

%% def harvest(acc: Int, pool: PoolMb?): Int {
%%   guard pool: *Result {
%%     free ->
%%       acc
%%     receive Result(n) from pool ->
%%       harvest(acc + n, pool)
%%   }
%% }
-spec harvest(integer(), integer(), integer()) -> integer().
harvest(Count, Chunks, Acc) ->
  if (Count == Chunks) ->
    harvest_exit(), % To consume the possibly unconsumed Result messages and balance out the mailbox.
    % Note that this matches the pattern in the other branch, which seems to be key, in this simple case.
    % What about having something like ?garbage_collect("*Result") which generates the draining
    % function automatically in Pat?
    Acc;
  true ->
    ?mb_assert_regex("*Result"),
    receive
      {result, Result} ->
        Count0 = Count + 1,
        harvest(Count0, Chunks, Acc + Result)
    end
  end.

-spec harvest_exit() -> no_return().
harvest_exit() ->
  ?mb_assert_regex("*Result"),
  receive
    {result, Result} ->
      harvest_exit()
  end.

%% def compute(n: Int): Int {
%%    n * n
%% }
-spec compute(integer()) -> integer().
compute(N) ->
  N * N.

%% def client(n: Int, self: ClientMb?, masterMb: MasterMb!): Unit {
%%   masterMb ! Task(self, n);
%%   guard(self) : Result {
%%     receive Result(result) from self ->
%%       free(self);
%%       print(intToString(result))
%%   }
%% }
-spec client(integer(), master_mb()) -> no_return().
client(N, MasterMb) ->
  Self = self(),
  MasterMb ! {task, Self, N},
  ?mb_assert_regex("Result"),
  receive
    {result, Result} ->
      format("~p~n", [Result])
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
-spec main() -> any().
main() ->
  ?mb_new(master_mb),
  MasterMb = spawn(?MODULE, master, []),

  ?mb_new(client_mb),
  Client1 = spawn(?MODULE, client, [5, MasterMb]),

  ?mb_new(client_mb),
  Client2 = spawn(?MODULE, client, [10, MasterMb]),
  ok.
