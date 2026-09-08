%%%-------------------------------------------------------------------
%%% Plain Interfacer version of master_worker.
%%%
%%% Master-worker set-up.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_plain).

-import(io, [format/2]).

-export([main/0]).
-export([master/0, worker/0, client/2]).

%%% Messages.
-type task() :: {task, pid(client_in()), integer()}.
-type result() :: {result, integer()}.
-type work() :: {work, pid(pool_in()), integer()}.

%%% Interfaces.
%%% pool/1 is called from the master loop, so the pool protocol runs in the
%%% master process: one mailbox, one interface. The master's interface is
%%% therefore the union of the two protocols it engages in, written here as a
%%% union of the types. pool_in names the result half on its own, for use where only that
%%% half is required.
-type pool_in() :: result().
-type task_in() :: task().
-type master_in() :: pool_in() | task_in().
-type worker_in() :: work().
-type client_in() :: result().

%% @doc Master server loop handling incoming client tasks.
-interface master :: master_in().
-spec master() -> no_return().
master() ->
  master_loop().

%% @doc Master server loop handling incoming client tasks.
-spec master_loop() -> no_return().
master_loop() ->
  receive
    {task, ReplyTo, N} ->
      format("Received task to compute ~b from client ~p.~n", [N, ReplyTo]),
      Result = pool(N),
      format("Received result ~b from workers.~n", [Result]),
      ReplyTo ! {result, Result},
      master_loop()
  end.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
%% pool/1 runs in the master process and would inherit master_in from its
%% caller. Declaring pool_in narrows that to the result half, which is all
%% this function uses. Interfacer checks the narrowing, pool_in =< master_in.
%% The narrowing describes how the process is used here, not a change to its
%% mailbox: receives are selective, so task messages stay queued until the
%% master loop reads them again.
-interface pool :: pool_in().
-spec pool(integer()) -> integer().
pool(Chunks) ->
  %% Self is typed pid(pool_in()) by the declaration above, which is what
  %% farm/3 requires. The workers therefore receive a reference that carries
  %% only the result messages they send back.
  Self = self(),
  farm(0, Chunks, Self),
  harvest(0, Chunks, 0).

%% @doc Worker computing assigned task by master.
-interface worker :: worker_in().
-spec worker() -> result().
worker() ->
  receive
    {work, ReplyTo, Task} ->
      Result = compute(Task),
      ReplyTo ! {result, Result}
  end.

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), pid(pool_in())) -> ok.
farm(Count, Chunks, Pool) ->
  if Count == Chunks ->
      ok;
    true ->
      Task = Count + 1,
      Worker = spawn(?MODULE, worker, []),
      Worker ! {work, Pool, Task},
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
harvest_exit() ->
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
-interface client :: client_in().
-spec client(integer(), pid(task_in())) -> any().
client(N, Master) ->
  Self = self(),
  Master ! {task, Self, N},
  format("Client ~p sent task ~b to master ~p.~n", [Self, N, Master]),
  receive
    {result, Result} ->
      format("Result from master: ~b.~n", [Result])
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Master = spawn(?MODULE, master, []),
  spawn(?MODULE, client, [5, Master]),
  ok.

% NOTE: this file uses the proposed notation, pid(I) and -interface, neither of
% which currently parses. See encoded/ for the runnable form.
