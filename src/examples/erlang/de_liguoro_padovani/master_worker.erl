%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Master-worker set-up.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(master_worker).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([master/0, worker/0, client/2]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Master.
-type task() :: {task, client_mb(), integer()}.

%% Pool.
-type result() :: {result, integer()}.

%% Worker.
-type work() :: {work, pool_mb(), integer()}.

%%% Interfaces.

%% Master.
-type master_mb() :: pid() | task().

%% Pool.
-type pool_mb() :: pid() | result().

%% Worker.
-type worker_mb() :: pid() | work().

%% Client.
-type client_mb() :: pid() | result().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Master.
-new({master_mb, [master/0]}).
-use({master_mb, [master_loop/0]}).

%% Pool.
-new({pool_mb, [pool/1]}).
-use({pool_mb, [harvest/3, harvest_exit/0]}).

%% Worker.
-new({worker_mb, [worker/0]}).

%% Client.
-new({client_mb, [client/2]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Master server loop handling incoming client tasks.
-spec master() -> no_return().
master() ->
  master_loop().

%% @doc Master server loop handling incoming client tasks.
-spec master_loop() -> no_return().
master_loop() ->
  ?expects("*Task"),
  receive
    {task, ReplyTo, N} ->
      format("Received task to compute ~b from client ~p.~n", [N, ReplyTo]),
      Result = pool(N),
      format("Received result ~b from workers.~n", [Result]),
      ReplyTo ! {result, Result},
      master_loop()
  end.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
-spec pool(integer()) -> integer().
pool(Chunks) ->
  Self = self(),
  farm(0, Chunks, Self),
  harvest(0, Chunks, 0).

%% @doc Worker computing assigned task by master.
-spec worker() -> no_return().
worker() ->
  ?expects("Work"),
  receive
    {work, ReplyTo, Task} ->
      Result = compute(Task),
      ReplyTo ! {result, Result}
  end.

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), pool_mb()) -> no_return().
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
  if (Count == Chunks) ->
    harvest_exit(), % To consume the possibly unconsumed Result messages and balance out the mailbox.
    % Note that this matches the pattern in the other branch, which seems to be key, in this simple case.
    % What about having something like ?garbage_collect("*Result") which generates the draining
    % function automatically in Pat but does not BLOCK the process forever in Erlang?
    Acc;
  true ->
    ?expects("*Result"),
    receive
      {result, Result} ->
        Count0 = Count + 1,

        format("Harvested chunk ~b with result ~b.~n", [Count0, Result]),
        harvest(Count0, Chunks, Acc + Result)
    end
  end.

%% @doc Flushes the master mailbox.
-spec harvest_exit() -> any().
harvest_exit() ->
  % TODO: This mailbox flushing function means that the process remains blocked forever.
  ?expects("*Result"),
  receive
    {result, Result} ->
      harvest_exit()
  after 0 ->
    ok
  end.

%% @doc Models a complex computation that a worker performs.
-spec compute(integer()) -> integer().
compute(N) ->
  N * N.

%% @doc Client issuing one (numerical) task to the master.
-spec client(integer(), master_mb()) -> any().
client(N, Master) ->
  Self = self(),
  Master ! {task, Self, N},

  format("Client ~p sent task ~b to master ~p.~n", [self(), N, Master]),
  ?expects("Result"),
  receive
    {result, Result} ->
      format("Result from master: ~b.~n", [Result])
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  MasterMb = spawn(?MODULE, master, []),

  spawn(?MODULE, client, [5, MasterMb]),
  ok.

%% ./src/paterl src/examples/erlang/de_liguoro_padovani/master_worker.erl -v all -I include