%%%-------------------------------------------------------------------
%%% Plain Interfacer version of master_worker_dir_rec.
%%%
%%% Master-worker set-up.
%%%
%%% This variant keeps the direct-recursive master from the Mailboxer
%%% example while replacing the behavioural annotations with Interfacer
%%% process-interface declarations and ?as annotations.
%%%-------------------------------------------------------------------
-module(master_worker_dir_rec_plain).

-include("paterl.hrl").

-import(io, [format/2]).

-export([main/0]).
-export([master/0, worker/0, client/2]).

%%% Messages.
-type task() :: {task, client_if(), integer()}.
-type result() :: {result, integer()}.
-type work() :: {work, pool_if(), integer()}.

%%% Interfaces.
%%% pool/1 is called from the master loop, so the pool protocol runs in the
%%% master process: one mailbox, one interface. The master's interface is
%%% therefore the union of the two protocols it engages in, written here as a
%%% union of the facet types. pool_if remains a name for the harvesting facet,
%%% used where only that half is required.
-type pool_if() :: pid() | result().
-type master_if() :: pool_if() | task().
-type worker_if() :: pid() | work().
-type client_if() :: pid() | result().

%%% ?as before a statically named spawn should be inferred
%%% from the call graph of the spawned function. It is kept below for
%%% explicitness. Only ?as before self() should be required.

%% @doc Master server loop handling incoming client tasks.
-spec master() -> no_return().
master() ->
  receive
    {task, ReplyTo, N} ->
      format("Received task to compute ~b from client ~p.~n", [N, ReplyTo]),
      Result = pool(N),
      format("Received result ~b from workers.~n", [Result]),
      ReplyTo ! {result, Result},
      master()
  end.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
-spec pool(integer()) -> integer().
pool(Chunks) ->
  %% Self is the master process, whose interface is master_if. It is passed to
  %% farm/3 where pool_if is expected: master_if includes pool_if, so the
  %% endpoint accepts at least the result messages the workers send back.
  ?as(master_if),
  Self = self(),
  farm(0, Chunks, Self),
  harvest(0, Chunks, 0).

%% @doc Worker computing assigned task by master.
-spec worker() -> no_return().
worker() ->
  receive
    {work, ReplyTo, Task} ->
      Result = compute(Task),
      ReplyTo ! {result, Result}
  end.

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), pool_if()) -> ok.
farm(Count, Chunks, Pool) ->
  if Count == Chunks ->
      ok;
    true ->
      Task = Count + 1,
      ?as(worker_if),
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
      % Consume possibly unconsumed Result messages
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
-spec client(integer(), master_if()) -> any().
client(N, Master) ->
  ?as(client_if),
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
  ?as(master_if),
  Master = spawn(?MODULE, master, []),
  ?as(client_if),
  spawn(?MODULE, client, [5, Master]),
  ok.

% erlc -I include -o ebin src/examples/interfacer/master_worker_dir_rec_plain.erl
% erl -pa ebin -noshell -eval 'master_worker_dir_rec_plain:main(), timer:sleep(500), init:stop().'
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize master_worker_dir_rec_plain
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check this file with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/master_worker_dir_rec_plain.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/master_worker_dir_rec_plain.erl
