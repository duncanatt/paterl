%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../master_worker_plain.erl for the proposed notation.
%%%
%%% Plain Interfacer version of master_worker.
%%%
%%% Master-worker set-up.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_plain_encoded).

-include("interfacer.hrl").
-import(io, [format/2]).

-export([main/0]).

%% master_in, worker_in and work are named only by the -interface entries in
%% this module, which erlc does not count as a type use; exporting them says
%% they are part of this module's interface and silences the unused-type
%% warning.
-export_type([master_in/0, work/0, worker_in/0]).

-export([master/0, worker/0, client/2]).

-interface([{master, master_in},
            {pool, pool_in},
            {worker, worker_in},
            {client, client_in}]).

%%% Messages.
-type task() :: {task, pid_of(client_in()), integer()}.
-type result() :: {result, integer()}.
-type work() :: {work, pid_of(pool_in()), integer()}.

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
%% Declared above as {pool, pool_in}: pool/1 runs in the master process and
%% would inherit master_in from its caller. Declaring pool_in narrows that to
%% the result half, which is all this function uses. Interfacer checks the
%% narrowing, pool_in =< master_in. The narrowing describes how the process is
%% used here, not a change to its mailbox: receives are selective, so task
%% messages stay queued until the master loop reads them again.
-spec pool(integer()) -> integer().
pool(Chunks) ->
  %% Self is typed pid_of(pool_in()) by the declaration above, which is what
  %% farm/3 requires. The workers therefore receive a reference that carries
  %% only the result messages they send back.
  Self = self(),
  farm(0, Chunks, Self),
  harvest(0, Chunks, 0).

%% @doc Worker computing assigned task by master.
-spec worker() -> result().
worker() ->
  receive
    {work, ReplyTo, Task} ->
      Result = compute(Task),
      ReplyTo ! {result, Result}
  end.

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), pid_of(pool_in())) -> ok.
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
-spec client(integer(), pid_of(task_in())) -> any().
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

%% Encoded form of ../master_worker_plain.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/master_worker_plain_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'master_worker_plain_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize master_worker_plain_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_plain_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_plain_encoded.erl
