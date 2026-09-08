%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../master_worker_otp.erl for the proposed notation.
%%%
%%% OTP-style Interfacer version of master_worker.
%%%
%%% Master-worker set-up.
%%%
%%% The master, pool, and workers are gen_servers. All communication uses
%%% gen_server:call/2, so the example contains no direct send operations.
%%%
%%% One callback module implements all three roles, so handle_call/3 accepts
%%% the union of their interfaces and the role is carried in the state. This
%%% is the fallback shape: start_master/0 and start_pool/0 both call
%%% gen_server:start_link(?MODULE, ...) so the endpoint types are asserted 
%%% rather than checked. See master_worker_roles_otp/ for the one-module-per-role
%%% version, in which each start_link/0 returns one kind of process.
%%%-------------------------------------------------------------------
-module(master_worker_otp_encoded).
-behaviour(gen_server).

-include("interfacer.hrl").
%% This module implements several roles, so its declared interface is the
%% union of theirs. See master_worker_roles_otp/ for the split version, in
%% which each module declares one role.
-interface([master_in, pool_in, worker_in]).
-import(io, [format/2]).

-export([start_master/0, client/2, main/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([task/0, run/0, result/0, work/0,
              master_in/0, pool_in/0, worker_in/0]).

%%% Messages and call replies.
-type task() :: {task, integer()}.
-type run() :: {run, integer()}.
-type result() :: {result, integer()}.
-type work() :: {work, integer()}.

%%% Interfaces.
-type master_in() :: task().
-type pool_in() :: run().
-type worker_in() :: work().

%%% No receive loop: the declarations above say which calls and casts are valid.

-type role() :: master | pool | worker.

%% @doc Starts the master server role.
-spec start_master() -> {ok, pid_of(master_in())} | ignore | {error, term()}.
start_master() ->
  gen_server:start_link(?MODULE, master, []).

%% @doc Starts the pool role that farms tasks and harvests results.
-spec start_pool() -> {ok, pid_of(pool_in())} | ignore | {error, term()}.
start_pool() ->
  gen_server:start_link(?MODULE, pool, []).

%% @doc Starts a worker role that computes an assigned task.
-spec start_worker() -> {ok, pid_of(worker_in())} | ignore | {error, term()}.
start_worker() ->
  gen_server:start_link(?MODULE, worker, []).

-spec init(role()) -> {ok, role()}.
init(Role) ->
  {ok, Role}.

%% @doc Handles master, pool, and worker calls dispatched by gen_server.
%% The union of the three roles' calls: this module implements master, pool
%% and worker, so its callback accepts the union of their interfaces.
-spec handle_call(task() | run() | work(), {pid(), term()}, role()) ->
        {reply, result(), role()} |
        {stop, normal, result(), role()} |
        {stop, {unexpected_call, term()}, role()}.
handle_call({task, N}, _From, master) ->
  {result, Result} = master_loop(N),
  {reply, {result, Result}, master};
handle_call({run, Chunks}, _From, pool) ->
  Result = pool(Chunks),
  {stop, normal, {result, Result}, pool};
handle_call({work, Task}, _From, worker) ->
  {stop, normal, {result, compute(Task)}, worker};
handle_call(Msg, _From, Role) ->
  {stop, {unexpected_call, Msg}, Role}.

%% Defensive fallbacks: no cast or plain send is part of any role's interface.
-spec handle_cast(term(), role()) ->
        {stop, {unexpected_cast, term()}, role()}.
handle_cast(Msg, Role) ->
  {stop, {unexpected_cast, Msg}, Role}.

-spec handle_info(term(), role()) ->
        {stop, {unexpected_info, term()}, role()}.
handle_info(Msg, Role) ->
  {stop, {unexpected_info, Msg}, Role}.

%% @doc Master server loop handling one incoming client task.
-spec master_loop(integer()) -> result().
master_loop(N) ->
  format("Received task to compute ~b.~n", [N]),
  {ok, Pool} = start_pool(),
  {result, Result} = gen_server:call(Pool, {run, N}),
  format("Received result ~b from workers.~n", [Result]),
  {result, Result}.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
-spec pool(integer()) -> integer().
pool(Chunks) ->
  Workers = farm(0, Chunks, []),
  harvest(Workers, 0).

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(), [{integer(), pid_of(worker_in())}]) ->
        [{integer(), pid_of(worker_in())}].
farm(Count, Chunks, Workers) ->
  if Count == Chunks ->
      lists:reverse(Workers);
    true ->
      Task = Count + 1,
      {ok, Worker} = start_worker(),
      format("Farmed chunk ~b to worker ~p.~n", [Task, Worker]),
      farm(Task, Chunks, [{Task, Worker} | Workers])
  end.

%% @doc Collects and sums the individual results of the tasks assigned to
%% workers.
-spec harvest([{integer(), pid_of(worker_in())}], integer()) -> integer().
harvest([], Acc) ->
  Acc;
harvest([{Task, Worker} | Workers], Acc) ->
  {result, Result} = gen_server:call(Worker, {work, Task}),
  format("Harvested chunk ~b with result ~b.~n", [Task, Result]),
  harvest(Workers, Acc + Result).

%% @doc Models a complex computation that a worker performs.
-spec compute(integer()) -> integer().
compute(N) ->
  N * N.

%% @doc Client issuing one numerical task to the master.
-spec client(integer(), pid_of(master_in())) -> any().
client(N, Master) ->
  {result, Result} = gen_server:call(Master, {task, N}),
  format("Result from master: ~b.~n", [Result]).

%% @doc Launcher.
-spec main() -> any().
main() ->
  {ok, Master} = start_master(),
  client(5, Master).

%% Encoded form of ../master_worker_otp.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/master_worker_otp_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'master_worker_otp_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize master_worker_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_otp_encoded.erl
