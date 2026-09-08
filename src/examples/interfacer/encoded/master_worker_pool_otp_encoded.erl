%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../master_worker_pool_otp.erl for the proposed notation.
%%%
%%% Pool role for the one-module-per-role OTP master_worker example.
%%%
%%% This module implements one process role, so pid_of(pool_in()) is the argument type
%%% of handle_call/3 directly. Worker results arrive as synchronous call
%%% replies, not as messages in the pool's mailbox, so pid_of(pool_in()) lists only
%%% the run request.
%%%-------------------------------------------------------------------
-module(master_worker_pool_otp_encoded).
-behaviour(gen_server).

-include("interfacer.hrl").
-interface(pool_in).
-import(io, [format/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([pool_in/0]).

%%% Interface.
%%% Message types are declared once in master_worker_msgs_otp and referred to
%%% remotely.
-type pool_in() :: #{call => master_worker_msgs_otp_encoded:run(), cast => none(), info => none()}.

-type state() :: [].

%% @doc Starts the pool role that farms tasks and harvests results.
%% start_link/0 here returns a pool pid and nothing else, so the declared
%% return type is checkable rather than asserted.
-spec start_link() -> {ok, pid_of(pool_in())} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

%% @doc Handles one pool run request.
-spec handle_call(master_worker_msgs_otp_encoded:run(), {pid(), term()}, state()) ->
        {stop, normal, master_worker_msgs_otp_encoded:result(), state()} |
        {stop, {unexpected_call, term()}, state()}.
handle_call({run, Chunks}, _From, State) ->
  Result = pool(Chunks),
  {stop, normal, {result, Result}, State};
handle_call(Msg, _From, State) -> % defensive
  {stop, {unexpected_call, Msg}, State}.

-spec handle_cast(term(), state()) ->
        {stop, {unexpected_cast, term()}, state()}.
handle_cast(Msg, State) -> % defensive
  {stop, {unexpected_cast, Msg}, State}.

-spec handle_info(term(), state()) ->
        {stop, {unexpected_info, term()}, state()}.
handle_info(Msg, State) -> % defensive
  {stop, {unexpected_info, Msg}, State}.

%% @doc Pool that interfaces with workers to farm tasks and harvest results.
-spec pool(integer()) -> integer().
pool(Chunks) ->
  Workers = farm(0, Chunks, []),
  harvest(Workers, 0).

%% @doc Distributes tasks between worker processes.
-spec farm(integer(), integer(),
           [{integer(), pid_of(master_worker_worker_otp_encoded:worker_in())}]) ->
        [{integer(), pid_of(master_worker_worker_otp_encoded:worker_in())}].
farm(Count, Chunks, Workers) ->
  if Count == Chunks ->
      lists:reverse(Workers);
    true ->
      Task = Count + 1,
      {ok, Worker} = master_worker_worker_otp_encoded:start_link(),
      format("Farmed chunk ~b to worker ~p.~n", [Task, Worker]),
      farm(Task, Chunks, [{Task, Worker} | Workers])
  end.

%% @doc Collects and sums the individual results of the tasks assigned to
%% workers.
-spec harvest([{integer(), pid_of(master_worker_worker_otp_encoded:worker_in())}], integer()) ->
        integer().
harvest([], Acc) ->
  Acc;
harvest([{Task, Worker} | Workers], Acc) ->
  {result, Result} = gen_server:call(Worker, {work, Task}),
  format("Harvested chunk ~b with result ~b.~n", [Task, Result]),
  harvest(Workers, Acc + Result).

%% Encoded form of ../master_worker_pool_otp.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this example alone. It is spread over five
%% modules and driven by master_worker_roles_otp_encoded, so compile the
%% whole directory:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/*.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'master_worker_roles_otp_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize master_worker_pool_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_pool_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_pool_otp_encoded.erl
