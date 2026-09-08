%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../master_worker_master_otp.erl for the proposed notation.
%%%
%%% Master role for the one-module-per-role OTP master_worker example.
%%%
%%% This module implements one process role, so pid_of(master_in()) is the argument
%%% type of handle_call/3 directly, not hidden behind a role-dispatch state.
%%%
%%% Note that pid_of(master_in()) lists only task(): run_pool/1 reaches the pool with
%%% gen_server:call/2, so the pool's reply is consumed by that call and never
%%% enters the master's mailbox. In the plain version the master received the
%%% workers' results directly, and its interface had to be the union of the
%%% two protocols it engaged in.
%%%-------------------------------------------------------------------
-module(master_worker_master_otp_encoded).
-behaviour(gen_server).

-include("interfacer.hrl").
-interface(master_in).
-import(io, [format/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([master_in/0]).

%%% Interface.
%%% Message types are declared once in master_worker_msgs_otp and referred to
%%% remotely.
-type master_in() :: #{call => master_worker_msgs_otp_encoded:task(), cast => none(), info => none()}.

-type state() :: [].

%% @doc Starts the master server role.
%% start_link/0 here returns a master pid and nothing else, so the declared
%% return type is checkable rather than asserted.
-spec start_link() -> {ok, pid_of(master_in())} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

%% @doc Handles one incoming client task.
-spec handle_call(master_worker_msgs_otp_encoded:task(), {pid(), term()}, state()) ->
        {reply, master_worker_msgs_otp_encoded:result(), state()} |
        {stop, {unexpected_call, term()}, state()}.
handle_call({task, N}, _From, State) ->
  format("Received task to compute ~b.~n", [N]),
  Result = run_pool(N),
  format("Received result ~b from workers.~n", [Result]),
  {reply, {result, Result}, State};
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

%% @doc Delegates the whole task to a fresh pool and returns its total.
-spec run_pool(integer()) -> integer().
run_pool(Chunks) ->
  {ok, Pool} = master_worker_pool_otp_encoded:start_link(),
  {result, Result} = gen_server:call(Pool, {run, Chunks}),
  Result.

%% Encoded form of ../master_worker_master_otp.erl. Build and run everything with
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
%%   elp eqwalize master_worker_master_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_master_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_master_otp_encoded.erl
