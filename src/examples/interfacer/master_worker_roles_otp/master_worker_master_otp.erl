%%%-------------------------------------------------------------------
%%% Master role for the one-module-per-role OTP master_worker example.
%%%
%%% This module implements one process role, so master_if() is the argument
%%% type of handle_call/3 directly, not hidden behind a role-dispatch state.
%%%
%%% Note that master_if() lists only task(): run_pool/1 reaches the pool with
%%% gen_server:call/2, so the pool's reply is consumed by that call and never
%%% enters the master's mailbox. In the plain version the master received the
%%% workers' results directly, and its interface had to be the union of the
%%% two protocols it engaged in.
%%%-------------------------------------------------------------------
-module(master_worker_master_otp).
-behaviour(gen_server).

-import(io, [format/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([master_if/0]).

%%% Interface.
%%% Message types are declared once in master_worker_msgs_otp and referred to
%%% remotely.
-type master_if() :: pid() | master_worker_msgs_otp:task().

-type state() :: [].

%%% Callback argument types state which messages reach each callback, and are
%%% checked against the interface declaration above. 

%% @doc Starts the master server role.
%% start_link/0 here returns a master pid and nothing else, so the declared
%% return type is checkable rather than asserted.
-spec start_link() -> {ok, master_if()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

%% @doc Handles one incoming client task.
-spec handle_call(master_worker_msgs_otp:task(), {pid(), term()}, state()) ->
        {reply, master_worker_msgs_otp:result(), state()} |
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
  {ok, Pool} = master_worker_pool_otp:start_link(),
  {result, Result} = gen_server:call(Pool, {run, Chunks}),
  Result.

% erlc -o ebin src/examples/interfacer/master_worker_roles_otp/*.erl
% erl -pa ebin -noshell -eval 'master_worker_roles_otp:main().' -s init stop
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize master_worker_roles_otp
%   elp eqwalize master_worker_master_otp
%   elp eqwalize master_worker_pool_otp
%   elp eqwalize master_worker_worker_otp
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check these files with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt src/examples/interfacer/master_worker_roles_otp/*.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt src/examples/interfacer/master_worker_roles_otp/*.erl
