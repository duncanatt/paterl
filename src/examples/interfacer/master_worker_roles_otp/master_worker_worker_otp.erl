%%%-------------------------------------------------------------------
%%% Worker role for the OTP master_worker example.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_worker_otp).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([worker_if/0]).

%%% Interface.
%%% Message types are declared once in master_worker_msgs_otp and referred to
%%% remotely.
-type worker_if() :: pid() | master_worker_msgs_otp:work().

-type state() :: [].

%%% Callback argument types state which messages reach each callback, and are
%%% checked against the interface declaration above.

%% @doc Starts a worker role that computes an assigned task.
%% start_link/0 here returns a worker pid and nothing else, so the declared
%% return type is checkable rather than asserted.
-spec start_link() -> {ok, worker_if()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

%% @doc Handles one worker task.
-spec handle_call(master_worker_msgs_otp:work(), {pid(), term()}, state()) ->
        {stop, normal, master_worker_msgs_otp:result(), state()} |
        {stop, {unexpected_call, term()}, state()}.
handle_call({work, Task}, _From, State) ->
  {stop, normal, {result, compute(Task)}, State};
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

%% @doc Models a complex computation that a worker performs.
-spec compute(integer()) -> integer().
compute(N) ->
  N * N.

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
