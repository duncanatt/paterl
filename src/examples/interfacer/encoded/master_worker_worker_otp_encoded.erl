%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../master_worker_worker_otp.erl for the proposed notation.
%%%
%%% Worker role for the OTP master_worker example.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_worker_otp_encoded).
-behaviour(gen_server).

-include("interfacer.hrl").
-interface(worker_in).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([worker_in/0]).

%%% Interface.
%%% Message types are declared once in master_worker_msgs_otp and referred to
%%% remotely.
-type worker_in() :: #{call => master_worker_msgs_otp_encoded:work(), cast => none(), info => none()}.

-type state() :: [].

%% @doc Starts a worker role that computes an assigned task.
%% start_link/0 here returns a worker pid and nothing else, so the declared
%% return type is checkable rather than asserted.
-spec start_link() -> {ok, pid_of(worker_in())} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

%% @doc Handles one worker task.
-spec handle_call(master_worker_msgs_otp_encoded:work(), {pid(), term()}, state()) ->
        {stop, normal, master_worker_msgs_otp_encoded:result(), state()} |
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

%% Encoded form of ../master_worker_worker_otp.erl. Build and run everything with
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
%%   elp eqwalize master_worker_worker_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_worker_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_worker_otp_encoded.erl
