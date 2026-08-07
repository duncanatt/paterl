%%%-------------------------------------------------------------------
%%% Pool role for the one-module-per-role OTP master_worker example.
%%%
%%% This module implements one process role, so pool_if() is the argument type
%%% of handle_call/3 directly. Worker results arrive as synchronous call
%%% replies, not as messages in the pool's mailbox, so pool_if() lists only
%%% the run request.
%%%-------------------------------------------------------------------
-module(master_worker_pool_otp).
-behaviour(gen_server).

-import(io, [format/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([pool_if/0]).

%%% Interface.
%%% Message types are declared once in master_worker_msgs_otp and referred to
%%% remotely.
-type pool_if() :: pid() | master_worker_msgs_otp:run().

-type state() :: [].

%%% Callback argument types state which messages reach each callback, and are
%%% checked against the interface declaration above.

%% @doc Starts the pool role that farms tasks and harvests results.
%% start_link/0 here returns a pool pid and nothing else, so the declared
%% return type is checkable rather than asserted.
-spec start_link() -> {ok, pool_if()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

%% @doc Handles one pool run request.
-spec handle_call(master_worker_msgs_otp:run(), {pid(), term()}, state()) ->
        {stop, normal, master_worker_msgs_otp:result(), state()} |
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
           [{integer(), master_worker_worker_otp:worker_if()}]) ->
        [{integer(), master_worker_worker_otp:worker_if()}].
farm(Count, Chunks, Workers) ->
  if Count == Chunks ->
      lists:reverse(Workers);
    true ->
      Task = Count + 1,
      {ok, Worker} = master_worker_worker_otp:start_link(),
      format("Farmed chunk ~b to worker ~p.~n", [Task, Worker]),
      farm(Task, Chunks, [{Task, Worker} | Workers])
  end.

%% @doc Collects and sums the individual results of the tasks assigned to
%% workers.
-spec harvest([{integer(), master_worker_worker_otp:worker_if()}], integer()) ->
        integer().
harvest([], Acc) ->
  Acc;
harvest([{Task, Worker} | Workers], Acc) ->
  {result, Result} = gen_server:call(Worker, {work, Task}),
  format("Harvested chunk ~b with result ~b.~n", [Task, Result]),
  harvest(Workers, Acc + Result).

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
