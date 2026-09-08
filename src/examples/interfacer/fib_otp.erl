%%%-------------------------------------------------------------------
%%% OTP-style Interfacer version of Savina fib.
%%%
%%% Adapted from Savina/fib.
%%% Server that calculates the Fibonacci number sent in client requests.
%%%
%%% Each Fibonacci worker is a gen_server. Recursive requests and results
%%% are expressed as gen_server:call/2 interactions.
%%%-------------------------------------------------------------------
-module(fib_otp).
-behaviour(gen_server).

-interface fib_in().

-import(io, [format/2]).

-export([start_fib/0, main/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([req/0, resp/0, fib_in/0]).

%%% Messages and call replies.
-type req() :: {req, integer()}.
-type resp() :: {resp, integer()}.

%%% Interfaces.
-type fib_in() :: #{call => req(), cast => none(), info => none()}.

%%% No receive loop: the declarations above say which calls and casts are valid.

%% @doc Starts a Fibonacci worker process.
-spec start_fib() -> {ok, pid(fib_in())} | ignore | {error, term()}.
start_fib() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, []}.
init([]) ->
  {ok, []}.

%% @doc Handles one Fibonacci request and returns the computed term.
-spec handle_call(req(), {pid(), term()}, []) ->
        {stop, normal, resp(), []} |
        {stop, {unexpected_call, term()}, []}.
handle_call({req, N}, _From, State) ->
  {stop, normal, {resp, fib(N)}, State};
handle_call(Msg, _From, State) ->
  {stop, {unexpected_call, Msg}, State}.

%% Defensive fallbacks: no cast or plain send is part of fib_in.
-spec handle_cast(term(), []) ->
        {stop, {unexpected_cast, term()}, []}.
handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

-spec handle_info(term(), []) ->
        {stop, {unexpected_info, term()}, []}.
handle_info(Msg, State) ->
  {stop, {unexpected_info, Msg}, State}.

%% @doc Computes the Fibonacci term, delegating recursive requests to workers.
-spec fib(integer()) -> integer().
fib(N) ->
  if N =< 2 ->
      % Base case.
      1;
    true ->
      % Inductive case: delegate computation of the (n - 1)st and (n - 2)nd
      % terms to fib process replicas.
      {ok, FibPid1} = start_fib(),
      {ok, FibPid2} = start_fib(),
      {resp, Term1} = gen_server:call(FibPid1, {req, N - 1}),
      {resp, Term2} = gen_server:call(FibPid2, {req, N - 2}),
      % Combine results computed for the (n - 1)st and (n - 2)nd terms.
      Term1 + Term2
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  {ok, FibPid} = start_fib(),
  {resp, Term} = gen_server:call(FibPid, {req, 16}),
  format("Result: ~p.~n", [Term]).

% NOTE: this file uses the proposed notation, pid(I) and -interface, neither of
% which currently parses. See encoded/ for the runnable form.
