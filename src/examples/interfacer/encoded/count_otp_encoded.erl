%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../count_otp.erl for the proposed notation.
%%%
%%% OTP-style Interfacer version of Savina count.
%%%
%%% Adapted from Savina/count.
%%% A generator actor sends messages to a receiving actor who increments a
%%% counter upon receiving a message. The generator actor retrieves the total.
%%%
%%% Both roles are gen_servers. The final total is returned by
%%% gen_server:call/2 rather than sent directly to the producer.
%%%-------------------------------------------------------------------
-module(count_otp_encoded).
-behaviour(gen_server).

-include("interfacer.hrl").
%% This module implements several roles, so its declared interface is the
%% union of theirs. See master_worker_roles_otp/ for the split version, in
%% which each module declares one role.
-interface([producer_in, counter_in]).
-import(io, [format/2]).

-export([start_counter/1, start_producer/2, main/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%% Messages and call replies.
-type inc() :: {inc}.
-type get() :: {get}.
-type total() :: {total, integer()}.

%%% Interfaces.
-type producer_in() :: inc().
-type counter_in() :: #{call => get(), cast => inc(), info => none()}.

%%% No receive loop: the declarations above say which calls and casts are valid.

-type role() :: {producer, pid_of(counter_in()), integer()} | {counter, integer()}.

%% @doc Starts the counter role that handles increment requests.
-spec start_counter(integer()) -> {ok, pid_of(counter_in())} | ignore | {error, term()}.
start_counter(Total) ->
  gen_server:start_link(?MODULE, {counter, Total}, []).

%% @doc Starts the producer role that issues increment requests.
-spec start_producer(pid_of(counter_in()), integer()) ->
        {ok, pid_of(producer_in())} | ignore | {error, term()}.
start_producer(Counter, NumMessages) ->
  gen_server:start_link(?MODULE, {producer, Counter, NumMessages}, []).

-spec init(role()) -> {ok, role()}.
init(Role) ->
  {ok, Role}.

%% @doc Handles asynchronous increment/start messages for producer and counter.
-spec handle_cast(inc(), role()) ->
        {noreply, role()} | {stop, normal, role()} |
        {stop, {unexpected_cast, term()}, role()}.
handle_cast({inc}, {counter, Total}) ->
  {noreply, {counter, Total + 1}};
handle_cast({inc}, {producer, Counter, NumMessages} = State) ->
  producer_loop(Counter, NumMessages),
  {stop, normal, State};
handle_cast(Msg, Role) ->
  {stop, {unexpected_cast, Msg}, Role}.

%% @doc Handles the producer's request for the final total.
-spec handle_call(get(), {pid(), term()}, role()) ->
        {stop, normal, total(), role()} |
        {stop, {unexpected_call, term()}, role()}.
handle_call({get}, _From, {counter, Total} = State) ->
  {stop, normal, {total, Total}, State};
handle_call(Msg, _From, Role) ->
  {stop, {unexpected_call, Msg}, Role}.

%% Defensive fallback: no plain send is part of either interface.
-spec handle_info(term(), role()) ->
        {stop, {unexpected_info, term()}, role()}.
handle_info(Msg, Role) ->
  {stop, {unexpected_info, Msg}, Role}.

%% @doc Producer main loop issuing increment requests.
-spec producer_loop(pid_of(counter_in()), integer()) -> ok.
producer_loop(Counter, NumMessages) ->
  if NumMessages =< 0 ->
      {total, Total} = gen_server:call(Counter, {get}),
      format("Total: ~p.~n", [Total]);
    true ->
      gen_server:cast(Counter, {inc}),
      producer_loop(Counter, NumMessages - 1)
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  {ok, Counter} = start_counter(0),
  {ok, Producer} = start_producer(Counter, 16),
  gen_server:cast(Producer, {inc}).

%% Encoded form of ../count_otp.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/count_otp_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'count_otp_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize count_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/count_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/count_otp_encoded.erl
