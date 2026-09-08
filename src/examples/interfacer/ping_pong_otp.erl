%%%-------------------------------------------------------------------
%%% OTP-style Interfacer version of Savina ping_pong.
%%%
%%% Adapted from Savina/pingpong.
%%% Two actors repeatedly send and reply to Ping messages. A ping count is
%%% maintained and, once exhausted, causes the system to terminate.
%%%
%%% Both roles are gen_servers. Ping requests use gen_server:call/2 and
%%% replies are call results rather than direct sends.
%%%-------------------------------------------------------------------
-module(ping_pong_otp).
-behaviour(gen_server).

%% This module implements several roles, so its declared interface is the
%% union of theirs. See master_worker_roles_otp/ for the split version, in
%% which each module declares one role.
-interface ping_in() | pong_in().

-import(io, [format/2]).

-export([start_pong/0, start_ping/2, main/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([start/0, ping/0, pong/0, stop/0, ping_in/0, pong_in/0]).

%%% Messages and call replies.
-type start() :: {start}.
-type ping() :: {ping}.
-type pong() :: {pong}.
-type stop() :: {stop}.

%%% Process interfaces.
-type ping_in() :: start().
-type pong_in() :: #{call => ping(), cast => stop(), info => none()}.

%%% No receive loop: the declarations above say which calls and casts are valid.
%%% Defensive fallback callbacks are typed with term() so ordinary Erlang
%%% analysers can type-check their clauses.

-type role() :: {ping_role, pid(pong_in()), integer()} | pong_role.

%% @doc Starts the pong role that issues pong replies.
-spec start_pong() -> {ok, pid(pong_in())} | ignore | {error, term()}.
start_pong() ->
  gen_server:start_link(?MODULE, pong_role, []).

%% @doc Starts the ping role that issues ping requests.
-spec start_ping(pid(pong_in()), integer()) -> {ok, pid(ping_in())} | ignore | {error, term()}.
start_ping(Pong, PingsLeft) ->
  gen_server:start_link(?MODULE, {ping_role, Pong, PingsLeft}, []).

-spec init(role()) -> {ok, role()}.
init(Role) ->
  {ok, Role}.

%% @doc Handles start and stop messages.
-spec handle_cast(start() | stop(), role()) ->
        {noreply, role()} | {stop, normal, role()} |
        {stop, {unexpected_cast, term()}, role()}.
handle_cast({start}, {ping_role, Pong, PingsLeft} = State) ->
  ping_loop(Pong, PingsLeft),
  {stop, normal, State};
handle_cast({stop}, pong_role) ->
  {stop, normal, pong_role};
handle_cast(Msg, Role) ->
  {stop, {unexpected_cast, Msg}, Role}.

%% @doc Pong callback issuing pong replies.
-spec handle_call(ping(), {pid(), term()}, role()) ->
        {reply, pong(), role()} | {stop, {unexpected_call, term()}, role()}.
handle_call({ping}, _From, pong_role) ->
  format("Ponging~n", []),
  {reply, {pong}, pong_role};
handle_call(Msg, _From, Role) ->
  {stop, {unexpected_call, Msg}, Role}.

%% Defensive fallback: no plain send is part of either interface.
-spec handle_info(term(), role()) ->
        {stop, {unexpected_info, term()}, role()}.
handle_info(Msg, Role) ->
  {stop, {unexpected_info, Msg}, Role}.

%% @doc Ping main loop issuing ping requests.
-spec ping_loop(pid(pong_in()), integer()) -> ok.
ping_loop(Pong, PingsLeft) ->
  if PingsLeft > 0 ->
      format("Pinging ~p...~n", [PingsLeft]),
      %% Issue ping and await reply.
      {pong} = gen_server:call(Pong, {ping}),
      ping_loop(Pong, PingsLeft - 1);
    true ->
      %% No more pings to issue: notify ponger to stop.
      gen_server:cast(Pong, {stop})
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  {ok, Pong} = start_pong(),
  {ok, Ping} = start_ping(Pong, 5),
  gen_server:cast(Ping, {start}).

% NOTE: this file uses the proposed notation, pid(I) and -interface, neither of
% which currently parses. See encoded/ for the runnable form.
