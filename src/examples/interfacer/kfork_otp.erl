%%%-------------------------------------------------------------------
%%% OTP-style Interfacer version of Savina kfork.
%%%
%%% Adapted from Savina/fjthrput.
%%% Models the creation of n actors that are given a number of messages, for
%%% each of which a computation is performed. The benchmark is parameterized by
%%% the number of actor processes. Since the array type is not available in
%%% Pat, the original fixes the number of actor processes to 3.
%%%-------------------------------------------------------------------
-module(kfork_otp).
-behaviour(gen_server).

-interface actor_in().

-import(io, [format/2]).

-export([start_actor/0, main/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export_type([packet/0, actor_in/0]).

%%% Messages.
-type packet() :: {packet}.

%%% Interfaces.
-type actor_in() :: #{call => none(), cast => packet(), info => none()}.

%%% No receive loop: the declarations above say which calls and casts are valid.

%% @doc Starts an actor process handling packet requests.
-spec start_actor() -> {ok, pid(actor_in())} | ignore | {error, term()}.
start_actor() ->
  gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, []}.
init([]) ->
  {ok, []}.

%% @doc Actor callback handling packet requests.
-spec handle_cast(packet(), []) -> {noreply, []}.
handle_cast({packet}, State) ->
  Self = self(),
  format("~p Received packet.~n", [Self]),
  {noreply, State}.

%% Defensive fallbacks: no call or plain send is part of actor_in.
-spec handle_call(term(), {pid(), term()}, []) ->
        {stop, {unexpected_call, term()}, []}.
handle_call(Msg, _From, State) ->
  {stop, {unexpected_call, Msg}, State}.

-spec handle_info(term(), []) ->
        {stop, {unexpected_info, term()}, []}.
handle_info(Msg, State) ->
  {stop, {unexpected_info, Msg}, State}.

%% @doc Sends the given number of messages to the specified actor process.
-spec flood(integer(), pid(actor_in())) -> ok.
flood(NumMessages, Actor) ->
  if NumMessages =< 0 ->
      ok;
    true ->
      gen_server:cast(Actor, {packet}),
      flood(NumMessages - 1, Actor)
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  {ok, Actor1} = start_actor(),
  {ok, Actor2} = start_actor(),
  {ok, Actor3} = start_actor(),
  flood(5, Actor1),
  flood(10, Actor2),
  flood(15, Actor3).

% NOTE: this file uses the proposed notation, pid(I) and -interface, neither of
% which currently parses. See encoded/ for the runnable form.
