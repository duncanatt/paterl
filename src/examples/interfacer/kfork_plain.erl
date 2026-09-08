%%%-------------------------------------------------------------------
%%% Plain Interfacer version of Savina kfork.
%%%
%%% Adapted from Savina/fjthrput.
%%% Models the creation of n actors that are given a number of messages, for
%%% each of which a computation is performed. The benchmark is parameterized by
%%% the number of actor processes. Since the array type is not available in
%%% Pat, the original fixes the number of actor processes to 3.
%%%-------------------------------------------------------------------
-module(kfork_plain).

-import(io, [format/2]).

-export([main/0]).
-export([actor/0]).
-export_type([packet/0, actor_in/0]).

%%% Messages.
-type packet() :: {packet}.
-type stop() :: {stop}.

%%% Interfaces.
-type actor_in() :: packet() | stop().

%% @doc Actor process entry point for handling packet requests.
-interface actor :: actor_in().
-spec actor() -> ok.
actor() ->
  actor_loop().

%% @doc Actor process loop handling packet requests.
-spec actor_loop() -> ok.
actor_loop() ->
  receive
    {packet} ->
      Self = self(),
      format("~p Received packet.~n", [Self]),
      actor_loop();
    {stop} ->
      actor_exit()
  end.

%% @doc Flushes any packets left in the mailbox and exits.
%% The after 0 belongs here, not in actor_loop/0. With it in the loop the actor
%% almost always finds an empty mailbox on entry, since main/0 spawns before it
%% floods, and exits before any packet arrives. ping_pong_plain and
%% master_worker_plain separate the two for the same reason.
-spec actor_exit() -> ok.
actor_exit() ->
  receive
    {packet} ->
      actor_exit();
    {stop} ->
      actor_exit()
  after 0 ->
    format("Actor exited.~n", [])
  end.

%% @doc Sends the given number of messages to the specified actor process.
-spec flood(integer(), pid(actor_in())) -> ok.
flood(NumMessages, Actor) ->
  if NumMessages =< 0 ->
      Actor ! {stop},
      ok;
    true ->
      Actor ! {packet},
      flood(NumMessages - 1, Actor)
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Actor1 = spawn(?MODULE, actor, []),
  Actor2 = spawn(?MODULE, actor, []),
  Actor3 = spawn(?MODULE, actor, []),
  flood(5, Actor1),
  flood(10, Actor2),
  flood(15, Actor3).

% NOTE: this file uses the proposed notation, pid(I) and -interface, neither of
% which currently parses. See encoded/ for the runnable form.
