%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/fjthrput.
%%%
%%% Models the creation of n actors that are given a number of messages, for
%%% each of which, a computation is performed. The benchmark is parameterized by
%%% the number of actor processes. Since the array type is not available in
%%% Pat, we fix the number of actor processes to 3.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(kfork_dir_rec).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([actor/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Actor.
-type packet() :: {packet}.

%%% Interfaces.

%% Actor.
-type actor_mb() :: pid() | packet().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Actor.
-new({actor_mb, [actor/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Actor processes handling the packet requests.
-spec actor() -> no_return().
actor() ->
  ?expects("*Packet"),
  receive
    {packet} ->
      format("~p Received packet.~n", [self()]),
      actor()
  after 0 ->
    format("Actor exited.~n", [])
  end.

%% @doc Sends the given number of messages to the specified actor mailbox.
-spec flood(integer(), actor_mb()) -> no_return().
flood(Num_messages, Actor) ->
  if Num_messages =< 0 -> ok;
    true ->
      Actor ! {packet},
      flood(Num_messages - 1, Actor)
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


%% ./src/paterl src/examples/erlang/savina/kfork_dir_rec.erl -v all -I include