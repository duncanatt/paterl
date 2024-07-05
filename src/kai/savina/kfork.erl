%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 18:15
%%%-------------------------------------------------------------------
-module(kfork).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([actor/0]).

%% Mailbox interface-function associations.
-new({actor_mb, [actor/0]}).

%% ActorMb's message types
-type packet() :: {packet}.

%% Interface ActorMb {
%%    Packet()
%% }
-type actor_mb() :: pid() | packet().

%% def actor(self: ActorMb?): Unit {
%%   guard self: *Packet {
%%     free ->
%%       ()
%%     receive Packet() from self ->
%%       let dummy = fact(rand(100000)) in
%%       actor(self)
%%   }
%% }
-spec actor() -> no_return().
actor() ->
  ?mb_assert_regex("*Packet"),
  receive
    {packet} ->
      Dummy = fact(rand:uniform(100000)),
      actor()
  end.

%% Computes the factorial of n.
%% def fact(n: Int): Int {
%%   if (n <= 0) {
%%     1
%%   }
%%   else {
%%     n * (fact(n - 1))
%%   }
%% }
-spec fact(integer()) -> integer().
fact(N) ->
  if N =< 0 -> 1;
    true -> N * fact(N - 1)
  end.

%% Sends the given number of messages to the specified actor mailbox.
%% def flood(numMessages: Int, actorMb: ActorMb!): Unit {
%%   if (numMessages <= 0) {
%%     ()
%%   }
%%   else {
%%     actorMb ! Packet();
%%     flood(numMessages - 1, actorMb)
%%   }
%% }
-spec flood(integer(), actor_mb()) -> no_return().
flood(Num_messages, Actor) ->
  if Num_messages =< 0 -> ok;
    true ->
      Actor ! {packet},
      flood(Num_messages - 1, Actor)
  end.

%% Launcher.
%% def main(): Unit {
%%   let actorMb1 = new [ActorMb] in
%%   spawn { actor(actorMb1) };
%%
%%   let actorMb2 = new [ActorMb] in
%%   spawn { actor(actorMb2) };
%%
%%   let actorMb3 = new [ActorMb] in
%%   spawn { actor(actorMb3) };
%%   TODO Maybe wrong in Pat
%%   flood(100, actorMb1);
%%   flood(1000, actorMb2);
%%   flood(10000, actorMb3);
%% }
-spec main() -> no_return().
main() ->
  ?mb_new(actor_mb),
  Actor1 = spawn(?MODULE, actor, []),

  ?mb_new(actor_mb),
  Actor2 = spawn(?MODULE, actor, []),

  ?mb_new(actor_mb),
  Actor3 = spawn(?MODULE, actor, []),

  flood(100, Actor1),
  flood(1000, Actor2),
  flood(10000, Actor3).
