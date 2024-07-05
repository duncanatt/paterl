%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 6月 2024 09:17
%%%-------------------------------------------------------------------
-module(thread_ring).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/2]).

-export([actor/1]).

%% Mailbox interface-function associations.
-new({actor_mb, [main/2]}).
-use({actor_mb, [actor/1, actor_loop/2, actor_exit/0]}).

%% ActorMb's message types
-type data() :: {data, actor_mb()}.
-type ping() :: {ping, integer()}.
-type exit() :: {exit, integer()}.

%% Interface ActorMb {
%%    Data(ActorMb!),
%%    Ping(Int),
%%    Exit(Int)
%% }
-type actor_mb() :: pid() | data() | ping() | exit().

%% def actor(self: ActorMb?, numActors: Int): Unit {
%%  guard self: Data . (*Ping) . (*Exit) {
%%    receive Data(neighborMb) from self ->
%%      actor_loop(self, numActors, neighborMb)
%%  }
%%}
-spec actor(integer()) -> no_return().
actor(Num_actors) ->
  ?mb_assert_regex("Data . (*Ping) . (*Exit)"),
  receive
    {data, Neighbor} ->
      actor_loop(Num_actors, Neighbor)
  end.

%% def actor_loop(self: ActorMb?, numActors: Int, neighborMb: ActorMb!): Unit {
%%
%%  guard self: (*Ping) . (*Exit) {
%%    free -> ()
%%    receive Ping(pingsLeft) from self ->
%%      if (pingsLeft <= 0) {
%%        neighborMb ! Exit(numActors);
%%        actor_exit(self)
%%      }
%%      else {
%%        neighborMb ! Ping(pingsLeft - 1);
%%        actor_loop(self, numActors, neighborMb)
%%      }
%%    receive Exit(exitsLeft) from self ->
%%      if (exitsLeft <= 0) {
%%        ()
%%      }
%%      else{
%%        neighborMb ! Ping(exitsLeft - 1)
%%      };
%%      actor_exit(self)
%%  }
%%}
-spec actor_loop(integer(), actor_mb()) -> no_return().
actor_loop(Num_actors, Neighbor) ->
  ?mb_assert_regex("(*Ping) . (*Exit)"),
  receive
    {ping, Pings_left} ->
      if Pings_left =< 0 ->
        Neighbor ! {exit, Num_actors},
        actor_exit();
        true ->
          Neighbor ! {ping, Pings_left - 1},
          actor_loop(Num_actors, Neighbor)
      end;
    {exit, Exits_left} ->
      if Exits_left =< 0 -> ok;
        true ->
          Neighbor ! {ping, Exits_left - 1}
      end,
      actor_exit()
  end.

%% def actor_exit(self: ActorMb?): Unit {
%%  guard self: *Ping . (*Exit) {
%%    free -> ()
%%    receive Ping(pingsLeft) from self ->
%%      actor_exit(self)
%%    receive Exit(exitsLeft) from self ->
%%      actor_exit(self)
%%  }
%%}
-spec actor_exit() -> no_return().
actor_exit() ->
  ?mb_assert_regex("*Ping . (*Exit)"),
  receive
    {ping, Pings_left} ->
      actor_exit();
    {exit, Exits_left} ->
      actor_exit()
  end.

%% def init_ring(numActors: Int, mainMb: ActorMb!): Unit {
%%
%%  if (numActors < 2) {
%%    # Cannot have a ring with less than two actors.
%%    ()
%%  }
%%  else {
%%    let firstActorMb = new [ActorMb] in
%%    spawn { actor(firstActorMb, numActors) };

%%    let tailActorMb = create_actors(numActors - 2, numActors, firstActorMb) in
%%    tailActorMb ! Data(firstActorMb);
%%
%%    mainMb ! Data(firstActorMb)
%%  }
%%}
-spec init_ring(integer(), actor_mb()) -> ok.
init_ring(Num_actors, Main) ->
  if Num_actors < 2 ->
    ok;
    true ->
      ?mb_new(actor_mb),
      First_actor = spawn(?MODULE, actor, [Num_actors]),

      Tail_actor = create_actors(Num_actors - 2, Num_actors, First_actor),
      Tail_actor ! {data, First_actor},

      Main ! {data, First_actor}
  end.

%% def create_actors(count: Int, numActors: Int, prevActorMb: ActorMb!): ActorMb![R] {
%%
%%  let actorMb = new [ActorMb] in
%%  spawn { actor(actorMb, numActors) };

%%  prevActorMb ! Data(actorMb);
%%
%%  if (count < 0) {
%%    actorMb
%%  }
%%  else {
%%    create_actors(count - 1, numActors, actorMb)
%%  }
%%}
-spec create_actors(integer(), integer(), actor_mb()) -> actor_mb().
create_actors(Count, Num_actors, Prev_actor) ->
  ?mb_new(actor_mb),
  Actor = spawn(?MODULE, actor, [Num_actors]),

  %% Link current actor to previous one.
  Prev_actor ! {data, Actor},

  %% Create next actor.
  if Count < 0 ->
    %% All actors created.
    Actor;
    true ->
      create_actors(Count - 1, Num_actors, Actor)
  end.

%% def main(numActors: Int, numRounds: Int): Unit {
%%  let mainMb = new [ActorMb] in
%%  init_ring(numActors, mainMb);
%%
%%  guard mainMb: Data + 1 {
%%    free -> ()
%%    receive Data(firstActorMb) from mainMb ->
%%      firstActorMb ! Ping(numRounds);
%%      free(mainMb)
%%  }
%%}
-spec main(integer(), integer()) -> no_return().
main(Num_actors, Num_rounds) ->
  ?mb_new(actor_mb),
  Self = self(),
  init_ring(Num_actors, Self),

  ?mb_assert_regex("Data + 1"),
  receive
    {data, First_actor} ->
      First_actor ! {ping, Num_rounds}
  end.

