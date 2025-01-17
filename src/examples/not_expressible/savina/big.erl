%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% A benchmark that implements a many-to-many message passing scenario. Several
%%% processes are spawned, each of which sends a ping message to the others, and
%%% responds with a pong message to any ping message it receives. The benchmark
%%% is parameterized by the number of processes. Since the array type is not
%%% available in Pat, we fix the number of processes to 3.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(big).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).
-import(rand, [uniform/1]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([actor/2, sink/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Actor.
-type ping() :: {ping, integer()}.
-type pong() :: {pong, integer()}.
-type neighbors() :: {neighbors, actor_mb(), actor_mb()}.

%% Exit.
-type exit() :: {exit}.

%% Sink.
-type done() :: {done}.
-type actors() :: {actors, exit_mb(), exit_mb(), exit_mb()}.

%%% Interfaces.

%% Actor.
-type actor_mb() :: pid() | ping() | pong() | neighbors().

%% Exit.
-type exit_mb() :: pid() | exit().

%% Sink.
-type sink_mb() :: pid() | done() | actors().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Actor.
-new({actor_mb, [actor/2]}).
-use({actor_mb, [actor_loop/5, actor_exit/0]}).

%% Exit.
-new({exit_mb, [await_exit/0]}).

%% Sink.
-new({sink_mb, [sink/0]}).
-use({sink_mb, [sink_loop/3]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%%  def actor(self: ActorMb?, exitMb : ExitMb?, id: Int, sinkMb: SinkMb!): Unit {
%%    guard self: Neighbors . *(Pong + Ping)  {
%%      receive Neighbors(actorMb1, actorMb2) from self ->
%%        actor_loop(self, exitMb, id, sinkMb, 100, actorMb1, actorMb2)
%%    }
%%  }
-spec actor(integer(), sink_mb()) -> no_return().
actor(Id, Sink) ->
  ?expects("Neighbors . *(Pong + Ping)"),
  receive
    {neighbors, Actor1, Actor2} ->
      actor_loop(Id, Sink, 100, Actor1, Actor2)
  end.

%%  def await_exit(exitMb: ExitMb?): Unit {
%%      guard exitMb : Exit {
%%          receive Exit() from exitMb ->
%%            free(exitMb)
%%      }
%%  }
-spec await_exit() -> integer().
await_exit() ->
  ?expects("Exit"),
  receive
    {exit} ->
%%    TODO: Not sure about free
      5
  end.

%%  def actor_loop(self: ActorMb?, exitMb: ExitMb?, id:Int, sinkMb: SinkMb!, numPings: Int, actorMb1: ActorMb!, actorMb2: ActorMb!): Unit {
%%    guard self: *(Ping + Pong) {
%%      free ->
%%        await_exit(exitMb)
%%      receive Ping(pingerId) from self ->
%%
%%        # Reply to ping.
%%        send_pong(id, pingerId, actorMb1, actorMb2);
%%        actor_loop(self, exitMb, id, sinkMb, numPings, actorMb1, actorMb2)
%%
%%      receive Pong(pongerId) from self ->
%%
%%        if (numPings <= 0) {
%%
%%          # No more pongs to issue.
%%          sinkMb ! Done();
%%          actor_exit(self);
%%          await_exit(exitMb)
%%        }
%%        else {
%%
%%          # Issue ping to random participant.
%%          send_ping(id, actorMb1, actorMb2);
%%          actor_loop(self, exitMb, id, sinkMb, numPings - 1, actorMb1, actorMb2)
%%        }
%%    }
%%  }
-spec actor_loop(integer(), sink_mb(), integer(), actor_mb(), actor_mb()) -> no_return().
actor_loop(Id, Sink, Num_pings, Actor1, Actor2) ->
  ?expects("*(Ping + Pong)"),
  receive
    {ping, Pinger_id} ->
      send_pong(Id, Pinger_id, Actor1, Actor2),
      actor_loop(Id, Sink, Num_pings, Actor1, Actor2);

    {pong, Ponger_id} ->
      if Num_pings =< 0 ->
        Sink ! {done},
        actor_exit(),
        await_exit();
        true ->
          % Issue ping to random participant
          send_ping(Id, Actor1, Actor2),
          actor_loop(Id, Sink, Num_pings - 1, Actor1, Actor2)
      end
  end.

%%  def actor_exit(self: ActorMb?): Unit {
%%    guard self: (*Ping) . (*Pong) {
%%      free -> ()
%%      receive Ping(pingerId) from self ->
%%        actor_exit(self)
%%      receive Pong(pongerId) from self ->
%%        actor_exit(self)
%%    }
%%  }
-spec actor_exit() -> no_return().
actor_exit() ->
  ?expects("(*Ping) . (*Pong)"),
  receive
    {ping, Pinger_id} ->
      actor_exit();
    {pong, Ponger_id} ->
      actor_exit()
  end.

%%  def send_pong(id: Int, pingerId: Int, actorMb1: ActorMb!, actorMb2: ActorMb!): Unit {
%%    if (pingerId == 1) {
%%      actorMb1 ! Pong(id)
%%    }
%%    else {
%%      actorMb2 ! Pong(id)
%%    }
%%  }
-spec send_pong(integer(), integer(), actor_mb(), actor_mb()) -> no_return().
send_pong(Id, Pinger_id, Actor1, Actor2) ->
  if Pinger_id == 1 ->
    Actor1 ! {pong, Id};
    true ->
      Actor2 ! {pong, Id}
  end.

%%  def send_ping(id: Int, actorMb1: ActorMb!, actorMb2: ActorMb!): Unit {
%%
%%    let pongerId = rand(2) in
%%
%%    if (pongerId == 1) {
%%      actorMb1 ! Ping(id)
%%    }
%%    else {
%%      actorMb2 ! Ping(id)
%%    }
%%  }
-spec send_ping(integer(), actor_mb(), actor_mb()) -> no_return().
send_ping(Id, Actor1, Actor2) ->
  PongerId = uniform(2),
  if PongerId == 1 ->
    Actor1 ! {ping, Id};
    true ->
      Actor2 ! {ping, Id}
  end.

%%  def sink(self: SinkMb?): Unit {
%%    guard self: Actors . (*Done) {
%%      receive Actors(exitMb1, exitMb2, exitMb3) from self ->
%%        sink_loop(self, exitMb1, exitMb2, exitMb3)
%%    }
%%  }
-spec sink() -> no_return().
sink() ->
  ?expects("Actors . (*Done)"),
  receive
    {actors, Exit1, Exit2, Exit3} ->
      sink_loop(Exit1, Exit2, Exit3)
  end.

%%  def sink_loop(self: SinkMb?, exitMb1: ExitMb!, exitMb2: ExitMb!, exitMb3: ExitMb!): Unit {
%%    guard self: *Done {
%%      free ->
%%          exitMb1 ! Exit();
%%          exitMb2 ! Exit();
%%          exitMb3 ! Exit()
%%      receive Done() from self ->
%%          sink_loop(self, exitMb1, exitMb2, exitMb3)
%%    }
%%  }
-spec sink_loop(exit_mb(), exit_mb(), exit_mb()) -> no_return().
sink_loop(Exit1, Exit2, Exit3) ->
%% TODO: Not sure about free
  ?expects("*Done"),
  receive
    {done} ->
      sink_loop(Exit1, Exit2, Exit3)
  end.

%%  def main(): Unit {
%%
%%    let sinkMb = new [SinkMb] in
%%    spawn { sink(sinkMb) };
%%
%%    let actorMb1 = new [ActorMb] in # actorMb1: ?1
%%    let actorMb2 = new [ActorMb] in # actorMb2: ?1
%%    let actorMb3 = new [ActorMb] in # actorMb3: ?1
%%    let exitMb1 = new [ExitMb] in # exitMb1: ?1
%%    let exitMb2 = new [ExitMb] in # exitMb2: ?1
%%    let exitMb3 = new [ExitMb] in # exitMb3: ?1
%%
%%    spawn { actor(actorMb1, exitMb1, 1, sinkMb) };
%%    spawn { actor(actorMb2, exitMb2, 2, sinkMb) };
%%    spawn { actor(actorMb3, exitMb3, 3, sinkMb) };
%%
%%    sinkMb ! Actors(exitMb1, exitMb2, exitMb3);
%%
%%    actorMb1 ! Neighbors(actorMb2, actorMb3); # actorMb1: ?Neighbors
%%    actorMb2 ! Neighbors(actorMb1, actorMb3); # actorMb2: ?Neighbors
%%    actorMb3 ! Neighbors(actorMb1, actorMb2); # actorMb3: ?Neighbors
%%
%%    actorMb1 ! Pong(0); # actorMb1: ?Neighbors . Pong
%%    actorMb2 ! Pong(0); # actorMb2: ?Neighbors . Pong
%%    actorMb3 ! Pong(0) # actorMb2: ?Neighbors . Pong
%%  }
-spec main() -> no_return().
main() ->
  Sink = spawn(?MODULE, sink, []),
  Actor1 = spawn(?MODULE, actor, [1, Sink]),
  Actor2 = spawn(?MODULE, actor, [2, Sink]),
  Actor3 = spawn(?MODULE, actor, [3, Sink]),

  Sink ! {actor, Actor1, Actor2, Actor3},

  Actor1 ! {neighbors, Actor2, Actor3},
  Actor2 ! {neighbors, Actor1, Actor3},
  Actor3 ! {neighbors, Actor1, Actor2},

  Actor1 ! {pong, 0},
  Actor2 ! {pong, 0},
  Actor3 ! {pong, 0}.
