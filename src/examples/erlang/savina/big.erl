%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% A benchmark that implements a many-to-many message passing scenario. Several
%%% processes are spawned, each of which sends a ping message to the others, and
%%% responds with a pong message to any ping message it receives. The benchmark
%%% is parameterized by the number of processes. Since the array type is not
%%% available in Pat, we fix the number of processes to 3.
%%%
%%% Single-mailbox adaptation of the pat-lang big example. The original actor
%%% owns two mailboxes (the actor mailbox and a separate exit mailbox), which
%%% paterl cannot currently overlay on the mailbox of a single Erlang process.
%%% See src/examples/not_expressible/savina/big.erl for the original sketch.
%%%
%%% Changes with respect to the two-mailbox original:
%%% 1. The exit_mb interface is dropped and its exit message is delivered to
%%%    the actor mailbox instead. Actor mailbox assertions gain a terminal
%%%    Exit, e.g. "(Ping + Pong)* . Exit" in actor_loop.
%%% 2. The sink holds actor mailbox references rather than exit mailbox
%%%    references, and notifies the actors directly once all done messages
%%%    are consumed (in the receive after clause, so that each actor is
%%%    notified exactly once).
%%% 3. Since Pat patterns are commutative, the exit message may be dequeued
%%%    before residual pings and pongs. Both exit procedures therefore keep
%%%    draining after consuming exit: actor_exit awaits the exit message while
%%%    flushing, and actor_flush flushes the leftovers that follow it.
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
-type exit() :: {exit}.

%% Sink.
-type done() :: {done}.
-type actors() :: {actors, actor_mb(), actor_mb(), actor_mb()}.

%%% Interfaces.

%% Actor.
-type actor_mb() :: pid() | ping() | pong() | neighbors() | exit().

%% Sink.
-type sink_mb() :: pid() | done() | actors().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Actor.
-new({actor_mb, [actor/2]}).
-use({actor_mb, [actor_loop/5, actor_exit/0, actor_flush/0]}).

%% Sink.
-new({sink_mb, [sink/0]}).
-use({sink_mb, [sink_loop/3]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Actor process handling the launching of the main loop.
-spec actor(integer(), sink_mb()) -> no_return().
actor(Id, Sink) ->
  ?expects("Neighbors . (Ping + Pong)* . Exit"),
  receive
    {neighbors, Actor1, Actor2} ->
      actor_loop(Id, Sink, 100, Actor1, Actor2)
  end.

%% @doc Actor process main loop issuing ping requests and handling pong
%% replies.
-spec actor_loop(integer(), sink_mb(), integer(), actor_mb(), actor_mb()) -> no_return().
actor_loop(Id, Sink, Num_pings, Actor1, Actor2) ->
  ?expects("(Ping + Pong)* . Exit"),
  receive
    {ping, Pinger_id} ->
      % Reply to ping.
      send_pong(Id, Pinger_id, Actor1, Actor2),
      actor_loop(Id, Sink, Num_pings, Actor1, Actor2);
    {pong, Ponger_id} ->
      if Num_pings =< 0 ->
        % No more pings to issue.
        Sink ! {done},
        actor_exit();
        true ->
          % Issue ping to random participant.
          send_ping(Id, Actor1, Actor2),
          actor_loop(Id, Sink, Num_pings - 1, Actor1, Actor2)
      end;
    {exit} ->
      % Sink terminated the actor early; flush residual messages.
      actor_flush()
  end.

%% @doc Actor process exit procedure that flushes residual messages and awaits
%% the termination message.
-spec actor_exit() -> no_return().
actor_exit() ->
  ?expects("Ping* . Pong* . Exit"),
  receive
    {ping, Pinger_id} ->
      actor_exit();
    {pong, Ponger_id} ->
      actor_exit();
    {exit} ->
      % Residual pings and pongs may still follow the exit message.
      actor_flush()
  end.

%% @doc Actor process exit procedure that flushes residual messages after the
%% termination message is received.
-spec actor_flush() -> no_return().
actor_flush() ->
  ?expects("Ping* . Pong*"),
  receive
    {ping, Pinger_id} ->
      actor_flush();
    {pong, Ponger_id} ->
      actor_flush()
  after 0 ->
    format("Actor exited.~n", [])
  end.

%% @doc Replies to ping messages via a pong issued to the specified actor ID.
-spec send_pong(integer(), integer(), actor_mb(), actor_mb()) -> no_return().
send_pong(Id, Pinger_id, Actor1, Actor2) ->
  if Pinger_id == 1 ->
    Actor1 ! {pong, Id};
    true ->
      Actor2 ! {pong, Id}
  end.

%% @doc Randomly issues a ping message to one of the participating actors.
-spec send_ping(integer(), actor_mb(), actor_mb()) -> no_return().
send_ping(Id, Actor1, Actor2) ->
  Ponger_id = uniform(2),
  if Ponger_id == 1 ->
    Actor1 ! {ping, Id};
    true ->
      Actor2 ! {ping, Id}
  end.

%% @doc Sink process that coordinates actor termination.
-spec sink() -> no_return().
sink() ->
  ?expects("Actors . Done*"),
  receive
    {actors, Actor1, Actor2, Actor3} ->
      sink_loop(Actor1, Actor2, Actor3)
  end.

%% @doc Sink process main loop issuing the termination messages.
-spec sink_loop(actor_mb(), actor_mb(), actor_mb()) -> no_return().
sink_loop(Actor1, Actor2, Actor3) ->
  ?expects("Done*"),
  receive
    {done} ->
      sink_loop(Actor1, Actor2, Actor3)
  after 100 ->
    % Notify all actors. Placing the sends in this clause ensures that each
    % actor is notified once.
    Actor1 ! {exit},
    Actor2 ! {exit},
    Actor3 ! {exit}
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Sink = spawn(?MODULE, sink, []),
  Actor1 = spawn(?MODULE, actor, [1, Sink]),
  Actor2 = spawn(?MODULE, actor, [2, Sink]),
  Actor3 = spawn(?MODULE, actor, [3, Sink]),

  Sink ! {actors, Actor1, Actor2, Actor3},

  Actor1 ! {neighbors, Actor2, Actor3},
  Actor2 ! {neighbors, Actor1, Actor3},
  Actor3 ! {neighbors, Actor1, Actor2},

  Actor1 ! {pong, 0},
  Actor2 ! {pong, 0},
  Actor3 ! {pong, 0},
  ok.


%% ./src/paterl src/examples/erlang/savina/big.erl -v all -I include
