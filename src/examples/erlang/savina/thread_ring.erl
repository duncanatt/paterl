%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/threadring
%%%
%%% A ring of actors modelling a cyclic workflow with a chain of tasks, where
%%% each actor decrements a token and forwards it until the token value reaches
%%% zero.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(thread_ring).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([actor/1]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Actor.
-type data() :: {data, actor_mb()}.
-type ping() :: {ping, integer()}.
-type exit() :: {exit, integer()}.

%%% Interfaces.

%% Actor.
-type actor_mb() :: pid() | data() | ping() | exit().

%%% Interface-function associations.

%% Actor.
-new({actor_mb, [main/0]}).
-new({actor_mb, [actor/1]}).
-use({actor_mb, [actor_loop/2, actor_exit/0]}).

% The two functions myfun/1 and myfun_loop/2 are associated with two mailboxes.
% We do not specify the usage modalities because these are inferred from the
% context: either a normal call = use, or a spawn, which must necessarily = new
% This leave the possibility (as in normal Erlang), to use functions in both
% spawn and invoke modalities.
-interface({[myfun/1, myfun_loop/2], [actor_mb/0, other_mb/0]}). % Not a good idea probably.

% How can I disambiguate if I have a function that is tied to both new an use. I can use spawn to say always new.
% But what about functions that are invoked but also require new? (see IRIF powerpoint presentation).
% Answer:
% 1. -new: Whenever there is a spawn = new, whenever there is an invoke = new
% 2. -use: Whenever there is a spawn = new, whenever there is an invoke = use
% Spawn always supersedes the new or use attribute and results in new.
% So we do need the usage attributes -new and -use but this avoids ?new and ?use
% macros in code if we stick to the above two conditions 1 and 2.

% Good.
-new1({function/1, [mb1, mb2]}).
-use1({function/1, [mb3]}).

% Bad.
-new1({function/1, [mb1, mb2]}).
-use2({function/1, [mb1]}). % mb1 is both new and use, which is wrong.

% Simple (with one mailbox, but for future extension to multiple mailboxes).
-new1({function/1, [mb1]}).
-use2({function/1, [mb2]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Actor process handling the launching of main loop.
%% @param NumActors: number of actors in ring.
-spec actor(integer()) -> no_return().
actor(NumActors) ->
  ?expects("Data . *Ping . *Exit"),
  receive
    {data, Neighbor} ->
      actor_loop(NumActors, Neighbor)
  end.

%% @doc Ping process main loop issuing pings and exits.
-spec actor_loop(integer(), actor_mb()) -> no_return().
actor_loop(NumActors, Neighbor) ->
  ?expects("*Ping . *Exit"),
  receive
    {ping, PingsLeft} ->
      if PingsLeft =< 0 ->
        Neighbor ! {exit, NumActors},
        actor_exit();
        true ->
          format("~p pinging ~p (~p)...~n", [self(), Neighbor, PingsLeft]),

          Neighbor ! {ping, PingsLeft - 1},
          actor_loop(NumActors, Neighbor)
      end;
    {exit, ExitsLeft} ->
      if ExitsLeft =< 0 -> ok;
        true ->
          Neighbor ! {ping, ExitsLeft - 1}
      end,
      actor_exit()
  end.

%% @doc Actor process exit procedure that flushes potential residual messages.
-spec actor_exit() -> no_return().
actor_exit() ->
  ?expects("*Ping . *Exit"),
  receive
    {ping, PingsLeft} ->
      actor_exit();
    {exit, ExitsLeft} ->
      actor_exit()
  after 0 ->
    format("~p actor exited.~n", [self()])
  end.

%% @doc Initializes ring of actors. The number of participants in the ring is
%% parametrized by 'NumActors'.
-spec init_ring(integer(), actor_mb()) -> ok.
init_ring(NumActors, Main) ->
  if NumActors < 2 ->
    % Cannot have a ring with less than two actors.
    ok;
    true ->
      % Create first mailbox and spawn corresponding actor.
%%      ?mb_new(actor_mb),
      FirstActor = spawn(?MODULE, actor, [NumActors]),

      % Create list of actors and close loop.
      TailActor = create_actors(NumActors - 2, NumActors, FirstActor),
      TailActor ! {data, FirstActor},

      % Notify main process of first actor mailbox.
      Main ! {data, FirstActor}
  end.

%% @doc Creates a series of actors, linking each actor to the one preceding it
%% by sending the address of its mailbox to the previous actor.
-spec create_actors(integer(), integer(), actor_mb()) -> actor_mb().
create_actors(Count, NumActors, PrevActor) ->
%%  ?mb_new(actor_mb),
  Actor = spawn(?MODULE, actor, [NumActors]),

  %% Link current actor to previous one.
  PrevActor ! {data, Actor},

  %% Create next actor.
  if Count < 0 ->
    %% All actors created.
    Actor;
    true ->
      create_actors(Count - 1, NumActors, Actor)
  end.

%% @doc Launcher
-spec main() -> any().
main() ->
  NumActors = 5,
  NumRounds = 5,

%%  ?mb_new(actor_mb),
  Self = self(),
  init_ring(NumActors, Self),

  ?expects("Data + 1"),
  receive
    {data, First_actor} ->
      First_actor ! {ping, NumRounds}
  end.


%% ./src/paterl src/examples/erlang/savina/thread_ring.erl -v all -I include
