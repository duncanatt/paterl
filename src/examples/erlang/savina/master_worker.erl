%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2023 12:21
%%%
%%% This is a simpler example that combines the farm and harvest functions so
%%% that the pool mailbox is shared and does not extend over different function
%%% invocations.
%%%-------------------------------------------------------------------
-module(master_worker).
-author("duncan").

%% API
-export([]).

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([master/0, worker/0, client/2]).

%%% Type definitions.

%% Master interface.
%% @type master_mb() :: {task, client_mb(), integer()}

%% Pool interface.
%% @type pool_mb() :: {result, integer() }

%% Worker interface.
%% @type worker_mb() :: {work, pool_mb(), integer()}

%% Client interface.
%% @type client_mb() :: {result, integer()}

%% @spec master() -> none()
%% @new master_mb()
master() ->
  %% @mb master_mb()
  %% @assert task*
  receive
    {task, ReplyTo, Task} ->
      format("= Master received task '~p' from client ~n", [Task]),

      Result =
        %% @new pool_mb()
        farm_and_harvest(Task),
      ReplyTo ! {result, Result},

      %% @use master_mb() %% Can be detected by a fixed point computation..maybe. This is required to determine that in the recursive call, the mailbox is @use, and not @new now!
      master()
  end.

%% @spec farm(integer()) -> integer()
%% @new pool_mb()
farm_and_harvest(Chunks) ->
  _Dummy =
    %% @use pool_mb()
    farm(Chunks),

  %% @use pool_mb()
  harvest(Chunks, 0).

%% @spec farm(integer()) -> none()
%% @use pool_mb()
farm(Chunk) ->
  if Chunk == 0 ->
    format("= Farming complete~n", []),
    ok;
    true ->
      format("= Farming chunk ~p~n", [Chunk]),
      WorkerPid =
        %% @new worker_mb()
        spawn(?MODULE, worker, []),

      Self =
        %% @mb pool_mb()
        self(),
      WorkerPid ! {work, Self, Chunk},
      Next = Chunk - 1,

      %% @use pool_mb()
      farm(Next)
  end.

%% @spec harvest(integer(), integer()) -> integer()
%% @use pool_mb()
harvest(Chunk, Acc) ->
  if Chunk == 0 ->
    format("= Harvesting complete~n", []),
    Acc;
    true ->
      %% @mb pool_mb
      %% @assert result*
      receive
        {result, N} ->
          format("= Harvested task ~p with result '~p'~n", [Chunk, N]),
          Next = Chunk - 1,

          %% @use pool_mb()
          harvest(Next, Acc + N)
      end
  end.

%% @spec compute(integer()) -> integer()
compute(N) ->
  N * N.

%% @spec worker() -> none()
%% @new worker_mb()
worker() ->
  %% @mb worker_mb()
  %% @assert Work
  receive
    {work, ReplyTo, Chunk} ->
      format("= Worker computing chunk ~p~n", [Chunk]),
      Result = compute(Chunk),
      ReplyTo ! {result, Result}
  end.

%% @spec client(integer(), master_mb()) -> none()
%% @new client_mb()
client(Task, MasterPid) ->
  format("= Started client~n", []),
  Self =
    %% @mb client_mb()
    self(),
  MasterPid ! {task, Self, Task},

  %% @mb client_mb()
  %% @assert result
  receive
    {result, Result} ->
      format("Result: '~p'~n", [Result])
  end.

%% @spec main() -> client_mb()
%% @new client_mb()
main() ->
  MasterPid =
    %% @new master_mb()
    spawn(?MODULE, master, []),
  %% @new client_mb()
  spawn(?MODULE, client, [5, MasterPid]).



