%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Dec 2023 17:45
%%%-------------------------------------------------------------------
-module(id_ts_type_pollution).
-author("duncan").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([client/0]).

%%% Internal exports.
-export([id_server/1, ts_server/0]).

%%% Type definitions.

%% @type id_server() :: {new, client()}
%% @type ts_server() :: {now, client()}
%% @type client() :: {id, integer()} | {ts, integer()} % Type is less precise.

%% @new id_server()
%% @spec id_server(integer) -> none()
id_server(Id) ->
  %% @assert new*
  receive
    {new, ReplyTo} ->
      ReplyTo ! {id, Id},
      id_server(Id + 1)
  end.

%% @new ts_server()
%% @spec ts_server() -> none()
ts_server() ->
  %% @assert now*
  receive
    {now, ReplyTo} ->
      ReplyTo ! {ts, now()},
      ts_server()
  end.

%% @spec id_rpc(id_server()) -> integer()
id_rpc(To) ->
  To ! {new, self()},
  %% @assert id.ts.ts
  %% @assert id.ts (ts_get called first)
  %% @assert id (ts_get * 2 called first )
  receive
    {id, Id} -> Id
  end.

%% @spec ts_asy(ts_server()) -> none()
ts_asy(To) ->
  To ! {now, self()},
  ok.

%% @spec ts_get() -> integer().
ts_get() ->
  %% @assert ts OR ts.id (depending on which invocation is first)

  %% @assert ts.ts.id
  %% @assert ts.id
  %% @assert ts.ts
  %% @assert ts
  receive
    {ts, Ts} -> Ts
  end.

client() ->
  IdServer = spawn(?MODULE, id_server, [0]),
  TsServer = spawn(?MODULE, ts_server, []),

  ts_asy(TsServer),
  ts_asy(TsServer),
  Id = id_rpc(IdServer),
  Ts1 = ts_get(),
  Ts2 = ts_get(),

  format("Id and time: ~p/~p/~p~n", [Id, Ts1, Ts2]).




