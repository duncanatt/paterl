%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Dec 2023 17:45
%%%-------------------------------------------------------------------
-module(id_ts_clean).
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
%% @type id_client() :: {id, integer()} % Type more precise and one-to-one with server counterpart.
%% @type ts_client() :: {ts, integer()} % Type more precise and one-to-one with server counterpart.

%% @new id_server()
%% @spec id_server(integer) -> none()
id_server(Id) ->
  %% @mb id_server
  %% @assert new*
  receive
    {new, ReplyTo} ->
      ReplyTo ! {id, Id},
      id_server(Id + 1)
  end.

%% @new ts_server()
%% @spec ts_server() -> none()
ts_server() ->
  %% @mb ts_server
  %% @assert now*
  receive
    {now, ReplyTo} ->
      ReplyTo ! {ts, now()},
      ts_server()
  end.

%% @new id_client()
%% @spec id_rpc(id_server()) -> integer().
id_rpc(To) ->
  To ! {new, self()},
  %% @mb id_client
  %% @assert id
  receive
    {id, Id} -> Id
  end.

%% @new ts_client()
%% @spec ts_asy(ts_server()) -> none().
ts_asy(To) ->
  To ! {now, self()},
  ok.

%% @use ts_client()
%% @spec ts_get() -> integer().
ts_get() ->
  %% @mb ts_client
  %% @assert ts
  receive
    {ts, Ts} -> Ts
  end.

client() ->
  IdServer = spawn(?MODULE, id_server, [0]),
  TsServer = spawn(?MODULE, ts_server, []),

  ts_asy(TsServer), % Isolated channels of communication.
  ts_asy(TsServer), % Isolated channels of communication.
  Id = id_rpc(IdServer), % Isolated channels of communication. (free of interleaving in this case)
  Ts1 = ts_get(),
  Ts2 = ts_get(),

  format("Id and time: ~p/~p/~p~n", [Id, Ts1, Ts2]).



