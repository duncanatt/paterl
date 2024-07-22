%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2023 17:41
%%%-------------------------------------------------------------------
-module(id_ts_server_demo).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).
-import(erlang, [system_time/0]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/0]).
-export([ts_server/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% ID server.
-type init() :: {init, integer()}.
-type get() :: {get, id_client_mb()}.
-type id() :: {id, integer()}.

%% TS server.
-type now() :: {now, ts_client_mb()}.
-type ts() :: {ts, integer()}.

%%% Interfaces.

%% ID server and client.
-type id_server_mb() :: pid() | init() | get().
-type id_client_mb() :: pid() | id().

%% TS server and client.
-type ts_server_mb() :: pid() | now().
-type ts_client_mb() :: pid() | ts().

%%% Interface-function associations.

%% ID server and client.
-new({id_server_mb, [id_server/0]}).
-use({id_server_mb, [id_server_loop/1]}).
-new({id_client_mb, [id_rpc/1]}).

%% TS server and client.
-new({ts_server_mb, [ts_server/0]}).
-use({ts_server_mb, [ts_server_loop/0]}).
-use({ts_client_mb, [ts_asy/1]}).
-use({ts_client_mb, [ts_get/0]}).

%% Main.
-new({ts_client_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

-spec id_server() -> no_return().
id_server() ->
  ?mb_assert_regex("Init.*Get"),
  receive
    {init, N} ->
      id_server_loop(N)
  end.

-spec id_server_loop(integer()) -> no_return().
id_server_loop(N) ->
  ?mb_assert_regex("*Get"),
  receive
    {get, Client} ->
      Client ! {id, N},
      id_server_loop(N + 1)
  end.

-spec ts_server() -> no_return().
ts_server() ->
  ts_server_loop(). %TODO: Fix once the 'use' bug is fixed.

-spec ts_server_loop() -> no_return().
ts_server_loop() ->
  ?mb_assert_regex("*Now"),
  receive
    {now, Client} ->
      Ts = system_time(),
      Client ! {ts, Ts},
      ts_server_loop()
  end.

-spec id_rpc(id_server_mb()) -> integer().
id_rpc(Server) ->
  Self = self(),
  Server ! {get, Self},
  ?mb_assert_regex("Id"),
  receive
    {id, Id} ->
      Id
  end.

-spec ts_asy(ts_server_mb()) -> any().
ts_asy(Server) ->
  Self = self(),
  Server ! {now, Self}.

-spec ts_get() -> integer().
ts_get() ->
  ?mb_assert_regex("Ts"),
  receive
    {ts, Ts} ->
      Ts
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ?mb_new(id_server_mb), % TODO: To remove once 'use' bug is fixed.
  IdServer = spawn(?MODULE, id_server, []),
  ?mb_new(ts_server_mb), % TODO: To remove once 'use' bug is fixed.
  TsServer = spawn(?MODULE, ts_server, []),

  % Bootstrap ID server.
  IdServer ! {init, 5},

  % Request servers.
  ts_asy(TsServer),
  Id = id_rpc(IdServer),
  Ts = ts_get(),
  format("Id: ~p~n", [Id]),
  format("Ts: ~p~n", [Ts]).


%% ./src/paterl src/examples/erlang/codebeam/id_ts_server_demo.erl -v all -I include