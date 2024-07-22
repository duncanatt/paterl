%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2023 17:41
%%%-------------------------------------------------------------------
-module(id_server_demo).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% ID server.
-type init() :: {init, integer()}.
-type get() :: {get, id_client_mb()}.
-type id() :: {id, integer()}.

%%% Interfaces.

%% ID server and client.
-type id_server_mb() :: pid() | init() | get().
-type id_client_mb() :: pid() | id().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% ID server and client.
-new({id_server_mb, [id_server/0]}).
-use({id_server_mb, [id_server_loop/1]}).
-new({id_client_mb, [id_client/1]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

-spec id_server() -> no_return().
id_server() ->
  ?mb_assert_regex("Init.*Get"),
%%  ?mb_assert_regex("*Get"), % Uncomment for "omitted Init receive".
  receive
    {init, N} ->
      id_server_loop(N)
  end.

-spec id_server_loop(integer()) -> no_return().
id_server_loop(N) ->
  ?mb_assert_regex("*Get"),
  receive
    {get, Client} ->
      Client ! {id, N}, % Comment for "omitted Id reply".
      id_server_loop(N + 1)
  end.

-spec id_client(id_server_mb()) -> integer().
id_client(Server) ->
  Self = self(),
  Server ! {get, Self}, % Mistype message tag for "unexpected message".
%%  Server ! {get, 16}, % Uncomment for "type mismatch".
%%  Server ! {get, Self}, % Uncomment for "extra Id reply".
  ?mb_assert_regex("Id"), % Fix "extra Id reply" by adding receive.
  receive
    {id, Id} ->
      Id
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ?mb_new(id_server_mb),
  Server = spawn(?MODULE, id_server, []),
  Server ! {init, 5},
%%  Server ! {init, 5}, % Uncomment for "extra Init request".
  Id = id_client(Server),
  format("Id: ~p~n", [Id]).


%% ./src/paterl src/examples/erlang/codebeam/id_server_demo.erl -v all -I include