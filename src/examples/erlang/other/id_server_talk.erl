%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2023 17:41
%%%-------------------------------------------------------------------
-module(id_server_talk).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/0]).

%% Id messages.
-type init() :: {init, integer()}.
-type get() :: {get, id_client()}.
-type id() :: {id, id()}.

%% Server and client interfaces.
-type id_server() :: pid() | init() | get().
-type id_client() :: pid() | id().

%% Interface associations.
-id_server({interface, [id_server/0]}).
-id_server({use, [id_server_loop/1]}).

-id_client({interface, [main/0]}).

-spec id_server() -> no_return().
id_server() ->
  ?mb_assert("init.get*", id_server),
  receive
    {init, N} -> id_server_loop(N)
  end.

-spec id_server_loop(integer()) -> no_return().
id_server_loop(N) ->
  ?mb_assert("get*", id_server),
  receive
    {get, Client} ->
      Client ! {id, N},
      id_server_loop(N + 1)
  end.

-spec main() -> ok.
main() ->
  Server = spawn(?MODULE, id_server, []),

  Server ! {init, 5},
  Server ! {get, self()},
  ?mb_assert("id", id_client),
  receive
    {id, Id} -> format("~p~n", [Id])
  end.
