%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2023 14:00
%%%-------------------------------------------------------------------
-module(id_add).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/1, add_server/0]).

%%% Type definitions.

%% Id mailbox interface.
-type get() :: {get, id_client()}.
-type id() :: {id, integer()}.

-type id_server() :: get() | pid().
-type id_client() :: id() | pid().

%% Add mailbox interface.
-type add() :: {add, add_client(), integer(), integer()}.
-type ok() :: {ok, integer()}.

-type add_server() :: add() | pid().
-type add_client() :: ok() | pid().

% Scope of mailbox = local/shared modifiers.
%% Mailbox interface associations.
-id_server({shared, [id_server/1]}).
-id_client({local, [id_asy/1]}). % Mailbox created privately and returned.
-id_client({shared, [id_get/0]}). % Mailbox of async call passed here.

-add_server({shared, [add_server/0]}).
-add_client({local, [add/2]}).


-spec id_server(integer()) -> no_return().
id_server(Next) ->
  receive
    {get, Client} ->
      Client ! {id, Next},
      id_server(Next + 1)
  end.

-spec add_server() -> no_return().
add_server() ->
  receive
    {add, Client, A, B} ->
      Client ! {ok, A + B},
      add_server()
  end.

% This is the second type of local mailbox that creates it and returns it.
-spec id_asy(id_server()) -> ok.
id_asy(Server) ->
  Server ! {get, self()},
  ok.

% The companion function then frees the local mailbox.
-spec id_get() -> integer().
id_get() ->
  receive
    {id, Id} ->
      Id
  end.

% This is the first simpler type of local mailbox that creates it and frees it
% within the call.
-spec add(add_server(), integer(), integer()) -> integer().
add(Server, A, B) ->
  Server ! {add, self(), A, B},
  receive
    {ok, Ans} ->
      Ans
  end.

main() ->

  IdServer = spawn(?MODULE, id_server, [1]),
  AddServer = spawn(?MODULE, add_server, []),

  id_asy(IdServer),

  Ans = add(AddServer, 16, 10),

  Id = id_get(),

  % Send stp command.


  format("~p/~p~n", [Id, Ans]).
