%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jul 2023 16:33
%%%-------------------------------------------------------------------
-module(id_echo_t).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/0, echo_server/0]).

%% Id messages.
-type init() :: {init, id_client(), integer()}.
-type ready() :: {ready}.
-type get() :: {get, id_client()}.
-type id() :: {id, integer()}.

%% Echo messages.
-type echo() :: {echo, echo_client(), string()}.
-type ok() :: {ok, string()}.

%% Mailbox interfaces.
-type id_server() :: pid() | init() | get().
-type id_client() :: pid() | ready() | id().

-type echo_server() :: pid() | echo().
-type echo_client() :: pid() | ok().

%% Mailbox interface associations. The difficult bit.

%% NAIVE.
-id_server([id_server/0, id_server_loop/1]). % Mailbox spans these two functions.
% Created externally.
-id_client([init/2, id_asy/1, id_get]). % Mailbox spans there three functions.
% Created externally, but would lead to one big common mailbox in Pat if naively
% translated.

-echo_server([echo_server/0]). % Mailbox created externally.
-echo_client([echo/2]). % Mailbox created externally.

% Current system not expressive enough. What about a notion of scoping/visibility
% for mailboxes?
% Scope of mailbox = local/shared (shared not quite precise, need another name) modifiers.
% Scope = private | visible?

-id_server({shared, [id_server/0, id_server_loop/1]}).
% Shared = mailbox passed externally in Pat. Can be freed internally or externally?
% This is another problem.

-id_client({shared, [id_get/0]}). % Mailbox passed externally in Pat.
-id_client({local, [init/2, id_asy/2]}). % Local = mailbox created in function
% and not visible from outside unless returned by function. In the case of
% id_asy it is freed from another place in Pat.

-echo_server({shared, [echo_server/0]}). % Mailbox created externally in Pat.
% Freed internally.
-echo_client({local, [echo/2]}). % Mailbox created internally in Pat. Freed
% internally.

% TODO: Investigate the relationship between mailbox creation (internal | external)
% TODO: and where they are freed (internal | external). Enumerate them.
% TODO: Investigate the type of calls (sync and async), and whether free can
% TODO: always be injected.

-spec id_server() -> no_return().
id_server() ->
  receive
    {init, Client, Start} ->
      Client ! {ready},
      id_server_loop(Start)
  end.

-spec id_server_loop(integer()) -> no_return().
id_server_loop(Next) ->
  receive
    {get, Client} ->
      Client ! {id, Next},
      id_server_loop(Next + 1)
  end.

-spec echo_server() -> no_return().
echo_server() ->
  receive
    {echo, Client, Msg} ->
      Client ! {ok, Msg},
      echo_server()
  end.

-spec init(id_server(), integer()) -> ok.
init(IdServer, Start) ->
  IdServer ! {init, self(), Start},
  receive
    {ready} ->
      ok
  end.

% @local @interface id_client. Are we saying that this is illegal?
-spec id_asy(id_server()) -> ok.
id_asy(IdServer) -> % Can also use any()/term() as the unit value, since Erlang does not have one except ok.
  IdServer ! {get, self()},
  ok.

-spec id_get() -> integer().
id_get() ->
  receive
    {id, Id} ->
      Id
  end.

-spec echo(echo_server(), string()) -> string().
echo(EchoServer, Msg) ->
  EchoServer ! {echo, self(), Msg},
  receive
    {ok, Msg} ->
      Msg
  end.

-spec main() -> any().
main() ->

  % Spawn ID server.
  IdServer = spawn(?MODULE, id_server, []),

  % Spawn Echo server.
  EchoServer = spawn(?MODULE, echo_server, []),

  % Initialize ID server with offset 10.
  init(IdServer, 10),

  % Issue asynchronous request to ID server.
  id_asy(IdServer),
  Id = id_get(),
  % Issue synchronous request to Echo server.
  Echo = echo(EchoServer, "hello"),

  % Fulfil ID request.


  % Print.
  format("~p~n", [Id]),
  format("~p~n", [Echo]).