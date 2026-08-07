%%%-------------------------------------------------------------------
%%% Plain Interfacer version of id_server.erl.
%%%
%%% Adapted from the Codebeam id_server_demo.
%%%
%%% Compared with Paterl, this omits -new/-use/?expects.
%%% Interfacer checks sends and receives against declared process interfaces:
%%% message tags and payload types are checked, but ordering is not.
%%%
%%% ?as(Interface) annotates a pid-valued expression, such as self() or
%%% spawn(...), when Interfacer cannot otherwise resolve its process interface.
%%% It is not a Mailboxer ?expects assertion and says nothing about ordering.
%%%
%%% Errors:
%%%   caught:     "unexpected message" (mistyped tag),
%%%               "type mismatch" (wrong payload type);
%%%   not caught: "extra Init request", "omitted Id reply" --
%%%               behavioural, out of scope.
%%%-------------------------------------------------------------------
-module(id_server_plain).

%%% Includes.
-include("paterl.hrl").

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
%%% Interfacer treats each *_mb type as a declared process interface:
%%% pid() says values of the type are process references, and the message
%%% alternatives declare the tags and payloads that process may receive.
%%% Eqwalizer, Dialyzer, and Typer provide ordinary Erlang type information;
%%% Interfacer performs the process-interface checks.

%% ID server and client.
-type id_server_mb() :: pid() | init() | get().
-type id_client_mb() :: pid() | id().

%%% ?as before a statically named spawn is redundant: the interface is inferred
%%% from the call graph of the spawned function. It is kept below for
%%% explicitness. Only ?as before self() is required.

%%% ----------------------------------------------------------------------------
%%% Server.
%%% ----------------------------------------------------------------------------

%% The id_server_mb declaration above is the interface this process is checked
%% against. Interfacer checks both receive clauses against that declared
%% interface.

%% @doc Server waits for an initial value before serving get requests.
-spec id_server() -> no_return().
id_server() ->
  %% Mailboxer used ?expects("Get*") here to demonstrate an omitted Init
  %% receive; Interfacer checks this receive against id_server_mb directly.
  receive
    {init, N} ->
      id_server_loop(N)
  end.

%% @doc Server loop replying to each client with the next id.
-spec id_server_loop(integer()) -> no_return().
id_server_loop(N) ->
  receive
    {get, Client} ->
      Client ! {id, N}, % Comment out for "omitted Id reply": NOT caught -> behavioural.
      id_server_loop(N + 1)
  end.

%%% ----------------------------------------------------------------------------
%%% Client.
%%% ----------------------------------------------------------------------------

%% The spec states that Server is a process reference with interface
%% id_server_mb(), so the sends below are checked without further annotation.
-spec id_client(id_server_mb()) -> integer().
id_client(Server) ->
  ?as(id_client_mb),
  Server ! {get, self()}, % Mistype tag for "unexpected message".
%%  Server ! {gte, self()}, % Uncomment for "unexpected message": caught, gte is not a tag of id_server_mb.
%%  Server ! {get, 16},     % Uncomment for "type mismatch": caught, 16 is not an id_client_mb process reference.
%%  Server ! {get, self()}, % Mailboxer "extra Id reply" toggle: behavioural.
%%  ?expects("Id"),         % Mailboxer fix for the extra reply; not used by Interfacer.
  receive
    {id, Id} ->
      Id
  end.

%%% ----------------------------------------------------------------------------
%%% Launcher.
%%% ----------------------------------------------------------------------------

%% @doc Launcher.
-spec main() -> any().
main() ->
  ?as(id_server_mb),
  Server = spawn(?MODULE, id_server, []),
  Server ! {init, 5},
%%  Server ! {init, 5},    % Uncomment for "extra Init request": NOT caught, well-typed against id_server_mb.
  Id = id_client(Server),
  io:format("Id: ~p~n", [Id]).

% erlc -I include -o ebin src/examples/interfacer/id_server_plain.erl
% erl -pa ebin -noshell -eval 'id_server_plain:main().' -s init stop
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize id_server_plain
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check this file with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/id_server_plain.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/id_server_plain.erl
