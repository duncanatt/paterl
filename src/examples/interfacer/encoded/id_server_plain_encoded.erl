%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../id_server_plain.erl for the proposed notation.
%%%
%%% The hoisting is not cosmetic. Erlang requires user-defined attributes to
%%% precede every function definition, and -spec is a special case exempt from
%%% that rule, so an encoded declaration cannot stand beside its function. Were
%%% -interface added to the language it should be given -spec's exemption, or
%%% every declaration ends up separated from its subject.
%%%
%%% Plain Interfacer version of id_server.erl.
%%%
%%% Adapted from the Codebeam id_server_demo.
%%%
%%% Interfacer checks sends and receives against declared process interfaces:
%%% message tags and payload types are checked, but ordering is not.
%%%
%%% Errors:
%%%   caught:     "unexpected message" (mistyped tag),
%%%               "type mismatch" (wrong payload type);
%%%   not caught: "extra Init request", "omitted Id reply" --
%%%               behavioural, out of scope.
%%%-------------------------------------------------------------------
-module(id_server_plain_encoded).

-include("interfacer.hrl").
%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/0]).

-interface([{id_server, id_server_in},
            {id_client, id_client_in}]).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% ID server.
-type init() :: {init, integer()}.
-type get() :: {get, pid_of(id_client_in())}.
-type id() :: {id, integer()}.

%%% Interfaces.
%%% An interface is a union of the message types a process accepts.
%%% pid_of(I) is the type of a reference to a process with interface I. It is
%%% contravariant in I: a process accepting more messages can be used where one
%%% accepting fewer is expected.
%%% An interface is attached to whatever determines the process's behaviour.
%%% -interface f :: I() states that processes running f have interface I,
%%% and determines the type of self() within f as pid_of(I()); a function
%%% without its own declaration inherits the interface of its caller. -interface I() at
%%% module level states the same for processes running a callback module, which is
%%% conventionally one process.
%%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%%% Interfacer performs the process-interface checks.

%% ID server and client.
-type id_server_in() :: init() | get().
-type id_client_in() :: id().

%%% ----------------------------------------------------------------------------
%%% Server.
%%% ----------------------------------------------------------------------------

%% The id_server_in declaration above is the interface this process is checked
%% against. Interfacer checks both receive clauses against that declared
%% interface.
%%
%% On the form of the declaration. It names its function for the same reason
%% -spec does: Erlang attributes are not bound to the form that follows them, so
%% the subject has to be written out.
%% The separator is :: rather than :, as in -spec and -type, because m:t() is
%% already remote type reference syntax in Erlang; writing id_server:id_server_in()
%% would read as "the type id_server_in() exported by module id_server", which is
%% the wrong parse and a form that appears elsewhere in these examples.

%% @doc Server waits for an initial value before serving get requests.
-spec id_server() -> no_return().
id_server() ->
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

%% The spec types Server as pid_of(id_server_in()), so the sends below are
%% checked without further declaration.
-spec id_client(pid_of(id_server_in())) -> integer().
id_client(Server) ->
  Server ! {get, self()}, % Mistype tag for "unexpected message".
%%  Server ! {gte, self()}, % Uncomment for "unexpected message": caught, gte is not a tag of id_server_in.
%%  Server ! {get, 16},     % Uncomment for "type mismatch": caught, 16 is not a pid_of(id_client_in()).
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
  Server = spawn(?MODULE, id_server, []),
  Server ! {init, 5},
%%  Server ! {init, 5},    % Uncomment for "extra Init request": NOT caught, well-typed against id_server_in.
  Id = id_client(Server),
  io:format("Id: ~p~n", [Id]).

%% Encoded form of ../id_server_plain.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/id_server_plain_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'id_server_plain_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize id_server_plain_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/id_server_plain_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/id_server_plain_encoded.erl
