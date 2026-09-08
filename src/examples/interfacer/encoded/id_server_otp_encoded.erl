%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../id_server_otp.erl for the proposed notation.
%%%
%%% gen_server version of id_server.erl, using gen_server:cast/2 for
%%% init and gen_server:call/2 for get. Message and interface types
%%% are exactly those of id_server.erl.
%%%
%%% Adapted from the Codebeam id_server_demo.
%%%
%%% The module has no receive expression. The role of the receive patterns is
%%% taken by gen_server callbacks, while id_server_in declares which messages
%%% may reach the server. handle_info/2 is only a defensive fallback: no plain
%%% send is part of the server interface.
%%%
%%% get()'s pid_of(id_client_in()) payload is carried on every call, but
%%% handle_call/3 does not need it: gen_server:call's own From already
%%% correlates the reply with the caller, so Client is bound and
%%% unused. It is kept because the type is kept.
%%%
%%% Errors:
%%%   caught:     "unexpected message" (bad tag in the call argument),
%%%               "type mismatch" (bad pid_of(id_client_in()) payload);
%%%   not caught: "omitted Id reply" -- behavioural, out of scope;
%%%   changed:    "extra Init request" crashes handle_cast/2
%%%               (function_clause) instead of sitting unmatched:
%%%               gen_server dispatches every cast.
%%%-------------------------------------------------------------------
-module(id_server_otp_encoded).
-behaviour(gen_server).

-include("interfacer.hrl").
%% Processes running this callback module have this interface. The callback
%% specs below are checked against it rather than defining it.
%%
%% This interface declaration takes no function name. Since the callback module is
%% conventionally one process, the declaration goes on the module. The
%% function-level form is used further down, for id_client/1.
-interface(id_server_in).
%%% API.
-export([start_link/0, id_client/1, main/0]).

%%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([init/0, get/0, id/0, id_server_in/0, id_client_in/0]).

-interface([{id_client, id_client_in}]).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% ID server.
-type init() :: {init, integer()}.
-type get() :: {get, pid_of(id_client_in())}.
-type id() :: {id, integer()}.

%%% Interfaces.

%% ID server and client.
-type id_server_in() :: #{call => get(), cast => init(), info => none()}.
-type id_client_in() :: id().

%%% ----------------------------------------------------------------------------
%%% Server.
%%% ----------------------------------------------------------------------------

%%% The module-level -interface above is what the callbacks are checked against;
%%% they do not define it. Having one component for call, cast and plain send, it
%%% says on its own which are empty, so a callback spec no longer has to.
%%%
%%% That resolves an awkwardness. As an interface component none() is right, it
%%% is the empty type and the identity for union; as a clause argument type it is
%%% wrong, because no clause head can match a value of type none() and ordinary
%%% analysers report the clause unreachable. The two claims belong in different
%%% places. The declaration says info => none(); the clause is typed term(),
%%% which is a statement about the clause, not about the interface.

%% @doc Starts the Id Server process.
-spec start_link() -> {ok, pid_of(id_server_in())} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% @doc Starts without an id value; init() is supplied by the first cast.
-spec init([]) -> {ok, undefined}.
init([]) ->
  {ok, undefined}.

%% @doc Handles the one-time Init message.
-spec handle_cast(init(), undefined) -> {noreply, integer()}.
handle_cast({init, N}, undefined) ->
  {noreply, N}.
%%handle_cast({reset}, _N) ->     % Uncomment for "callback outside the declared
%%  {noreply, 0};                 % interface": caught. reset is not in the cast
                                  % component of id_server_in, and the module
                                  % declaration is what this clause is checked
                                  % against, so adding a clause cannot widen it.

%% @doc Handles Get requests by replying with the next id.
-spec handle_call(get(), {pid(), term()}, integer()) -> {reply, id(), integer()}.
handle_call({get, _Client}, _From, N) ->
  {reply, {id, N}, N + 1}.
%%handle_call({get, _Client}, _From, N) -> {noreply, N}.  % Uncomment for "omitted Id reply": NOT caught (behavioural).

%% Defensive fallback. The declaration above already says info => none(); this
%% clause is typed term() because that is a claim about the clause, and none()
%% would make it unmatchable.
-spec handle_info(term(), undefined | integer()) ->
        {stop, {unexpected_info, term()}, undefined | integer()}.
handle_info(Msg, State) -> % defensive: exempt from the unmatchable-clause check.
  {stop, {unexpected_info, Msg}, State}.

%%% ----------------------------------------------------------------------------
%%% Client.
%%% ----------------------------------------------------------------------------

%% The spec types Server as pid_of(id_server_in()), so Interfacer checks the
%% call below against that interface. id_client/1 needs a declaration of its own
%% because it calls self(): the client is a plain process, not this callback
%% module, so the module-level declaration above does not cover it.
-spec id_client(pid_of(id_server_in())) -> integer().
id_client(Server) ->
  case gen_server:call(Server, {get, self()}) of % Mistype tag for "unexpected message".
%%  case gen_server:call(Server, {gte, self()}) of  % Uncomment for "unexpected message": gte is not a tag of id_server_in.
%%  case gen_server:call(Server, {get, 16}) of       % Uncomment for "type mismatch": 16 is not a pid_of(id_client_in()).
    {id, Id} -> Id
  end.

%%% ----------------------------------------------------------------------------
%%% Launcher.
%%% ----------------------------------------------------------------------------

%% @doc Launcher.
-spec main() -> any().
main() ->
  {ok, Server} = start_link(),
  gen_server:cast(Server, {init, 5}),
%%  gen_server:cast(Server, {init, 5}),  % Uncomment for "extra Init request":
%%                                         now a crash (function_clause in handle_cast/2),
%%                                         not silently unmatched. See header note.
  Id = id_client(Server),
  io:format("Id: ~p~n", [Id]).

%% Encoded form of ../id_server_otp.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/id_server_otp_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'id_server_otp_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize id_server_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/id_server_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/id_server_otp_encoded.erl
