%%%-------------------------------------------------------------------
%%% gen_server version of id_server.erl, using gen_server:cast/2 for
%%% init and gen_server:call/2 for get. Message and interface types
%%% are exactly those of id_server.erl.
%%%
%%% Adapted from the Codebeam id_server_demo.
%%%
%%% The module has no receive expression. The role of the receive patterns is
%%% taken by gen_server callbacks, while id_server_mb declares which messages
%%% may reach the server. handle_info/2 is only a defensive fallback: no plain
%%% send is part of the server interface.
%%%
%%% get()'s id_client_mb() payload is carried on every call, but
%%% handle_call/3 does not need it: gen_server:call's own From already
%%% correlates the reply with the caller, so Client is bound and
%%% unused. It is kept because the type is kept.
%%%
%%% Errors:
%%%   caught:     "unexpected message" (bad tag in the call argument),
%%%               "type mismatch" (bad id_client_mb() payload);
%%%   not caught: "omitted Id reply" -- behavioural, out of scope;
%%%   changed:    "extra Init request" crashes handle_cast/2
%%%               (function_clause) instead of sitting unmatched:
%%%               gen_server dispatches every cast.
%%%-------------------------------------------------------------------
-module(id_server_otp).
-behaviour(gen_server).

%%% API.
-export([start_link/0, id_client/1, main/0]).

%%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([init/0, get/0, id/0, id_server_mb/0, id_client_mb/0]).

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
%%% Eqwalizer, Dialyzer, and TypEr provide ordinary Erlang type information;
%%% Interfacer performs the process-interface checks.

%% ID server and client.
-type id_server_mb() :: pid() | init() | get().
-type id_client_mb() :: pid() | id().

%%% ----------------------------------------------------------------------------
%%% Server.
%%% ----------------------------------------------------------------------------

%%% Callback argument types state which messages reach each callback, and are
%%% checked against the interface declarations above. A callback that exists
%%% only as a defensive fallback is typed term(): it carries no interface
%%% information, and is typed so that ordinary Erlang analysers can check the
%%% clause. Interfacer takes the declared *_if types, not the fallbacks, as
%%% the process interface.

%% @doc Starts the Id Server process.
-spec start_link() -> {ok, id_server_mb()} | ignore | {error, term()}.
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

%% @doc Handles Get requests by replying with the next id.
-spec handle_call(get(), {pid(), term()}, integer()) -> {reply, id(), integer()}.
handle_call({get, _Client}, _From, N) ->
  {reply, {id, N}, N + 1}.
%%handle_call({get, _Client}, _From, N) -> {noreply, N}.  % Uncomment for "omitted Id reply": NOT caught (behavioural).

%% Defensive fallback: no plain send is part of id_server_mb. Typed term()
%% rather than none() so ordinary Erlang analysers can check the clause; it
%% contributes nothing to the interface.
-spec handle_info(term(), undefined | integer()) ->
        {stop, {unexpected_info, term()}, undefined | integer()}.
handle_info(Msg, State) -> % defensive: exempt from the unmatchable-clause check.
  {stop, {unexpected_info, Msg}, State}.

%%% ----------------------------------------------------------------------------
%%% Client.
%%% ----------------------------------------------------------------------------

%% The spec states that Server is a process reference with interface
%% id_server_mb(), so Interfacer can check the call below against that declared
%% interface.
-spec id_client(id_server_mb()) -> integer().
id_client(Server) ->
  case gen_server:call(Server, {get, self()}) of % Mistype tag for "unexpected message".
%%  case gen_server:call(Server, {gte, self()}) of  % Uncomment for "unexpected message": gte is not a tag of id_server_mb.
%%  case gen_server:call(Server, {get, 16}) of       % Uncomment for "type mismatch": 16 is not an id_client_mb process reference.
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

% erlc -o ebin src/examples/interfacer/id_server_otp.erl
% erl -pa ebin -noshell -eval 'id_server_otp:main().' -s init stop
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize id_server_otp
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check this file with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/id_server_otp.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/id_server_otp.erl
