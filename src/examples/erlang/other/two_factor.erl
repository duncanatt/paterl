%%%-------------------------------------------------------------------
%%% @author laura
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% Port of the pat-lang two_factor example. A client logs into a server
%%% that either denies the request, grants it, or issues a two-factor
%%% challenge handled by a dedicated challenge response process.
%%% @end
%%%-------------------------------------------------------------------
-module(two_factor).

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([server/0, challenge_response_handler/1]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Server.
-type login_request() :: {login_request, string(), string(), client_mb()}.

%% Client.
-type auth_denied() :: {auth_denied}.
-type auth_granted() :: {auth_granted}.
-type challenge() :: {challenge, string(), challenge_response_handler_mb()}.

%% Challenge response handler.
-type challenge_response() :: {challenge_response, string(), client_mb()}.

%%% Interfaces.

%% Server.
-type server_mb() :: pid() | login_request().

%% Client.
-type client_mb() :: pid() | auth_denied() | auth_granted() | challenge().

%% Challenge response handler.
-type challenge_response_handler_mb() :: pid() | challenge_response().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Server.
-new({server_mb, [server/0]}).
-use({server_mb, [server_loop/0]}).

%% Client.
-new({client_mb, [client/1]}).

%% Challenge response handler.
-new({challenge_response_handler_mb, [challenge_response_handler/1]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Checks the login details of a client.
-spec check_details(string(), string()) -> boolean().
check_details(Username, Password) ->
  true.

%% @doc Checks the response to a challenge.
-spec response_valid(string()) -> boolean().
response_valid(Response) ->
  true.

%% @doc Generates a new challenge key.
-spec gen_challenge_key() -> string().
gen_challenge_key() ->
  "challenge".

%% @doc Computes the response to a challenge.
-spec challenge_response(string()) -> string().
challenge_response(Challenge) ->
  "challenge".

%% @doc Handler process validating the response to a challenge.
-spec challenge_response_handler(string()) -> no_return().
challenge_response_handler(Key) ->
  ?expects("Challenge_response"),
  receive
    {challenge_response, Response, ReplyTo} ->
      Valid = response_valid(Response),
      if Valid ->
        ReplyTo ! {auth_granted};
        true ->
          ReplyTo ! {auth_denied}
      end
  end.

%% @doc Server process handling the login requests.
-spec server() -> no_return().
server() ->
  server_loop().

%% @doc Server loop handling the remaining login requests.
-spec server_loop() -> no_return().
server_loop() ->
  ?expects("Login_request*"),
  receive
    {login_request, Username, Password, ReplyTo} ->
      Ok = check_details(Username, Password),
      if Ok ->
        Key = gen_challenge_key(),
        Handler = spawn(?MODULE, challenge_response_handler, [Key]),
        ReplyTo ! {challenge, Key, Handler},
        server_loop();
        true ->
          ReplyTo ! {auth_denied},
          server_loop()
      end
  after 0 ->
    format("Server exited.~n", [])
  end.

%% @doc Client logging into the server and answering its challenge.
-spec client(server_mb()) -> any().
client(Server) ->
  Self = self(),
  Server ! {login_request, "simon", "password", Self},
  ?expects(client_mb, "Auth_granted + Auth_denied + Challenge"),
  receive
    {auth_granted} ->
      format("Granted!~n", []);
    {auth_denied} ->
      format("Denied!~n", []);
    {challenge, Key, ReplyTo} ->
      format("Challenge!~n", []),
      Self0 = self(),
      Response = challenge_response(Key),
      ReplyTo ! {challenge_response, Response, Self0},
      ?expects("Auth_granted + Auth_denied"),
      receive
        {auth_granted} ->
          format("Granted!~n", []);
        {auth_denied} ->
          format("Denied!~n", [])
      end
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ServerMb = spawn(?MODULE, server, []),
  client(ServerMb).


%% ./src/paterl src/examples/erlang/other/two_factor.erl -v all -I include
