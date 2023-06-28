%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jun 2023 11:30
%%%-------------------------------------------------------------------
-module(id_echo).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([id_server/1, echo_server/0]).

%% interface IdServer { Get(IdClient!) }
%% interface IdClient { Id(Int) }

%% interface EchoServer { Echo(EchoClient!, String) }
%% interface EchoClient { Reply(String) }

%% Id messages.
-type get() :: {get, id_client()}.
-type id() :: {id, integer()}.

%% Echo messages.
-type echo() :: {echo, echo_client(), string()}.
-type reply() :: {reply, string()}.

%% Mailbox interfaces.
-type id_server() :: pid() | get().
-type id_client() :: pid() | id().

-type echo_server() :: pid() | echo().
-type echo_client() :: pid() | reply().

-id_server([id_server/1]).
-id_client([id_client/1]).

-echo_server([echo_server/0]).
-echo_client([echo_client/2]).

%% def id_server(self: IdServer?, next: Int): Unit {
%%   guard self: *Get {
%%     free -> ()
%%     receive Get(client) from self ->
%%       client ! Id(next);
%%       id_server(self, next + 1)
%%   }
%% }
-spec id_server(integer()) -> no_return().
id_server(Next) ->
  ?mb_state_free("get*"),
  receive
    {get, Client} ->
      Client ! {id, Next},
      id_server(Next + 1)
  end.

%% def echo_server(self: EchoServer?): Unit {
%%   guard self: *Echo {
%%     free -> ()
%%     receive Echo(client, msg) from self ->
%%       client ! Reply(msg);
%%       echo_server(self)
%%   }
%% }
-spec echo_server() -> no_return().
echo_server() ->
  ?mb_state_free("echo*"),
  receive
    {echo, Client, Msg} ->
      Client ! {reply, Msg},
      echo_server()
  end.

%% def id_client(serverMb: IdServer!): Int {
%%   let clientMb = new [IdClient] in
%%   serverMb ! Get(clientMb);
%%   guard clientMb: Id {
%%     receive Id(id) from clientMb ->
%%       free(clientMb);
%%       id
%%   }
%% }
-spec id_client(id_server()) -> integer().
id_client(Server) ->
  Server ! {get, self()},
  ?mb_state("id"),
  receive
    {id, Id} ->
      Id
  end.

%% def echo_client(serverMb: EchoServer!, msg: String): String {
%%   let clientMb = new [EchoClient] in
%%   serverMb ! Echo(clientMb, msg);
%%   guard clientMb: Reply {
%%     receive Reply(echoed) from clientMb ->
%%       free(clientMb);
%%       echoed
%%   }
%% }
-spec echo_client(echo_server(), string()) -> string().
echo_client(Server, Msg) ->
  Server ! {echo, self(), Msg},
  ?mb_state("reply", echo_client),
  receive
    {reply, Echoed} ->
      Echoed
  end.

%% def main(): Unit {
%%   let idServerMb = new [IdServer] in
%%   spawn {id_server(idServerMb, 0)};
%%
%%   let echoServerMb = new [EchoServer] in
%%   spawn {echo_server(echoServerMb)};
%%
%%   print(intToString(id_client(idServerMb)));
%%   print(echo_client(echoServerMb, "Hello"))
%% }

main() ->
  IdServer = spawn(?MODULE, id_server, [0]),
  EchoServer = spawn(?MODULE, echo_server, []),

  % Means that when we translate:
  % 1. We need to create a mailbox MB here in Pat.
  % 2. Pass the MB to id_client which returns it.
  % 3. Pass the returned MB to echo_client.
  % 4. MB must be a type union of all the messages, in this case, id() | reply()
  %    that come from two different MB interfaces.
  % 5. The regexes used by id_client and echo_client must also consider id() and
  %    reply. Therefore, the ones above are wrong: both should be "id.reply".
  %    This will grow accordingly the more different requests are issued from
  %    main. Perhaps this could break the local reasoning from the part of the
  %    developer, since in Erlang, the MB is global in a process.
  % 6. We need to keep track of when free(MB) is generated in the resulting Pat
  %    code.
  format("~p~n", [id_client(IdServer)]),
  format("~s~n", [echo_client(EchoServer, "Hello")]).
