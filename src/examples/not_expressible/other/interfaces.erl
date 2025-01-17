%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 7月 2024 10:27
%%%-------------------------------------------------------------------
-module(interfaces).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([go/0]).

%% Mailbox interface-function associations
-new({receiver_mb, [go/1]}).
-new({reply1_mb, [main/0]}).
-new({reply2_mb, [main/0]}).
-new({main_mb, [main/0]}).

%% Reply1's message types
-type go() :: {go}.

%% Receiver's message types
-type ready1() :: {ready1, reply1_mb()}.
-type ready2() :: {ready2, reply2_mb()}.

%% # Mailbox message interfaces.
%% interface Reply1 { Go() }
%% interface Reply2 { Go() }
%% interface Receiver { Ready1(Reply1!), Ready2(Reply2!) }
-type reply1_mb() :: pid() | go().
-type reply2_mb() :: pid() | go().
-type receiver_mb() :: pid() | ready1() | ready2().
-type main_mb() :: pid().

%% def go(self: Receiver?): Unit {
%%   guard self : Ready1 . Ready2 {
%%     receive Ready1(reply1) from mb ->
%%       guard mb : Ready2 {
%%         receive Ready2(reply2) from mb ->
%%           reply1 ! Go();
%%           reply2 ! Go();
%%           free(mb)
%%       }
%%   }
%% }
-spec go() -> no_return().
go() ->
  ?expects("Ready1 . Ready2"),
  receive
    {ready1, Reply1} ->
      ?expects("Ready2"),
      receive
        {ready2, Reply2} ->
          Reply1 ! {go},
          Reply2 ! {go}
      end
  end.

%% def main(): Unit {
%%   let mb = new[Receiver] in
%%   spawn { go(mb) };
%%   let client1 = new[Reply1] in
%%   let client2 = new[Reply2] in
%%   mb ! Ready1(client1);
%%   mb ! Ready2(client2);
%%   guard client1 : Go {
%%     receive Go() from client1 -> free(client1)
%%   };
%%   guard client2 : Go {
%%     receive Go() from client2 -> free(client2)
%%   }
%% }
-spec main() -> no_return().
main() ->
  Self = self(),
  spawn(?MODULE, go, []),

  Self ! {ready1, Self},
  Self ! {ready2, Self},

  ?expects("Go"),
  receive
    {go} ->
      ok
  end,

  ?expects("Go"),
  receive
    {go} ->
      ok
  end.