%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2023 17:31
%%%-------------------------------------------------------------------
-module(receive_scoping_test_1).
-author("duncan").

%% API
-export([main/0, server/0]).

server() ->
  receive
    {add, Client, A, B} ->
      Client ! A + B,
      server();
    {mul, Client, A, B} ->
      Client ! A * B,
      server()
  end.

main() ->

  ServerPid = spawn(?MODULE, server, []),

  ServerPid ! {add, self(), 1, 2},
  ServerPid ! {mul, self(), 1, 2},

  receive
    {ans, A} ->
      A
  end,

  receive
    {ans, B} ->
      B
  end.

