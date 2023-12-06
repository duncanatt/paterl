%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2023 15:05
%%%-------------------------------------------------------------------
-module(id_calc).
-author("duncan").

%% API
-export([]).


%%-id_server([id_server/1]).
%%-id_client(local, [id/1]).
%%
%%-add_server([calc_server/0, handle_add/3]).
%%-add_client(local, []).



id_server(Next) ->
  receive
    {get, Client} ->
      Client ! {id, Next},
      id_server(Next + 1)
  end.

calc_server() ->
  receive
    {add, ClientPid, A, B} ->
%%      Client ! {ok, A + B},
%%      calc_server()
      handle_add(ClientPid, A, B)
  end.

% Split mailbox.
handle_add(ClientPid, A, B) ->
  ClientPid ! {ok, A + B},
  calc_server().

% The subtract is introduced so that we have two operations contacting the server
% add, sub: these should show that the mailbox is not a continuation of these
% but.

% Local.
id(ServerPid) ->
  ServerPid ! {get, self()},
  receive
    {id, Id} ->
      Id
  end.

% Local. The idea is that.
add(ServerPid, A, B) ->
  ServerPid ! {add, self(), A, B},
  "ok",
  receive
    {ok, Ans} ->
      Ans
  end.

% Local mailbox here.
mul(ServerPid, A, B) ->
  if B =:= 0 ->
    0;
    B =:= 1 ->
      A;
    true ->
      mul_loop(ServerPid, A, B, 0) % Self must be injected here.
  end.

% Local in mul, but shared in mul_loop
mul_loop(ServerPid, A, B, Acc) ->
  ServerPid ! {add, self(), A, A},
  "ok*",
  receive
    {ok, Ans} ->
      mul_loop(ServerPid, A, B - 1, Acc + Ans)
  end.




