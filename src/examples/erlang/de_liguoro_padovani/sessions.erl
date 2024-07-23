%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Alice asks Carol to compute the sum of two numbers exchanged through a
%%% session that is mediated via an arbiter.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(sessions).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([alice/1, carol/1]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Alice.
-type a_reply1() :: {a_reply1, arbiter_mb()}.
-type a_reply2() :: {a_reply2, arbiter_mb()}.
-type a_reply3() :: {a_reply3, integer()}.

%% Carol.
-type c_reply1() :: {c_reply1, integer(), arbiter_mb()}.
-type c_reply2() :: {c_reply2, integer(), arbiter_mb()}.
-type c_reply3() :: {c_reply3}.

%% Arbiter.
-type a_send1() :: {a_send1, integer(), alice_mb()}.
-type a_send2() :: {a_send2, integer(), alice_mb()}.
-type a_receive() :: {a_receive, alice_mb()}.
-type c_receive1() :: {c_receive1, carol_mb()}.
-type c_receive2() :: {c_receive2, carol_mb()}.
-type c_send() :: {c_send, integer(), carol_mb()}.

%%% Interfaces.

%% Alice.
-type alice_mb() :: pid() | a_reply1() | a_reply2() | a_reply3().

%% Carol.
-type carol_mb() :: pid() | c_reply1() | c_reply2() | c_reply3().

%% Arbiter.
-type arbiter_mb() :: pid() | a_send1() | a_send2() | a_receive() | c_receive1() | c_receive2() | c_send().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Alice.
-new({alice_mb, [alice/1]}).

%% Carol.
-new({carol_mb, [carol/1]}).

%% Arbiter.
-use({arbiter_mb, [arbiter/0]}).
-new({arbiter_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Alice process requesting addition.
-spec alice(arbiter_mb()) -> no_return().
alice(Arb) ->
  Self = self(),
  Arb ! {a_send1, 4, Self},
  ?mb_assert_regex("A_reply1"),
  receive
    {a_reply1, Arb} ->
      Self = self(), % Self is needed here since after receiving, the mailbox ref is updated.
      Arb ! {a_send2, 2, Self},
      ?mb_assert_regex("A_reply2"),
      receive
        {a_reply2, Arb} ->
          Self = self(), % Self is needed here since after receiving, the mailbox ref is updated.
          Arb ! {a_receive, Self}
      end
  end,
  ?mb_assert_regex("A_reply3"),
  receive
    {a_reply3, Res} ->
      format("~p~n", [Res])
  end.

%% @doc Carol process performing addition and returning result to arbiter.
-spec carol(arbiter_mb()) -> no_return().
carol(Arb) ->
  Self = self(),
  Arb ! {c_receive1, Self},
  ?mb_assert_regex("C_reply1"),
  receive
    {c_reply1, X, Arb} ->
      Self = self(), % Self is needed here since after receiving, the mailbox ref is updated.
      Arb ! {c_receive2, Self},
      ?mb_assert_regex("C_reply2"),
      receive
        {c_reply2, Y, Arb} ->
          Self = self(), % Self is needed here since after receiving, the mailbox ref is updated.
          Arb ! {c_send, X + Y, Self}
      end
  end,
  ?mb_assert_regex("C_reply3"),
  receive
    {c_reply3} ->
      ok
  end.

%% @doc Arbiter coordinating Alice and Carol.
-spec arbiter() -> no_return().
arbiter() ->
  Self = self(),
  ?mb_assert_regex("A_send1 . C_receive1"),
  receive
    {a_send1, Price1, AliceMB} ->
      format("Arbiter received first summand ~p from Alice.~n", [Price1]),
      ?mb_assert_regex("C_receive1"),
      receive
        {c_receive1, CarolMB} ->
          format("Arbiter received ack from Carol.~n", []),
          Self = self(), % Self is needed here since after receiving, the mailbox ref is updated.
          AliceMB ! {a_reply1, Self},
          CarolMB ! {c_reply1, Price1, Self},
          format("Arbiter sent ack to Alice and first summand ~p to Carol.~n", [Price1])
      end
  end,
  ?mb_assert_regex("A_send2 . C_receive2"),
  receive
    {a_send2, Price2, AliceMB} ->
      format("Arbiter received second summand ~p from Alice.~n", [Price2]),
      ?mb_assert_regex("C_receive2"),
      receive
        {c_receive2, CarolMB} ->
          format("Arbiter received ack from Carol.~n", []),
          Self = self(), % Self is needed here since after receiving, the mailbox ref is updated.
          AliceMB ! {a_reply2, Self},
          CarolMB ! {c_reply2, Price2, Self},
          format("Arbiter sent ack to Alice and second summand ~p to Carol.~n", [Price2])
      end
  end,
  ?mb_assert_regex("C_send . A_receive"),
  receive
    {c_send, Res, CarolMB} ->
      format("Arbiter received addition result ~p from Carol.~n", [Res]),
      ?mb_assert_regex("A_receive"),
      receive
        {a_receive, AliceMB} ->
          format("Arbiter received ack from Alice.~n", []),
          AliceMB ! {a_reply3, Res},
          CarolMB ! {c_reply3},
          format("Arbiter sent result ~p to Alice and ack to Carol.~n", [Res])
      end
  end.

%% @doc Launcher.
-spec main() -> no_return().
main() ->

  ?mb_new(alice_mb),
  ArbiterMB = self(),

  ?mb_new(alice_mb),
  spawn(?MODULE, alice, [ArbiterMB]),

  ?mb_new(carol_mb),
  spawn(?MODULE, carol, [ArbiterMB]),

  arbiter().


%% ./src/paterl src/examples/erlang/de_liguoro_padovani/sessions.erl -v all -I include