%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 7月 2024 11:31
%%%-------------------------------------------------------------------
-module(sessions).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([alice/1, carol/1]).

%% Mailbox interface-function associations
-new({alice_mb, [alice/1]}).
-new({carol_mb, [carol/1]}).
-new({arbiter_mb, [arbiter/0]}).
-new({main_mb, [main/0]}).

%% Alice's message types
-type a_reply1() :: {a_reply1, arbiter_mb()}.
-type a_reply2() :: {a_reply2, arbiter_mb()}.
-type a_reply3() :: {a_reply3, integer()}.

%% Carol's message types
-type c_reply1() :: {c_reply1, integer(), arbiter_mb()}.
-type c_reply2() :: {c_reply2, integer(), arbiter_mb()}.
-type c_reply3() :: {c_reply3}.

%% Arbiter's message types
-type a_send1() :: {a_send1, integer(), alice_mb()}.
-type a_send2() :: {a_send2, integer(), alice_mb()}.
-type a_receive() :: {a_receive, alice_mb()}.
-type c_receive1() :: {c_receive1, carol_mb()}.
-type c_receive2() :: {c_receive2, carol_mb()}.
-type c_send() :: {c_send, integer(), carol_mb()}.

%% # Mailbox message interfaces.
%% interface Alice {
%%    AReply1(Arbiter!),
%%    AReply2(Arbiter!),
%%    AReply3(Int)
%% }
%%
%% interface Carol {
%%    CReply1(Int, Arbiter!),
%%    CReply2(Int, Arbiter!),
%%    CReply3()
%% }
%%
%% interface Arbiter {
%%    ASend1(Int, Alice!),
%%    ASend2(Int, Alice!),
%%    AReceive(Alice!),
%%    CReceive1(Carol!),
%%    CReceive2(Carol!),
%%    CSend(Int, Carol!)
%% }
-type alice_mb() :: pid() | a_reply1() | a_reply2() | a_reply3().
-type carol_mb() :: pid() | c_reply1() | c_reply2() | c_reply3().
-type arbiter_mb() :: pid() | a_send1() | a_send2() | a_receive() | c_receive1() | c_receive2() | c_send().
-type main_mb() :: pid().

%% def alice(self: Alice?, arb: Arbiter!): Unit {
%%   arb ! ASend1(4, self);
%%   let self =
%%     guard self : AReply1 {
%%       receive AReply1(arb) from self ->
%%         arb ! ASend2(2, self);
%%         guard self : AReply2 {
%%           receive AReply2(arb) from self ->
%%             arb ! AReceive(self);
%%             self
%%         }
%%     }
%%   in
%%   guard self : AReply3 {
%%     receive AReply3(res) from self ->
%%       print(intToString(res));
%%       free(self)
%%   }
%% }
-spec alice(arbiter_mb()) -> no_return().
alice(Arb) ->
  Self = self(),
  Arb ! {a_send1, 4, Self},
  ?mb_assert_regex("AReply1"),
  receive
    {a_reply1, Arb} ->
      Arb ! {a_send2, 2, Self},
      ?mb_assert_regex("AReply2"),
      receive
        {a_reply2, Arb} ->
          Arb ! {a_receive, Self}
      end
  end,
  ?mb_assert_regex("AReply3"),
  receive
    {a_reply3, Res} ->
      io:format("~p~n", [Res])
  end.

%% def carol(self: Carol?, arb: Arbiter!): Unit {
%%   arb ! CReceive1(self);
%%   let self =
%%     guard self : CReply1 {
%%       receive CReply1(x, arb) from self ->
%%         arb ! CReceive2(self);
%%         guard self : CReply2 {
%%           receive CReply2(y, arb) from self ->
%%             arb ! CSend(x + y, self);
%%             self
%%         }
%%     }
%%   in
%%   guard self : CReply3 {
%%     receive CReply3() from self -> free(self)
%%   }
%% }
-spec carol(arbiter_mb()) -> no_return().
carol(Arb) ->
  Self = self(),
  Arb ! {c_receive1, Self},
  ?mb_assert_regex("CReply1"),
  receive
    {c_reply1, X, Arb} ->
      Arb ! {c_receive2, Self},
      ?mb_assert_regex("CReply2"),
      receive
        {c_reply2, Y, Arb} ->
          Arb ! {c_send, X + Y, Self}
      end
  end,
  ?mb_assert_regex("CReply3"),
  receive
    {c_reply3} ->
      ok
  end.

%% def arbiter(self: Arbiter?): Unit {
%%   let self =
%%     guard self : ASend1 . CReceive1 {
%%       receive ASend1(x, aliceMB) from self ->
%%         guard self : CReceive1 {
%%           receive CReceive1(carolMB) from self ->
%%             aliceMB ! AReply1(self);
%%             carolMB ! CReply1(x, self);
%%             self
%%         }
%%     }
%%   in
%%   let self =
%%     guard self : ASend2 . CReceive2 {
%%       receive ASend2(y, aliceMB) from self ->
%%         guard self : CReceive2 {
%%           receive CReceive2(carolMB) from self ->
%%             aliceMB ! AReply2(self);
%%             carolMB ! CReply2(y, self);
%%             self
%%         }
%%     }
%%   in
%%   guard self : CSend . AReceive {
%%     receive CSend(res, carolMB) from self ->
%%       guard self : AReceive {
%%         receive AReceive(aliceMB) from self ->
%%           aliceMB ! AReply3(res);
%%           carolMB ! CReply3();
%%           free(self)
%%       }
%%   }
%% }
-spec arbiter() -> no_return().
arbiter() ->
  Self = self(),
  ?mb_assert_regex("ASend1 . CReceive1"),
  receive
    {a_send1, X, AliceMB} ->
      ?mb_assert_regex("CReceive1"),
      receive
        {c_receive1, CarolMB} ->
          AliceMB ! {a_reply1, Self},
          CarolMB ! {c_reply1, X, Self}
      end
  end,
  ?mb_assert_regex("ASend2 . CReceive2"),
  receive
    {a_send2, Y, AliceMB} ->
      ?mb_assert_regex("CReceive2"),
      receive
        {c_receive2, CarolMB} ->
          AliceMB ! {a_reply2, Self},
          CarolMB ! {c_reply2, Y, Self}
      end
  end,
  ?mb_assert_regex("CSend . AReceive"),
  receive
    {c_send, Res, CarolMB} ->
      ?mb_assert_regex("AReceive"),
      receive
        {a_receive, AliceMB} ->
          AliceMB ! {a_reply3, Res},
          CarolMB ! {c_reply3}
      end
  end.

%% def main(): Unit {
%%   let aliceMB = new[Alice] in
%%   let carolMB = new[Carol] in
%%   let arbiterMB = new[Arbiter] in
%%   spawn { alice(aliceMB, arbiterMB) };
%%   spawn { carol(carolMB, arbiterMB) };
%%   arbiter(arbiterMB)
%% }
-spec main() -> no_return().
main() ->

  ?mb_new(alice_mb),
  ArbiterMB = self(),

  ?mb_new(alice_mb),
  spawn(?MODULE, alice, [ArbiterMB]),

  ?mb_new(carol_mb),
  spawn(?MODULE, carol, [ArbiterMB]),

  arbiter().
