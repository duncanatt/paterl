%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 7月 2024 12:57
%%%-------------------------------------------------------------------
-module(account_future).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([account/1, await/2, debit/2]).

%% Mailbox interface-function associations
-new({account_mb, [account/1]}).
-use({account_mb, [account_loop/1, flush/2]}).

%%-new({main_mb, [main/0]}).

-new({future_mb, [await/2, main/0]}).
%%-use({future_mb, [debit/2, resume/0]}).
-use({future_mb, [debit/2]}).

%% Account's message types
-type debit() :: {debit, integer(), future_mb()}.
-type credit() :: {credit, integer(), account_mb(), future_mb()}.
-type stop() :: {stop}.

%% Future's message types
-type reply() :: {reply}.

%% # Mailbox message interfaces.
%% interface AccountMb {
%%    Debit(Int, FutureMb!),
%%    Credit(Int, AccountMb!, FutureMb!),
%%    Stop()
%%}
%%
%% interface FutureMb {
%%    Reply()
%% }
-type future_mb() :: pid() | reply().
-type account_mb() :: pid() | debit() | credit() | stop().
-type main_mb() :: pid().

%% def await(amount: Int, recipient: AccountMb!): FutureMb? {
%%   let future = new [FutureMb] in
%%   spawn { recipient ! Debit(amount, future) };
%%   future
%% }
-spec await(integer(), account_mb()) -> future_mb().
await(Amount, Recipient) ->
  Self = self(),
%%  ?mb_new(future_mb),
%%  Future.
%%  Future = spawn(?MODULE, debit, [Amount, Recipient]),
%%  Self = self(),
  Recipient ! {debit, Amount, Self}.
%%  Self.
%%  ?mb_assert_regex("Reply + 1"),
%%  receive
%%    {reply} ->
%%      ok
%%  end.



-spec debit(integer(), account_mb()) -> future_mb().
debit(Amount, Recipient) ->
  Self = self(),
  Recipient ! {debit, Amount, Self},
  Self.


%% def resume(future: FutureMb?): Unit {
%%   guard future: Reply + 1 {
%%     free ->
%%         print("WARN: Did not receive Reply ack from account!")
%%     receive Reply() from future ->
%%         free(future)
%%   }
%% }
-spec resume(future_mb()) -> no_return().
resume(Future) ->
%%  TODO Not sure about free
  ?expects("Reply + 1"),
  receive
    {reply} ->
      ok
  end.

%% def flush(account: AccountMb?, stale: Int): Int {
%%   guard account: (*Debit) . (*Credit) {
%%     free -> stale
%%     receive Debit(amount, sender) from account ->
%%         flush(account, stale + 1)
%%     receive Credit(amount, recipient, sender) from account ->
%%         flush(account, stale + 1)
%%   }
%% }
-spec flush(account_mb(), integer()) -> integer().
flush(Account, Stale) ->
  %%  TODO Not sure about free
  ?expects("*Debit . *Credit"),
  receive
    {debit, Amount, Sender} ->
      flush(Account, Stale + 1);
    {credit, Amount, Recipient, Sender} ->
      flush(Account, Stale + 1)
  end.

%% def account(self: AccountMb?, balance: Int): Unit {
%%   guard self: ((*Debit) . (*Credit)) . Stop {
%%     receive Debit(amount, sender) from self ->
%%         sender ! Reply();
%%         account(self, balance + amount)
%%     receive Credit(amount, recipient, sender) from self ->
%%         let future = await(amount, recipient) in
%%         resume(future);
%%         sender ! Reply();
%%         account(self, balance - amount)
%%     receive Stop() from self ->
%%         print("INFO: Terminating account.");
%%         let stale = flush(self, 0) in
%%             if (stale > 0) {
%%                 print("WARN: Flushed ");
%%                 print(intToString(stale));
%%                 print(" message(s)!")
%%             }
%%         else {
%%             ()
%%         }
%%   }
%% }
-spec account(integer()) -> no_return().
account(Balance) ->
  account_loop(Balance).

-spec account_loop(integer()) -> no_return().
account_loop(Balance) ->
  ?expects("((*Debit) . (*Credit)) . Stop"),
  receive
    {debit, Amount, Sender} ->
      Sender ! {reply},
      account_loop(Balance + Amount);
    {credit, Amount, Recipient, Sender} ->
%%      Future = await(Amount, Recipient),
      ?mb_new(future_mb),
      Future = spawn(?MODULE, await, [Amount, Recipient]),
      resume(Future),
      Sender ! {reply},
      account_loop(Balance - Amount);
    {stop} ->
      Self = self(),
      Stale = flush(Self, 0)
%%      if
%%        Stale > 0 ->
%%          format("WARN: Flushed ~p message(s)!~n", [Stale]);
%%        true ->
%%          ok
%%      end
  end.

%% def main(): Unit {
%%   let alice = new [AccountMb] in
%%   spawn { account(alice, 5) };
%%   let bob = new [AccountMb] in
%%   spawn { account(bob, 20) };
%%   let self = new [FutureMb] in
%%   bob ! Credit(20, alice, self);
%%   resume(self);
%%   alice ! Stop();
%%   bob ! Stop()
%% }
-spec main() -> any().
main() ->
  ?mb_new(account_mb),
  Alice = spawn(?MODULE, account, [5]),
  ?mb_new(account_mb),
  Bob = spawn(?MODULE, account, [20]),

  ?mb_new(main_mb),
  Self = self(),
  Bob ! {credit, 20, Alice, Self},

  Self = self(),
  resume(Self),

  Alice ! {stop},
  Bob ! {stop}.