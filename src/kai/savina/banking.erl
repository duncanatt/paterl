%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 6月 2024 21:24
%%%-------------------------------------------------------------------
-module(banking).
-author("walker").


-include("paterl.hrl").

-import(io, [format/2]).
-import(rand, [uniform/1]).

%% API
-export([main/0]).

-export([account/2, teller/1, generate_work/5]).

%% Mailbox interface-function associations.
-new({teller_mb, [teller/1]}).
-use({teller_mb, [teller_loop/3]}).
-new({account_mb, [account/2]}).
-use({account_mb, [account_exit/0]}).
-new({main_mb, [main/0]}).

%% TellerMb's massage types
-type start() :: {start}.
-type reply() :: {reply}.

%% AccountMb's massage types
-type debit() :: {debit, account_mb(), integer()}.
-type credit() :: {credit, teller_mb(), integer(), account_mb()}.
-type done() :: {done}.
-type stop() :: {stop}.

%% Mailbox interfaces.
%%  interface TellerMb {
%%    Start(),
%%    Reply()
%%  }
%%  interface AccountMb {
%%    Debit(AccountMb!, Int),
%%    Credit(TellerMb!, Int, AccountMb!),
%%    Done(),
%%    Stop()
%%  }
-type teller_mb() :: pid() | start() | reply().
-type account_mb() :: pid() | debit() | credit() | done() | stop().
-type main_mb() :: pid().

%% def teller(self: TellerMb?, numAccounts: Int): Unit {
%%   let accountMb1 = new [AccountMb] in
%%   spawn { account(accountMb1, 1, 200) };
%%   let accountMb2 = new [AccountMb] in
%%   spawn { account(accountMb2, 2, 150) };
%%   let accountMb3 = new [AccountMb] in
%%   spawn { account(accountMb3, 3, 50) };
%%   guard self: Start {
%%     receive Start() from self ->
%%       spawn { generate_work(self, numAccounts, accountMb1, accountMb2, accountMb3) } ;
%%       teller_loop(self, accountMb1, accountMb2, accountMb3)
%%   }
%% }
-spec teller(integer()) -> no_return().
teller(Num_accounts) ->
  ?mb_new(account_mb),
  Account1 = spawn(?MODULE, account, [1, 200]),
  ?mb_new(account_mb),
  Account2 = spawn(?MODULE, account, [2, 150]),
  ?mb_new(account_mb),
  Account3 = spawn(?MODULE, account, [3, 50]),

  ?mb_assert_regex("Start"),
  receive
    {start} ->
      ?mb_new(teller_mb),
      spawn(?MODULE, generate_work, [Num_accounts, Account1, Account2, Account3]),
      teller_loop(Account1, Account3, Account2)
  end.


%% def generate_work(tellerMb: TellerMb!, numAccounts: Int, acc1: AccountMb![R], acc2: AccountMb![R], acc3 : AccountMb![R]): Unit {
%%   # Randomly choose source account from which the funds shall be taken.
%%   let sourceId = rand(numAccounts - 1) in # -1 because rand() is 0-indexed.
%%   if (sourceId == 0) {
%%     # First source account.
%%     choose_dst_acc(tellerMb, numAccounts, acc1, acc2, acc3)
%%   }
%%   else {
%%     if (sourceId == 1) {
%%         # Second source account.
%%         choose_dst_acc(tellerMb, numAccounts, acc2, acc1, acc3)
%%     }
%%     else {
%%         # Third source account.
%%         choose_dst_acc(tellerMb, numAccounts, acc3, acc1, acc2)
%%     }
%%   }
%% }
-spec generate_work(teller_mb(), integer(), account_mb(), account_mb(), account_mb()) -> no_return().
generate_work(Teller, Num_accounts, Account1, Account2, Account3) ->
  Source_id = uniform(Num_accounts),
  if Source_id == 1 -> choose_dst_acc(Teller, Num_accounts, Account1, Account2, Account3);
    true ->
      if Source_id == 2 -> choose_dst_acc(Teller, Num_accounts, Account2, Account1, Account3);
        true ->
          choose_dst_acc(Teller, Num_accounts, Account3, Account1, Account2)
      end
  end.

%% def choose_dst_acc(tellerMb: TellerMb!, numAccounts: Int, srcAccountMb: AccountMb![R], dstAccountMb1: AccountMb![R], dstAccountMb2 : AccountMb![R]): Unit {
%%   let dstAccountId = rand(numAccounts - 2) in
%%   let dstAccount =
%%     if (dstAccountId == 0) {
%%       dstAccountMb1
%%     } else {
%%       dstAccountMb2
%%     }
%%   in
%%   let amount = rand(200) in
%%   dstAccount ! Credit(tellerMb, amount, srcAccountMb)
%% }
-spec choose_dst_acc(teller_mb(), integer(), account_mb(), account_mb(), account_mb()) -> no_return().
choose_dst_acc(Teller, Num_accounts, Src_account, Dst_account1, Dst_account2) ->
  Dst_account_id = uniform(Num_accounts - 1),
  Dst_account =
    if Dst_account_id == 1 -> Dst_account1;
      true -> Dst_account2
    end,
  Dst_account ! {credit, Teller, 2, Src_account}.

%% def teller_loop(self: TellerMb?, accountMb1: AccountMb!, accountMb2: AccountMb!, accountMb3: AccountMb!): Unit {
%%   guard self: *Reply {
%%     free ->
%%       # All credit requests serviced. Stop accounts.
%%       accountMb1 ! Stop();
%%       accountMb2 ! Stop();
%%       accountMb3 ! Stop()
%%     receive Reply() from self ->
%%       teller_loop(self, accountMb1, accountMb2, accountMb3)
%%   }
%% }
-spec teller_loop(account_mb(), account_mb(), account_mb()) -> no_return().
teller_loop(Account1, Account2, Account3) ->
%%  TODO No idea about free
%%  ?mb_state_free or ?free
  ?mb_assert_regex("*Reply"),
  receive
    {reply} ->
      teller_loop(Account1, Account2, Account3)
  end.

%% def account(self: AccountMb?, id: Int, balance: Int): Unit {
%%   guard self: *(Debit + Credit) . Stop {
%%     free ->
%%       ()
%%     receive Debit(accountMb, amount) from self ->
%%
%%       accountMb ! Done();
%%       account(self, id, balance + amount)
%%     receive Credit(tellerMb, amount, accountMb) from self ->
%%
%%       # A more uglier implementation would have been to use the 'global mailbox
%%       # way' where all messages are collected in one mailbox.
%%       let transMb = new [AccountMb] in
%%       accountMb ! Debit(transMb, amount);
%%
%%       guard transMb: Done + 1{
%%         free ->
%%           account(self, id, balance)
%%         receive Done() from transMb ->
%%           free(transMb);
%%           tellerMb ! Reply();
%%           account(self, id, balance - amount)
%%       }
%%
%%     receive Stop() from self ->
%%       account_exit(self)
%%   }
%% }
-spec account(integer(), integer()) -> no_return().
account(Id, Balance) ->
  Self = self(),
  ?mb_assert_regex("*(Debit + Credit) . Stop"),
  receive
    {debit, Src_account, Amount} ->
      Src_account ! {done},
      account(Id, Balance + Amount);
    {credit, Teller, Amount, Des_account} ->
      Des_account ! {debit, Self, Amount},
      ?mb_assert_regex("Done + 1"),
      receive
        {done} ->
          Teller ! {reply},
          account(Id, Balance - Amount)
      end;
    {stop} ->
      account_exit()
  end.

%%  def account_exit(self: AccountMb?): Unit {
%%    guard self: *(Debit + Credit)  {
%%      free -> ()
%%      receive Debit(accountMb, amount) from self ->
%%        account_exit(self)
%%      receive Credit(tellerMb, amount, accountMb) from self ->
%%        account_exit(self)
%%    }
%%  }
-spec account_exit() -> no_return().
account_exit() ->
%%  ?free,
  ?mb_assert_regex("*(Debit + Credit)"),
  receive
    {debit, Account, Amount} ->
      account_exit();
    {credit, Teller, Amount, Account} ->
      account_exit()
  end.

%%  def main(): Unit {
%%    let tellerMb = new [TellerMb] in
%%    spawn { teller(tellerMb, 3) };
%%    tellerMb ! Start()
%%  }
-spec main() -> no_return().
main() ->
  ?mb_new(teller_mb),
  Teller = spawn(?MODULE, teller, [3]),
  Teller ! {start}.
