%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jun 2023 18:01
%%%-------------------------------------------------------------------
-module(account).
-author("duncan").

%%% Includes.
-include("paterl.hrl").
-include("log.hrl").

-import(io, [format/2]).
%%-import(log, [write/4, write/5]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([account/1]).

%% interface Account {
%%   Debit(Int, Barrier!),
%%   Credit(Int, Account!, Barrier!)
%% }

%% Message types.
-type debit() :: {debit, integer(), account()}. % We should never allow pid() here.
%%-type credit() :: {credit, integer(), account(), barrier()}. % We have a problem here due to the first transaction being initiated from main(). Using account() instead of barrier() would not be correct.
-type credit() :: {credit, integer(), account(), account()}. % We have a problem here due to the first transaction being initiated from main(). Using account() instead of barrier() would not be correct.


-type reply() :: {reply}.

%% Mailbox interfaces.
%%-type barrier() :: pid() | reply().
-type account() :: pid() | debit() | credit() | reply().

%% Mailbox interface-function associations.
%%-barrier([await/0]). % Means that the functions will be automatically parametrized with 'barrier' in generated Pat.
-account([await/0, account/1, main/0]).
% These attributes specify the functions that are compatible with the interface. For instance, main() below is
% not an instance of an account process, but it adheres to the account() mb interface in that it can process
% a subset of the messages defined in the account() type union. In particular, it handles reply messages. So, by this
% definition, it is compatible and should be placed in the list of accounts.



%% def notify(barrier: Barrier!): Unit {
%%   barrier ! Reply()
%% }
-spec notify(account()) -> {reply}.
notify(Account) ->
%%  ?TRACE("Sending ack to ~p.", [Account]),
  Account ! {reply}.

%% def await(barrier: Barrier?): Unit {
%%   guard barrier : Reply {
%%     receive Reply() from barrier ->
%%       free(barrier)
%%     }
%% }
-spec await() -> no_return().
await() -> % Mailbox 'account' will be injected as first 'self' parameter based on mailbox interface-function association above.
%%  ?TRACE("Waiting for ack.."),

  ?mb_state("reply"),
  receive
    {reply} ->
%%      ?TRACE("Ack received"),
      ?free
  end.

%% def account(self: Account?, balance: Int): Unit {
%%   guard self : ((*Debit) . (*Credit))  {
%%     # receive Stop() from self -> free(self)
%%     free -> ()
%%
%%     receive Debit(amount, ack) from self ->
%%       notify(ack);
%%       account(self, balance - amount)
%%
%%     receive Credit(amount, payer, ack) from self ->
%%
%%       # Create new barrier for this transaction
%%       let barrier = new[Barrier] in
%%
%%       # Debit the payer
%%       payer ! Debit(amount, barrier);
%%
%%       # Wait for confirmation
%%       await(barrier);
%%
%%       # Notify initiator of this transaction
%%       notify(ack);
%%       account(self, balance + amount)
%%   }
%% }
-spec account(integer()) -> no_return().
account(Balance) ->
  ?mb_state_free("debit*.credit*"),
  receive
    {debit, Amount, Ack} ->
%%      ?TRACE("Handling debit request ~p.", [Amount]),

      notify(Ack),
      account(Balance - Amount);

    {credit, Amount, Payer, Ack} ->
%%      ?TRACE("Handling credit request ~p to ~p.", [Amount, Payer]),

      % Debit payer and wait for confirmation from recipient account.
      Payer ! {debit, Amount, self()},
      await(),

      notify(Ack), % Unblock barrier process which will die.
      account(Balance + Amount)
  end.

%% def main(): Unit {
%%
%%   # Alice
%%   let alice = new[Account] in
%%   spawn { account(alice, 10) };
%%
%%   # Bob
%%   let bob = new[Account] in
%%   spawn { account(bob, 15) };
%%
%%   ### Ack
%%   let barrier = new[Barrier] in
%%   ####
%%
%%   alice ! Credit(10, bob, barrier);
%%   await(barrier)
%% }

-spec main() -> ok.
main() ->
  Alice = spawn(?MODULE, account, [10]),
%%  ?TRACE("Created Alice ~p.", [Alice]),
  Bob = spawn(?MODULE, account, [15]),
%%  ?TRACE("Created Bob ~p.", [Bob]),

  Alice ! {credit, 10, Bob, self()},
  await().


% Scheme for local mailboxes: Translate a new local mailbox by unifying all the other messages under one interface.
% Show the type and interface scheme.
% Show that it passed Dialyzer checks.