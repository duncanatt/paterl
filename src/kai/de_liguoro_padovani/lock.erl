%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 7月 2024 23:45
%%%-------------------------------------------------------------------
-module(lock).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([free_lock/0, user/2]).

%% Mailbox interface-function associations
-new({lock_mb, [free_lock/0]}).
-use({lock_mb, [busy_lock/1]}).
-new({user_mb, [user/2]}).
-new({main_mb, [main/0]}).

%% Lock's message types
-type acquire() :: {acquire, user_mb()}.
-type release() :: {release}.

%% User's message types
-type reply() :: {reply, lock_mb()}.

%% # Mailbox message interfaces.
%% interface Lock {
%%    Acquire(User!),
%%    Release(Unit)
%% }
%%
%% interface User {
%%    Reply(Lock!)
%% }
-type lock_mb() :: pid() | acquire() | release().
-type user_mb() :: pid() | reply().
-type main_mb() :: pid().

%% def freeLock(self: Lock?): Unit {
%%   guard (self) : *Acquire  {
%%     free -> ()
%%     receive Acquire(owner) from self ->
%%       busyLock(self, owner)
%%     receive Release(x) from self ->
%%       fail(self)[Unit]
%%   }
%% }
-spec free_lock() -> no_return().
free_lock() ->
  ?mb_assert_regex("*Acquire"),
  receive
    {acquire, Owner} ->
      busy_lock(Owner);
    {release} ->
%%      TODO: fail(self)[Unit]
      ok
  end.

%% def busyLock(self: Lock?, owner: User!): Unit {
%%   owner ! Reply(self);
%%   guard (self) : (*Acquire).Release {
%%     receive Release(x) from self ->
%%       freeLock(self)
%%   }
%% }
-spec busy_lock(user_mb()) -> no_return().
busy_lock(Owner) ->
  Self = self(),
  Owner ! {reply, Self},
  ?mb_assert_regex("(*Acquire) . Release"),
  receive
    {release} ->
      free_lock()
  end.

%% def user(num: Int, lock: Lock!): Unit {
%%   let self = new[User] in
%%   lock ! Acquire(self);
%%   guard(self) : Reply {
%%     receive Reply(lock) from self ->
%%       print(intToString(num));
%%       lock ! Release(());
%%       free(self)
%%   }
%% }
-spec user(integer(), lock_mb()) -> no_return().
user(Num, Lock) ->
  Self = self(),
  Lock ! {acquire, Self},
  ?mb_assert_regex("Reply"),
  receive
    {reply, Lock} ->
      format("~p~n", [Num]),
      Lock ! {release}
  end.

%% def main(): Unit {
%%   let lock = new[Lock] in
%%   spawn { freeLock(lock) };
%%   spawn { user(1, lock) };
%%   spawn { user(2, lock) }
%% }
-spec main() -> no_return().
main() ->
  ?mb_new(lock_mb),
  Lock = spawn(?MODULE, free_lock, []),
  ?mb_new(user_mb),
  spawn(fun() -> user(1, Lock) end),
  ?mb_new(user_mb),
  spawn(fun() -> user(2, Lock) end).