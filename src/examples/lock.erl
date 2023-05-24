%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. May 2023 18:50
%%%-------------------------------------------------------------------
-module(lock).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% API.
-export([main/0]).

%% Internal exports.
-export([free_lock/0, user/2]).

%% interface Lock { Acquire(User!), Release(Unit) }
%% interface User { Reply(Lock!) }

%% Mailbox interface-function associations.
-lock([free_lock/0, busy_lock/1]).
-user([user/2]).

%% Message types.
-type acquire() :: {acquire, user()}.
-type release() :: {release}.
-type reply() :: {reply, lock()}.

%% Mailbox interfaces.
-type lock() :: pid() | acquire() | release().
-type user() :: pid() | reply().

%% def freeLock(self: Lock?): Unit {
%%   guard (self) : *Acquire  {
%%     free -> ()
%%     receive Acquire(owner) from self ->
%%       busyLock(self, owner)
%%         receive Release(x) from self ->
%%           fail(self)[Unit]
%%   }
%% }
-spec free_lock() -> none().
free_lock() ->
  ?mb_state_free("acquire*"),
  % Whenever we have a *, we generate a free guard clause in Pat, unless
  % explicitly told not to do so. This is a crutch for now. It should be auto
  % inferred. But test all the other examples first.
  receive
    {acquire, Owner} ->
      busy_lock(Owner),
      receive
        {release} ->
          fail
      end
  end.

%% def busyLock(self: Lock?, owner: User!): Unit {
%%   owner ! Reply(self);
%%   guard (self) : (*Acquire).Release {
%%     receive Release(x) from self ->
%%       freeLock(self)
%%   }
%% }
-spec busy_lock(user()) -> none().
busy_lock(Owner) ->
  Owner ! {reply, self()},
  ?mb_state("acquire*.release", no_free),
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
-spec user(integer(), lock()) -> none().
user(Num, Lock) -> % TODO: We need a built-in function free or a macro free.

  Lock ! {acquire, self()},
  ?mb_state("reply"),
  receive
    {reply, Lock} ->
      io:format("User num: ~p~n", [Num]),
      Lock ! {release},
      ?free
  end.

%% def main(): Unit {
%%   let lock = new[Lock] in
%%   spawn { freeLock(lock) };
%%   spawn { user(1, lock) };
%%   spawn { user(2, lock) }
%% }
-spec main() -> none().
main() ->
  Lock = spawn(?MODULE, free_lock, []),
  spawn(?MODULE, user, [1, Lock]),
  spawn(?MODULE, user, [2, Lock]).




% NOTES.
% Are there any specific rules to inserting free?
% The relationship is between the starred messages in the regex and the clauses
% in Erlang receive:
% 1. If the regex is empty, a free must be inserted in the Pat guard.
% 2. If the regex is not empty, and there is a starred message in the regex:
%    a. Insert a free if the message in the Erlang receive has a clause with
%       that message type.
%    b. Do not insert free if the starred message in the regex does not appear
%       in as an Erlang clause in receive.
% For this to be implementable, we need to parse the commutative regex.

% Test this theory in pat by having "acquire*.release" and then consuming
% release and then freeing acquire. I suppose it should work? In pipeline.
