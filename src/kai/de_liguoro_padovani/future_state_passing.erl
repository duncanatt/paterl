%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 7月 2024 22:17
%%%-------------------------------------------------------------------
-module(future_state_passing).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([future/0]).

%% Mailbox interface-function associations
-new({future_mb, [future/0]}).
-use({future_mb, [resolved_future/1]}).
-new({user_mb, [user/1]}).
-new({main_mb, [main/0]}).

%% Future's message types
-type put() :: {put, integer()}.
-type get() :: {get, user_mb()}.

%% User's message types
-type reply() :: {reply, integer()}.

%% # Mailbox message interfaces.
%% interface Future {
%%    Put(Int),
%%    Get(User!)
%% }
%%
%% interface User {
%%    Reply(Int)
%% }
-type future_mb() :: pid() | put() | get().
-type user_mb() :: pid() | reply().
-type main_mb() :: pid().

%% def future(self: Future?): (Unit * Future?1) {
%%   guard self : Put.(*Get) {
%%     receive Put(x) from self -> resolvedFuture(self, x)
%%   }
%% }
-spec future() -> no_return().
future() ->
  ?mb_assert_regex("Put . (*Get)"),
  receive
    {put, X} ->
      resolved_future(X)
  end.

%% def resolvedFuture(self: Future?, value: Int): (Unit * Future?1) {
%%   guard self : *Get {
%%     empty(x) -> ((), x)
%%     receive Get(user) from self ->
%%       user ! Reply(value);
%%       resolvedFuture(self, value)
%%   }
%% }
-spec resolved_future(integer()) -> no_return().
resolved_future(Value) ->
  ?mb_assert_regex("*Get"),
  receive
    {get, User} ->
      User ! {reply, Value},
      resolved_future(Value)
  end.

%% def user(future: Future!): Int {
%%   let self = new[User] in
%%   future ! Get(self);
%%   guard self : Reply {
%%     receive Reply(x) from self ->
%%       free(self);
%%       x
%%   }
%% }
-spec user(future_mb()) -> integer().
user(Future) ->
  Self = self(),
  Future ! {get, Self},
  ?mb_assert_regex("Reply"),
  receive
    {reply, X} -> X
  end.

%% def main(): Unit {
%%   # Test comment
%%   let future_mb = new[Future] in
%%   spawn { let (x, mb) : (Unit * Future?1) = future(future_mb) in free(mb) };
%%   future_mb ! Put(5);
%%   print(intToString(user(future_mb)));
%%   print(intToString(user(future_mb)))
%% }
%% main()
-spec main() -> no_return().
main() ->
  ?mb_new(future_mb),
  FutureMb = spawn(?MODULE, future, []),
  FutureMb ! {put, 5},
  format("~p~n", [user(FutureMb)]),
  format("~p~n", [user(FutureMb)]).