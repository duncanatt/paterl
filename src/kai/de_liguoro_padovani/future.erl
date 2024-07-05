%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2023 16:53
%%%-------------------------------------------------------------------
-module(future).
-author("duncan").

-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future/0]).

-new({future_mb, [future/0]}).
-use({future_mb, [resolved_future/1]}).
-new({user_mb, [user/1]}).
-new({main_mb, [main/0]}).

%%% Type definitions.

-type put() :: {put, integer()}.
-type get() :: {get, user_mb()}.

-type reply() :: {reply, integer()}.

-type future_mb() :: pid() | put() | get().
-type user_mb() :: pid() | reply().
-type main_mb() :: pid().

%%% API.

%% def future(self: Future?): Unit {
%%   guard self : Put.(*Get) {
%%     receive Put(x) from self -> resolvedFuture(self, x)
%%   }
%% }
-spec future() -> no_return().
future() ->
  %% @mb future()
  ?mb_assert_regex("Put.(*Get)"),
  receive {put, Value} ->
    resolved_future(Value)
  end.

%% def resolvedFuture(self: Future?, value: Int): Unit {
%%   guard self : *Get {
%%     free -> ()
%%     receive Put(x) from self ->
%%       fail(self)[Unit]
%%     receive Get(user) from self ->
%%       user ! Reply(value);
%%       resolvedFuture(self, value)
%%   }
%% }
-spec resolved_future(integer()) -> no_return().
resolved_future(Value) ->
  %% @mb future()
  %% @assert get*
  ?mb_assert_regex("*Get"),
  receive
    {get, UserPid} ->
      UserPid ! {reply, Value},
      %% @use future()
      resolved_future(Value)
  end.

%% def user(future: Future!): Int {
%%   let self = new[User] in
%%   future ! Get(self);
%%   guard self : Reply {
%%     receive Reply(x) from self ->
%%     free(self);
%%     x
%%   }
%% }
-spec user(future_mb()) -> integer().
%% @new user()
user(FuturePid) ->
  Self = self(),
  FuturePid ! {get, Self},

  %% @mb user()
  %% @assert reply
  ?mb_assert_regex("Reply"),
  receive
    {reply, Value} ->
      Value
  end.

%% def main(): Unit {
%%   let future_mb = new[Future] in
%%   spawn { future(future_mb) };
%%   future_mb ! Put(5);
%%   future_mb ! Put(5);
%%   print(intToString(user(future_mb)));
%%   print(intToString(user(future_mb)))
%% }
-spec main() -> no_return().
%% @new user()
main() ->
  ?mb_new(future_mb),
  FuturePid = spawn(?MODULE, future, []),
  FuturePid ! {put, 5},
  Get1 = user(FuturePid),
 format("A: ~p~n", [Get1]).
%%  Get2 = user(FuturePid), format("B: ~p~n", [Get2]).
