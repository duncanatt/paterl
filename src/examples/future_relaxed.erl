%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. May 2023 18:33
%%%-------------------------------------------------------------------
-module(future_relaxed).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future/0]).

%% Mailbox interface-function associations.

-new({future_mb, [future/0]}).
-use({future_mb, [resolved_future/1]}).
-new({user_mb, [user/1]}).
-new({main_mb, [main/0]}).

%% Message types.
-type put() :: {put, integer()}.
-type get() :: {get, user_mb()}.
-type reply() :: {reply, integer()}.

%% Mailbox interfaces.
-type future_mb() :: pid() | put() | get().
-type user_mb() :: pid() | reply().
-type main_mb() :: pid().

-spec future() -> no_return().
future() ->
  ?mb_assert_regex("put.*get"),
  receive
    {put, X} ->
      resolved_future(X)
  end.

-spec resolved_future(integer()) -> no_return().
resolved_future(X) ->
  ?mb_assert_regex("*get"),
  receive
    {get, User} ->
      T0 = User ! {reply, X},
      resolved_future(X)
  end.

-spec user(future_mb()) -> integer().
user(Future) ->
  Self = self(),
  T2 = Future ! {get, Self},
  ?mb_assert_regex("reply"),
  receive
    {reply, X} ->
      T3 = X
  end.

-spec main() -> any().
main() ->
  ?mb_new(future_mb),
  Future_mb = spawn(?MODULE, future, []),
  T4 = Future_mb ! {put, 5},
  T5 = format("Got ~p.~n", [user(Future_mb)]).












