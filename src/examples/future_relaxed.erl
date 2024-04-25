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
  ?mb_assert_regex("Put.*Get"),
  receive
    {put, X} ->
      resolved_future(X)
  end.

-spec resolved_future(integer()) -> no_return().
resolved_future(X) ->
  ?mb_assert_regex("*Get"),
  receive
    {get, User} ->
      T0 = User ! {reply, X},
%%      TX = 5,
      resolved_future(X)
  end.

-spec user(future_mb()) -> integer().
user(Future) ->
  Self = self(),
  T0 = Future ! {get, Self},
  ?mb_assert_regex("Reply"),
  receive
    {reply, X} ->
      X
  end.

-spec main() -> any().
main() ->
  ?mb_new(future_mb),
  Future_mb = spawn(?MODULE, future, []),
  T0 = Future_mb ! {put, 5},
  A = user(Future_mb),
  T1 = format("~s", [A]),
  if (1 == 1) -> T1; true -> T1 end.

-spec main0() -> any().
main0() ->
  main().


%%-spec a_non_annotated_fun(string(), integer(), float(), atom()) -> any().
%%a_non_annotated_fun(Var, 1, 2.0, hello) ->
%%  if 5 == 2 -> five_is_two; true -> five_is_not_two, "but two is not", 5 end,
%%  a_non_annotated_fun(Var, 1, 2.0, hello).










