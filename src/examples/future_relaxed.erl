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
-import(rand, [uniform/0]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future/0]).

%% Mailbox interface-function associations.
%%-new({future_mb, [future/0, x/0]}).
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
%%  ?mb_assert_regex("Put"), % Uncomment for "unexpected Get"
  receive
    {put, X} ->
      resolved_future(X)
%%    {put, X} ->
%%      resolved_future(X)
  end.

-spec resolved_future(integer()) -> no_return().
resolved_future(X) ->
  ?mb_assert_regex("*Get"),
  receive
    {get, User} ->
      10,
      User ! {reply, X}, % Comment for "missing reply".
      resolved_future(X)
  end.

-spec user(future_mb()) -> integer().
user(Future) ->
  Self = self(),
  T0 = Future ! {get, Self},
%%  T0 = Future ! {get, Self}, % Uncomment for "extra Reply".
  ?mb_assert_regex("Reply"), % Add extra Reply and fix it with nested receive.
  receive
    {reply, X} ->
      10,
      X
  end,
  80.

-spec main() -> any().
main() ->
  ?mb_new(future_mb),
  Future_mb = spawn(?MODULE, future, []),
  Future_mb ! {put, 5}, % Comment out for "missing Put".
%%  Future_mb ! {put, 5}, % Uncomment for "extra Put".
  Boolean = true,
  A = user(Future_mb),
  5 + 6,
  T1 = format("~s", [A]),
  format("~s", [A]),
  a_non_annotated_fun("hello", 6 + 8, 900),
  if 5 =< 2 -> K = "five_is_two"; true -> X = "five_is_not_two", Y = "but two is not" end,
  if 5 == 2 -> "five_is_two"; true -> "five_is_not_two", "but two is not", "and third line" end,
  if 5 == 1 -> "TRUE"; true -> "FALSE" end,
  10,
  20,
  30,
  40,
  50,
  B = uniform(),
  T1.



-spec a_non_annotated_fun(string(), integer(), integer()) -> boolean().
a_non_annotated_fun(Var, A, B) ->
%%  U = if 5 == 2 -> K = "five_is_two"; true -> X = "five_is_not_two", Y = "but two is not" end,
%%  %: TODO: Ask Simon.
%%  U = if 5 == 2 -> "five_is_two"; true -> "five_is_not_two" end.
%%  T1 = format("Some text", []),
%%  G = 5 + 5,
%%  H = "String",
%%  a_non_annotated_fun(Var, 1, 2),
%%  ?mb_assert_regex("Regex"),
%%  O = receive
%%    {a, Var} ->
%%      5
%%  end,
  T = true,
  Last = a_non_annotated_fun(Var, 1, 2 + 3).

-spec send_ping(integer(), any(), any()) -> string().
send_ping(Id, Actor1, Actor2) ->
  if Id == 1 -> "TRUE"; true -> "FALSE" end.

%%-spec simple() -> integer().
%%simple() ->
%%  ?mb_new(future_mb),
%%  MyMb = spawn(?MODULE, x, []),
%%  5.

%%-spec x() -> integer().
%%x() ->
%%  5.

%%-spec main1() -> any().
%%main1() -> main().

%%-spec pure1() -> integer().
%%pure1() ->
%%  X = 5.
%%  % Expected: let x = 5 in x; Generated: let x = 5 in <EMPTY>.
%%
%%-spec pure2() -> integer().
%%pure2() ->
%%  X = 5,
%%  7.
%%  % Expected: let x = 5 in 7; Generated: let x = 5 in 7.
%%
%%-spec pure3() -> integer().
%%pure3() ->
%%  Y = X = 5.
%%  % Expected: let y = (let x = 5 in x) in y; Generated: let y = (let x = 5 in <EMPTY>) in <EMPTY>.
%%
%%-spec pure4() -> integer().
%%pure4() ->
%%  Z = Y = X = 5.
%%  % Expected: let z = (let y = (let x = 5 in x) in y) in z
%%
%%-spec pure5() -> integer().
%%pure5() ->
%%  X = 5,
%%  Y = X.
%%  % Expected: let x = 5 in let y = x in x; Generated: let x = 5 in let y = x in <EMPTY>.
%%
%%-spec pure6() -> integer().
%%pure6() ->
%%  Y = X = 5,
%%  7.
%%  % Expected: let y = (let x = 5 in x) in 7; Generated: let y = (let x = 5 in <EMPTY>) in 7

%%./src/paterl src/examples/future_relaxed.erl -v all -I include