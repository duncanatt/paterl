%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2023 16:53
%%%-------------------------------------------------------------------
-module(future_test).
%%-author("duncan").
%%-compile({parse_transform, main_pt}).

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future/0]).


%%% Type definitions.

-new({future_mb, [future/0]}).
%%-use({future_mb, [future/0]}).

-use({future_mb, [resolved_future/3]}).

-new({user_mb, [user/1]}).

-new({main_mb, [main/0]}).
%%-use({future_mb, [main/0]}).




%% Future interface (a la EDoc annotations!).
%% interface Future { Put(Int), Get(User!) }

-type simpl() :: ok.
-type tina() :: pid() | pid().

-type get() :: {get, user_mb()}
| pid()
| tina()
.

-type future_mb() ::
{put, integer(), integer()} |
%%{get, user_mb()} |
pid() |
get().
%%pid().

-type duncan(Key, Value, Elvis) :: {Key, Value, Elvis} |  Elvis | ok.

-type type_with_variable(TypeVariable) :: {TypeVariable} | tina.

%% User interface.
%% interface User { Reply(Int) }
%%-type user_mb() :: {reply, integer()} | pid().

%% Used to bootstrap the main function. Maybe inject an automatic interface?
%%-type main() :: ok.

%% Mailbox-function associations.
%%-future_mb([{new, future/0}, {use, [resolved_future/12]}]).
%%-future_mb({new, future/0}).
%%-future_mb({new, future/0}).
%%-future_mb(#{use => future/0}).
%%-user_mb([{new, user/1}]).
%%-main_mb([{new, main/0}]).


%%% API.

%% def future(self: Future?): Unit {
%%   guard self : Put.(*Get) {
%%     receive Put(x) from self -> resolvedFuture(self, x)
%%   }
%% }
-spec
future() ->
  no_return().
%%  | duncan.
-type main_mb() :: pid().
future() ->
  ?expects("put.get*"),
  receive
    {put, Value} ->
    resolved_future(Value, 42, hello)
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
%%-use future.
-spec resolved_future(integer(), integer(), any()) -> none().
resolved_future(Value, 42, State) ->
  ?expects("get*"),
%%  ok,
  receive
    {get, UserPid} ->
      UserPid ! {reply, Value},
      resolved_future(Value, 42, State)
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
user(FuturePid) ->
  Self =
    self(),
  FuturePid ! {get, Self},

  ?expects("reply"),
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
-spec main() -> any().
main() ->
  FuturePid =
    spawn(?MODULE, future, []),
  FuturePid ! {put, 5},

  if a == b -> hello; true ->
    GetX =
    user(FuturePid),
    goodbye
  end,

  Get1 =
    user(FuturePid),
  format("A: ~p~n", [Get1]),
  Get2 =
    user(FuturePid),
  format("B: ~p~n", [Get2]),
  a_non_annotated_fun("duncan", 1, 2.0, hello).%,
%%  ?mb_use(user_mb),
%%  ?mb_use(dun),
%%  Dun = ?mb_new(xxx), test.
%%  Dun = test.

-type
user_mb() :: {reply, integer()}
%%| pid()
.

-spec a_non_annotated_fun(string(), integer(), float(), atom()) -> any().
a_non_annotated_fun(Var, 1, 2.0, hello) ->
  if 5 == 2 -> five_is_two; true -> five_is_not_two, "but two is not", 5 end,
  a_non_annotated_fun(Var, 1, 2.0, hello).

