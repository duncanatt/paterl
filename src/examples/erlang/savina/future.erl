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

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future/0]).


%%% Type definitions.

%% Future interface (a la EDoc annotations!).
%% interface Future { Put(Int), Get(User!) }
%% @type future() :: {put, integer()} | {get, user()} (don't need the pid if I'm not using dialyzer)

%% User interface.
%% interface User { Reply(Int) }
%% @type user() :: {reply, integer()} | (don't need the pid if I'm not using dialyzer)

%%% API.

%% def future(self: Future?): Unit {
%%   guard self : Put.(*Get) {
%%     receive Put(x) from self -> resolvedFuture(self, x)
%%   }
%% }
%% @spec future() -> none()
%% @new future()
future() ->
  %% @mb future()
  %% @assert put.get*
  receive {put, Value} ->
    %% @use future() (@use is derived from the interface of the resolved_future function)
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
%% @spec resolved_future(integer()) -> none()
%% @use future()
resolved_future(Value) ->
  %% @mb future()
  %% @assert get*
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
%% @spec user(future()) -> integer()
%% @new user()
user(FuturePid) ->
  Self =
    %% @mb user()
    self(),
  FuturePid ! {get, Self},

  %% @mb user()
  %% @assert reply
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
%% @spec  main() -> any()
%% @new user()
main() ->
  FuturePid =
    %% @new future()
    spawn(?MODULE, future, []),
  FuturePid ! {put, 5},

  Get1 =
    %% @new user
    user(FuturePid),
  format("A: ~p~n", [Get1]),
  Get2 =
    %% @new user
    user(FuturePid),
  format("B: ~p~n", [Get2]).g
