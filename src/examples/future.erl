%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. May 2023 18:33
%%%-------------------------------------------------------------------
-module(future).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future_fun/0]).

%% interface Future { Put(Int), Get(User!) }
%% interface User   { Reply(Int) }

%% Mailbox interface-function associations.
%% E.g. The mailbox interface future() is used by future_fun/0, full_future/1.
-future([future_fun/0, resolved_future/1]).
-user([user/1]).

%% Message types.
-type put() :: {put, integer()}.
-type get() :: {get, user()}.
-type reply() :: {reply, integer()}.

%% Mailbox interfaces.
%%-type future() :: pid() | put() | get().
-type future() :: put() | get().
-type user() :: pid() | reply().
%%-type user() :: pid().
%%-type user() :: reply().
-type dummy() :: pid().

%% def future(self: Future?): Unit {
%%   guard self : Put.(*Get) {
%%     receive Put(x) from self ->
%%       resolvedFuture(self, x)
%%   }
%% }
-spec future_fun() -> no_return().
future_fun() ->
  ?mb_state("put.get*"),
  receive
    {put, X} -> % of type put().
      resolved_future(X)
  end.

%% def resolvedFuture(self: Future?, value: Int): Unit {
%%   guard self : *Get {
%%     free -> ()
%%     receive Get(user) from self ->
%%       user ! Reply(value);
%%       resolvedFuture(self, value)
%%   }
%% }
-spec resolved_future(integer()) -> no_return(). % I need to support none() type as the unit.
resolved_future(X) ->
  ?mb_state("get*"),
  % Parse the string and insert a free clause (because of *) in the generated
  % guard expression in Pat, unless user explicitly states not to do so. This
  % is a crutch for now. I will understand how this works later (see notes in
  % lock.erl).
  receive
    {get, User} -> % of type get().
      User ! {reply, X},
      resolved_future(X)
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
-spec user(future()) -> integer().
user(Future) ->
  Future ! {get, self()},
  ?mb_state("reply"),
  receive
    {reply, X} -> % of type reply().
      ?free,
      X
  end.

%% def main(): Unit {
%%   let future_mb = new[Future] in
%%   spawn { future(future_mb) };
%%   future_mb ! Put(5);
%%   print(intToString(user(future_mb)));
%%   print(intToString(user(future_mb)))
%% }
%%-spec main() -> ok.
main() ->
  Future = spawn(?MODULE, future_fun, []),
  Future ! {put, 5},

  format("Got ~p.~n", [user(Future)]),
  format("Got ~p.~n", [user(Future)]).












