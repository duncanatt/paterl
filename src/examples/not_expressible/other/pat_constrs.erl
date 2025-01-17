%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 7月 2024 10:28
%%%-------------------------------------------------------------------
-module(pat_constrs).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([foo/0]).

%% Mailbox interface-function associations
-new({test_mb, [foo/0]}).
-new({main_mb, [main/0]}).

%% Test's message types
-type foo() :: {foo}.
-type bar() :: {bar}.

%% # Mailbox message interfaces.
%% interface Test { Foo(), Bar() }
-type test_mb() :: pid() | foo() | bar().
-type main_mb() :: pid().

%% def foo(x: Test?): Unit {
%%   guard x : Foo {
%%     receive Foo() from x -> free(x); ()
%%     receive Bar() from x -> free(x); ()
%%   }
%% }
-spec foo() -> no_return().
foo() ->
  ?expects("Foo"),
  receive
    {foo} ->
      ok;
    {bar} ->
      ok
  end.

%% def main(): Unit {
%%   let mb = new[Test] in
%%   spawn { foo(mb) };
%%   mb ! Foo()
%% }
-spec main() -> no_return().
main() ->
  MB = self(),
%%  ?mb_new(test_mb),
  spawn(?MODULE, foo, []),
  MB ! {foo}.