%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 7月 2024 10:29
%%%-------------------------------------------------------------------
-module(products).
-author("walker").

%% API
-export([]).

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

%% Internal exports
-export([nested/0, pairs/0]).

%% Mailbox interface-function associations
-new({test_mb, [nested/0]}).
-used({test_mb, [pairs/0]}).
-new({main_mb, [main/0]}).

%% Test's message types
-type arg() :: {arg, integer()}.

%% # Mailbox message interfaces.
%% interface Test { Arg(Int) }
-type test_mb() :: pid() | arg().
-type main_mb() :: pid().

%% def nested(mb: Test?): Unit {
%%   guard mb : Arg . Arg {
%%     receive Arg(x) from mb1 ->
%%       guard mb1 : Arg {
%%         receive Arg(y) from mb2 ->
%%           free(mb2); print(intToString(x + y))
%%       }
%%   }
%% }
-spec nested() -> no_return().
nested() ->
  ?expects("Arg . Arg"),
  receive
    {arg, X} ->
      ?expects("Arg"),
      receive
        {arg, Y} ->
          format("~p~n", [X + Y])
      end
  end.

%% def pairs(mb: Test?): Unit {
%%   let (x, mb1) =
%%     guard mb : Arg . Arg {
%%       receive Arg(x) from mb1 -> (x, mb1)
%%     }
%%   in
%%   guard mb1 : Arg {
%%     receive Arg(y) from mb2 ->
%%       free(mb2); print(intToString(x + y))
%%   }
%% }
-spec pairs() -> no_return().
pairs() ->
  ?expects("Arg . Arg"),
  First = receive {arg, X} -> X end,
  ?expects("Arg"),
  receive {arg, Y} -> format("~p~n", [First + Y]) end.

%% def main(): Unit {
%%   let mb1 = new[Test] in
%%   mb1 ! Arg(1);
%%   mb1 ! Arg(2);
%%   spawn { nested(mb1) };

%%   let mb2 = new[Test] in
%%   mb2 ! Arg(1);
%%   mb2 ! Arg(2);
%%   spawn { pairs(mb2) }
%% }
-spec main() -> no_return().
main() ->
  ?mb_new(test_mb),
  MB1 = spawn(?MODULE, nested, []),
  MB1 ! {arg, 1},
  MB1 ! {arg, 2},

  ?mb_new(test_mb),
  MB2 = spawn(?MODULE, pairs, []),
  MB2 ! {arg, 1},
  MB2 ! {arg, 2}.


