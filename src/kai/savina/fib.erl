%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 7月 2024 17:38
%%%-------------------------------------------------------------------
-module(fib).
-author("walker").

-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([fib/0]).

%%% Mailbox interface-function associations.
-new({fib_mb, [main/0]}).
-use({fib_mb, [fib/0]}).

%%% Type definitions.

-type req() :: {req, fib_mb(), integer()}.
-type resp() :: {resp, integer()}.

%% interface FibMb {
%%  Req(FibMb!, Int),
%%  Resp(Int)
%% }
-type fib_mb() :: pid() | req() | resp().


%%% API.

%% def fib(self: FibMb?): Unit {
%%   guard self: Req {
%%     receive Req(replyTo, n) from self ->
%%       let term =
%%         if (n <= 2) {
%%           # Base case.
%%           free(self);
%%           1
%%         }
%%         else {
%%           # Inductive case (n - 1) and (n - 2). Delegate computation of (n - 1)st
%%           # and (n - 2)nd term to fib process replicas.
%%           let fib1Mb = new [FibMb] in
%%           spawn { fib(fib1Mb) };
%%
%%           let fib2Mb = new [FibMb] in
%%           spawn { fib(fib2Mb) };
%%
%%           fib1Mb ! Req(self, n - 1);
%%           fib2Mb ! Req(self, n - 2);
%%
%%           # Combine results computed for the (n - 1)st and (n - 2)nd terms.
%%           guard self: Resp . Resp {
%%             receive Resp(f1) from self ->
%%               guard self: Resp {
%%                 receive Resp(f2) from self ->
%%                   free(self);
%%                   f1 + f2
%%               }
%%           }
%%         } in
%%       replyTo ! Resp(term)
%%   }
%% }
-spec fib() -> no_return().
fib() ->
  ?mb_assert_regex("Req"),
  receive
    {req, ReplyTo, N} ->
      Term =
        if N =< 2 ->
          1;
          true ->
            ?mb_new(fib_mb),
            FibPid1 = spawn(?MODULE, fib, []),
            ?mb_new(fib_mb),
            FibPid2 = spawn(?MODULE, fib, []),

            Self = self(),
            FibPid1 ! {req, Self, N - 1},
            FibPid2 ! {req, Self, N - 2},

            ?mb_assert_regex("Resp"),
            receive
              {resp, Term1} ->
                ?mb_assert_regex("Resp"),
                receive
                  {resp, Term2} ->
                    Term1 + Term2
                end
            end
        end,
      ReplyTo ! {resp, Term}
  end.

%% def main(): Unit {
%%   let fibMb = new [FibMb] in
%%   spawn { fib(fibMb) };
%%
%%   let self = new [FibMb] in
%%   fibMb ! Req(self, 5);
%%   guard self: Resp {
%%     receive Resp(f) from self ->
%%     free(self);
%%     print(concat("Result: ", intToString(f)))
%%   }
%% }
-spec main() -> no_return().
main() ->
  ?mb_new(fib_mb),
  FibPid1 = spawn(?MODULE, fib, []),

  Self = self(),
  FibPid1 ! {req, Self, 5},

  ?mb_assert_regex("Resp"),
  receive
    {resp, Term} ->
      format("Result: ~p~n", [Term])
  end.
