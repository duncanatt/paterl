%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2023 09:48
%%%-------------------------------------------------------------------
-module(fib).
-author("duncan").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([fib/0]).

%%% Type definitions.

%% Fib interface.
%% interface FibMb { Req(FibMb!, Int), Resp(Int) }
%% @type fib_mb() :: {req, fib_mb(), integer()} | {resp, integer()} (reference to mailbox is always ! because ? cannot be delegated)

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
%% @spec fib() -> none()
%% @new fib_mb()
fib() ->
  %% @mb fib_mb()
  %% @assert req
  receive
    {req, ReplyTo, N} ->
      Term =
        if N =< 2 ->
          1;
        true ->
          FibPid1 =
            %% @new fib_mb()
            spawn(?MODULE, fib, []),
          FibPid2 =
            %% @new fib_mb()
            spawn(?MODULE, fib, []),

          Self =
            %% @mb fib_mb()
            self(),
          FibPid1 ! {req, Self, N - 1},
          FibPid2 ! {req, Self, N - 2},

          %% @mb fib_mb()
          %% @assert resp.resp
          receive
            {resp, Term1} ->
              %% @mb fib_mb()
              %% @assert resp
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
%% @spec main() -> none()
%% @new fib_mb()
main() ->
  FibPid1 =
    %% @new fib_mb()
    spawn(?MODULE, fib, []),

  Self =
    %% @mb fib_mb()
    self(),
  FibPid1 ! {req, Self, 16},

  %% @mb fib_mb()
  %% @assert Resp
  receive
    {resp, Term} ->
      format("Result: ~p~n", [Term])
  end.
