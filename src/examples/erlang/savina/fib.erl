%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/fib
%%%
%%% Server that calculates the Fibonacci number sent in client requests.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(fib).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([fib/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Fib.
-type req() :: {req, fib_mb(), integer()}.
-type resp() :: {resp, integer()}.

%%% Interfaces.

%% Fib.
-type fib_mb() :: pid() | req() | resp().

%%% Interface-function associations.

%% Fib.
-new({fib_mb, [fib/0, main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Fibonacci process computing the (n - 1)st and (n - 2)nd terms.
-spec fib() -> no_return().
fib() ->
  ?mb_assert_regex("Req"),
  receive
    {req, ReplyTo, N} ->
      Term =
        if N =< 2 ->
          % Base case.
          1;
          true ->
            % Inductive case (n - 1) and (n - 2). Delegate computation of the
            % (n - 1)st and (n - 2)nd term to fib process replicas.
            ?mb_new(fib_mb),
            FibPid1 = spawn(?MODULE, fib, []),
            ?mb_new(fib_mb),
            FibPid2 = spawn(?MODULE, fib, []),

            Self = self(),
            FibPid1 ! {req, Self, N - 1},
            FibPid2 ! {req, Self, N - 2},

            % Combine results computed for the (n - 1)st and (n - 2)nd terms.
            ?mb_assert_regex("Resp.Resp"),
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

%% @doc Launcher.
-spec main() -> any().
main() ->
  ?mb_new(fib_mb),
  FibPid1 = spawn(?MODULE, fib, []),

  Self = self(),
  FibPid1 ! {req, Self, 16},

  ?mb_assert_regex("Resp"),
  receive
    {resp, Term} ->
      format("Result: ~p.~n", [Term])
  end.


%% ./src/paterl src/examples/erlang/savina/fib.erl -v all -I include