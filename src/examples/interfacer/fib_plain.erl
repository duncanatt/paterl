%%%-------------------------------------------------------------------
%%% Plain Interfacer version of Savina fib.
%%%
%%% Adapted from Savina/fib.
%%% Server that calculates the Fibonacci number sent in client requests.
%%%-------------------------------------------------------------------
-module(fib_plain).

-include("paterl.hrl").

-import(io, [format/2]).

-export([main/0]).
-export([fib/0]).
-export_type([req/0, resp/0, fib_if/0]).

%%% Messages.
-type req() :: {req, fib_if(), integer()}.
-type resp() :: {resp, integer()}.

%%% Interfaces.
-type fib_if() :: pid() | req() | resp().

%%% ?as before a statically named spawn should be inferred
%%% from the call graph of the spawned function. It is kept below for
%%% explicitness. Only ?as before self() should be required.

%% @doc Fibonacci process computing the (n - 1)st and (n - 2)nd terms.
-spec fib() -> no_return().
fib() ->
  receive
    {req, ReplyTo, N} ->
      Term =
        if N =< 2 ->
            % Base case.
            1;
          true ->
            % Inductive case: delegate computation of the (n - 1)st and
            % (n - 2)nd terms to fib process replicas.
            ?as(fib_if),
            FibPid1 = spawn(?MODULE, fib, []),
            ?as(fib_if),
            FibPid2 = spawn(?MODULE, fib, []),
            ?as(fib_if),
            Self = self(),
            FibPid1 ! {req, Self, N - 1},
            FibPid2 ! {req, Self, N - 2},
            % Combine results computed for the (n - 1)st and (n - 2)nd terms.
            receive
              {resp, Term1} ->
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
  ?as(fib_if),
  FibPid = spawn(?MODULE, fib, []),
  ?as(fib_if),
  Self = self(),
  FibPid ! {req, Self, 16},
  receive
    {resp, Term} ->
      format("Result: ~p.~n", [Term])
  end.

% erlc -I include -o ebin src/examples/interfacer/fib_plain.erl
% erl -pa ebin -noshell -eval 'fib_plain:main().' -s init stop
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize fib_plain
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check this file with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/fib_plain.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/fib_plain.erl
