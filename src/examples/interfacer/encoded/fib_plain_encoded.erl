%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I)
%%% and -interface f :: I(). is collected into one -interface([...]) at
%%% the top. Read ../fib_plain.erl for the proposed notation.
%%%
%%% Plain Interfacer version of Savina fib.
%%%
%%% Adapted from Savina/fib.
%%% Server that calculates the Fibonacci number sent in client requests.
%%%-------------------------------------------------------------------
-module(fib_plain_encoded).

-include("interfacer.hrl").
-import(io, [format/2]).

-export([main/0]).
-export([fib/0]).
-export_type([req/0, resp/0, fib_in/0]).

-interface([{fib, fib_in},
            {main, fib_in}]).

%%% Messages.
-type req() :: {req, pid_of(fib_in()), integer()}.
-type resp() :: {resp, integer()}.

%%% Interfaces.
-type fib_in() :: req() | resp().

%% @doc Fibonacci process computing the (n - 1)st and (n - 2)nd terms.
-spec fib() -> resp().
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
            FibPid1 = spawn(?MODULE, fib, []),
            FibPid2 = spawn(?MODULE, fib, []),
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
  FibPid = spawn(?MODULE, fib, []),
  Self = self(),
  FibPid ! {req, Self, 16},
  receive
    {resp, Term} ->
      format("Result: ~p.~n", [Term])
  end.

%% Encoded form of ../fib_plain.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this file alone:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/fib_plain_encoded.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'fib_plain_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize fib_plain_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/fib_plain_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/fib_plain_encoded.erl
