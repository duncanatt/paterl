%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 15:15
%%%-------------------------------------------------------------------
-module(fib_pairs).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([fib_actor/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Fib actor.
-type request() :: {request, integer(), fib_actor_mb()}.
-type response() :: {response, integer()}.

%%% Interfaces.

%% Fib actor.
-type fib_actor_mb() :: pid() | request() | response().

%%% Interface-function associations.

%% Fib actor.
-new({fib_actor_mb, [fib_actor/0, main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Fibonacci process computing the (n - 1)st and (n - 2)nd terms.
-spec fib_actor() -> no_return().
fib_actor() ->
  ?expects("Request"),
  receive
    {request, N, Parent} ->
      if N =< 2 ->
        Parent ! {response, 1};
        true ->
          Self = self(),
          Child1 = spawn(?MODULE, fib_actor, []),
          Child1 ! {request, N - 1, Self},

          Child2 = spawn(?MODULE, fib_actor, []),
          Child2 ! {request, N - 2, Self},

          ?expects("Response.Response"),
          Term1 = receive
            {response, X1} ->
              X1
          end,
          ?expects("Response"),
          Term2 = receive
            {response, X2} ->
              X2
          end,
          Parent ! {response, Term1 + Term2}
      end
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  Self = self(),
  First_actor = spawn(?MODULE, fib_actor, []),
  First_actor ! {request, 16, Self},

  ?expects("Response"),
  receive
    {response, X} ->
      format("Result: ~p.~n", [X])
  end.


%% ./src/paterl src/examples/erlang/savina/fib_pairs.erl -v all -I include