%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 15:15
%%%-------------------------------------------------------------------
-module(fib_pairs).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/1]).

-export([fib_actor/0]).

%% Mailbox interface-function associations.
-new({fib_actor_mb, [main/1]}).
-use({fib_actor_mb, [fib_actor/0]}).

%% FibActor's message types
-type request() :: {request, integer(), fib_actor_mb()}.
-type response() :: {response, integer()}.

%% Interface FibActor {
%%    Request(Int, FibActor!),
%%    Response(Int)
%% }
-type fib_actor_mb() :: pid() | request() | response().
-type main_mb() :: pid().

%% def fibActor(self: FibActor?): Unit {
%%   guard self : Request {
%%     receive Request(n, parent) from self ->
%%       if (n <= 2) {
%%         parent ! Response(1);
%%         free(self)
%%       } else {
%%         let childMB1 = new[FibActor] in
%%         spawn { fibActor(childMB1) };
%%         childMB1 ! Request(n - 1, self);
%%
%%         let childMB2 = new[FibActor] in
%%         spawn { fibActor(childMB2) };
%%         childMB2 ! Request(n - 2, self);
%%         let (x1, self) =
%%           guard self : Response {
%%             receive Response(x1) from self -> (x1, self)
%%           }
%%         in
%%         let (x2, self) =
%%           guard self : Response {
%%             receive Response(x2) from self -> (x2, self)
%%           }
%%         in
%%         free(self);
%%         parent ! Response(x1 + x2)
%%       }
%%   }
%% }
-spec fib_actor() -> no_return().
fib_actor() ->
  ?mb_assert_regex("Request"),
  receive
    {request, N, Parent} ->
      if N =< 2 ->
        Parent ! {response, 1};
        true ->
          Self = self(),
          ?mb_new(fib_actor_mb),
          Child1 = spawn(?MODULE, fib_actor, []),
          Child1 ! {request, N - 1, Self},

          ?mb_new(fib_actor_mb),
          Child2 = spawn(?MODULE, fib_actor, []),
          Child2 ! {request, N - 2, Self},

          ?mb_assert_regex("Response"),
          Term1 = receive
            {response, X1} ->
              X1
          end,
          ?mb_assert_regex("Response"),
          Term2 = receive
            {response, X2} ->
              X2
          end,
          Parent ! {response, Term1 + Term2}
      end
  end.

%% def mainFibActor(n: Int): Int {
%%   let root = new[FibActor] in
%%   let firstActor = new[FibActor] in
%%   spawn { fibActor(firstActor) };
%%   firstActor ! Request(n, root);
%%   guard root : Response {
%%     receive Response(x) from root ->
%%       free(root);
%%       x
%%   }
%% }
-spec main(integer()) -> integer().
main(N) ->
  Self = self(),
  ?mb_new(fib_actor_mb),
  First_actor = spawn(?MODULE, fib_actor, []),
  First_actor ! {request, N, Self},

  ?mb_assert_regex("Response"),
  receive
    {response, X} ->
      X
  end.

