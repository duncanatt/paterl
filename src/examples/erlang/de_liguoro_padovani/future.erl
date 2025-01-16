%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% A future that supports one write and multiple reads.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(future).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([future/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Future.
-type put() :: {put, integer()}.
-type get() :: {get, user_mb()}.

%% User.
-type reply() :: {reply, integer()}.

%%% Interfaces.

%% Future.
-type future_mb() :: pid() | put() | get().

%% User.
-type user_mb() :: pid() | reply().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Future.
-new({future_mb, [future/0]}).
-use({future_mb, [resolved_future/1]}).

%% User.
-new({user_mb, [user/1]}).

%% Main.
-new({main_mb, [main/0]}).

%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Initializes the future.
-spec future() -> no_return().
future() ->
  ?expects("Put . *Get"),
  receive {put, Value} ->
    resolved_future(Value)
  end.

%% @doc Resolved future servicing requests.
-spec resolved_future(integer()) -> no_return().
resolved_future(Value) ->
  ?expects("*Get"),
  receive
    {get, UserPid} ->
      UserPid ! {reply, Value},
      resolved_future(Value)
  end.

%% @doc User requesting the value held in the future.
-spec user(future_mb()) -> integer().
user(FuturePid) ->
  Self = self(),
  FuturePid ! {get, Self},

  ?expects("Reply"),
  receive
    {reply, Value} ->
      Value
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
%%  ?mb_new(future_mb),
  FuturePid = spawn(?MODULE, future, []),
  FuturePid ! {put, 5},

  Get1 = user(FuturePid),
  format("A: ~p~n", [Get1]),

  Get2 = user(FuturePid),
  format("B: ~p~n", [Get2]).


%% ./src/paterl src/examples/erlang/de_liguoro_padovani/future.erl -v all -I include