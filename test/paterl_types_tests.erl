%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2024 15:53
%%%-------------------------------------------------------------------
-module(paterl_types_tests).
-author("duncan").


%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").

%% API
-export([]).

%%simple_test_() ->
%%  ?_test(?assert(1 + 3 =:= 2)).
%%  {"Duncan", ?_assert(4 == 3)}.

%%simple2_test_() -> {"", ?_test(ok)}.

simple3_test_() ->
  {foreach,
    fun() ->
      ?debugHere
    end,
    fun(_) ->
      ok
    end,
    [
      fun(X) ->
        {"One",
          ?_test(
            begin
              ?debugMsg("Test"),
              io:format(user, "~s~n~n", ["Hello"])
%%              ?assertEqual(1, 2)
            end
          )
        }
      end
    ]
  }.

simple4_test_() ->
  {foreachx,
    fun startup/1,
    fun cleanup/2,
    [
      {"Test case 1 value",
        fun(X, R) ->
          {"Test case 1",
            ?_test(
              begin
                ?debugMsg("In main function " ++ X ++ R),
                ?assert(4 == 3)
              end)}
        end
      }
    ]
  }.

startup(Erl) ->
  Forms = ?Q([
    "-module(test).",
    "-type msg_1() :: {msg_1, integer()}. ",
    "-type mb_1() :: {msg_2, integer()} | msg_1().",
    "-type mb_2() :: {msg_3, mb_1()}.",
    "-new({mb_1, [fun_1/0]}).",
    "-use({mb_1, [fun_2/0]}).",
    "-spec fun_1() -> ok.",
    "fun_1() -> ok.",
    "fun_2() -> ok."
    ]),

  ?assertMatch({ok, _}, erl_lint:module(Forms)),
  case erl_lint:module(Forms) of
    {ok, Warnings0} ->
      errors:show_warnings(Warnings0);
    {error, Errors, Warnings} ->
      errors:show_warnings(Warnings),
      errors:show_errors(Errors),
      error
  end,


  io:format(user, "~p~n", [Forms]),
  ?debugHere,
  ?debugMsg("In Startup"),
  ?debugMsg("From test case: " ++ Erl),
  Erl.

cleanup(Erl, Ret) ->
  ?debugMsg("In Cleanup"),
  ?debugMsg("From test case: " ++ Erl),
  ?debugMsg("From startup: " ++ Ret).


