%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2024 15:53
%%%-------------------------------------------------------------------
-module(test_samples).
-author("duncan").


%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

%% API
-export([]).
-compile(export_all).

simple_1_test_x() ->
  {"Test case name", ?_test(begin ?debugMsg("Test case with name") end)}.

simple_2_test_x() ->
  {"Test case name", fun() -> ?debugMsg("Test case with name") end}.

simple_3_test_x() ->
  ?_test(begin ?debugMsg("Test case without name") end).

foreach_template_test_x() ->
  {foreach,
    _Startup =
      fun() ->
        %?debugHere
        ?debugMsg("[STARTUP]"),
        "Return from Startup"
      end,
    _Cleanup =
      fun(ReturnFromStartup) ->
        ?debugMsg("[CLEANUP] Return from startup: " ++ ReturnFromStartup)
      end,
    [
      fun(ReturnFromStartup) ->
        {"Test case name",
          ?_test(
            begin
              ?debugMsg("[TEST] Return from startup: " ++ ReturnFromStartup),
              ?assertEqual(1, 1)
            end
          )
        }
      end
    ]
  }.

foreachx_template_test_x() ->
  {foreachx,
    _Startup =
      fun(InputFromTestCase) ->
        %?debugHere,
        ?debugMsg("[STARTUP] Input from test case: " ++ InputFromTestCase),
        "Return from Startup"
      end,
    _Cleanup =
      fun(InputFromTestCase, ReturnFromStartup) ->
        ?debugMsg("[CLEANUP] Input from test case: " ++ InputFromTestCase),
        ?debugMsg("[CLEANUP] Return from startup: " ++ ReturnFromStartup)
      end,
    [
      {"Test case input passed to Startup and Test Case",
        fun(InputFromTestCase, ReturnFromStartup) ->
          {"Test case name",
            ?_test(
              begin
                ?debugMsg("[TEST] Input from test case: " ++ InputFromTestCase),
                ?debugMsg("[TEST] Return from startup: " ++ ReturnFromStartup),
                ?_assert(4 == 3)
              end)}
        end
      }
    ]
  }.

foreachx_template_2_test_() ->
  {foreachx,
    _Startup =
      fun(InputFromTestCase) ->
        %?debugHere,
        ?debugMsg("[STARTUP] Input from test case: " ++ InputFromTestCase),
        "Return from Startup"
      end,
    _Cleanup =
      fun(InputFromTestCase, ReturnFromStartup) ->
        ?debugMsg("[CLEANUP] Input from test case: " ++ InputFromTestCase),
        ?debugMsg("[CLEANUP] Return from startup: " ++ ReturnFromStartup)
      end,

    [Test() || Test <- [
      fun test_case/0
    ]]
  }.

test_case() ->
  {"Test case input passed to Startup and Test Case",


    fun(InputFromTestCase, ReturnFromStartup) ->
      {"Name",
        fun() ->
          ?debugMsg("[TEST] Input from test case: " ++ InputFromTestCase),
          ?debugMsg("[TEST] Return from startup: " ++ ReturnFromStartup),
          ?_assert(4 == 3)
        end}
    end
  }.





