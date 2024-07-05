%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 6月 2024 22:59
%%%-------------------------------------------------------------------
-module(run).
-export([main/0,debug/0,compile_debug/0]).

main() ->
  {ok, _} = compile:file("src/kai/savina/count.erl", [{i, "include"}]),
  banking:main().

debug() ->
  % 编译文件
  {ok, _} = compile:file("src/kai/other/products.erl", [{i, "include"}, debug_info]),
  % 启动调试器
  debugger:start().

compile_debug() ->
  % 编译文件
  {ok, _} = compile:file("src/paterl.erl", [{i, "include"}, debug_info]),
  % 启动调试器
  debugger:start().

%% paterl:compile("src/kai/de_liguoro_padovani/future.erl",[all,include]).