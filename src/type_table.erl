%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2023 15:01
%%%-------------------------------------------------------------------
-module(type_table).
-author("duncan").

%%% API
-export([]).

-spec get(erl_syntax:syntaxTree()) -> ok.
get(Forms) ->
  {a, b}.

-spec format_errors(any()) -> ok.
format_errors(_) ->
  ok.