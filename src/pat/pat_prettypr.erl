%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2024 10:29
%%%-------------------------------------------------------------------
-module(pat_prettypr).
-author("duncan").

%% API
-export([]).
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(SEP_NL, [$\n]).

-define(SEP_TAB, [$\t]).

%% TODO: Implement visitor that pretty prints the Pat syntax.

indent(Lines) ->
  Lines0 = string:split(lists:flatten(Lines), ?SEP_NL, all),
  io:format("Lines to indent: ~p~n~n~n", [Lines0]),
  indent(Lines0, 0).

indent([], _) ->
  [];

indent([Line = [$i, $n, $t, $e, $r, $f, $a, $c, $e | _] | Lines], _) ->
  io:format("Found interface { (indent ~p).~n", [0]),
  [[10, Line] | indent(Lines, 1)];

indent([Line = [$d, $e, $f | _] | Lines], _) ->
  io:format("Found def { (indent ~p).~n", [0]),
  [[10, Line] | indent(Lines, 1)];

indent([Line = [$} | _] | Lines], Level) ->
  io:format("Found closing } (indent ~p).~n", [Level - 1]),
  [tabs(Level - 1, Line) | indent(Lines, Level - 1)];

indent([Line = [$l, $e, $t | _] | Lines], Level) ->
  io:format("Found let (indent ~p).~n", [Level]),
  [tabs(Level, Line) | indent(Lines, Level + 1)];

indent([Line = [$i, $n | _] | Lines], Level) ->
  io:format("Found in (indent ~p).~n", [Level - 1]),
  [tabs(Level - 1, Line), indent(Lines, Level)];

indent([Line = [$i, $f | _] | Lines], Level) ->
  io:format("Found if (indent ~p).~n", [Level]),
  [tabs(Level, Line) | indent(Lines, Level + 1)];

indent([Line = [$e, $l, $s, $e | _] | Lines], Level) ->
  io:format("Found else (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$s, $p, $a, $w, $n | _] | Lines], Level) ->
  io:format("Found spawn (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$g, $u, $a, $r, $d | _] | Lines], Level) ->
  io:format("Found receive (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$r, $e, $c, $e, $i, $v, $e | _] | Lines], Level) ->
  io:format("Found receive (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$e, $m, $p, $t, $y | _] | Lines], Level) ->
  io:format("Found empty (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line | Lines], Level) ->
  io:format("Found other ~s (indent ~p).~n", [Line, Level]),
  [tabs(Level, Line) | indent(Lines, Level)].

tabs(N, Line) ->
  lists:flatten([?SEP_NL, lists:duplicate(N, ?SEP_TAB), Line]).