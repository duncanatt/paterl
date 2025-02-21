%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Feb 2025 14:54
%%%-------------------------------------------------------------------
-module(table_fmt).
-export([print_table/2]).

-define(KEY_TITLE, "Key").

-define(VALUE_TITLE, "Value").

% Prints a table from a map
print_table(Title, Map) when is_map(Map) ->

  KeysValues = lists:map(
    fun({Key, Value}) ->
      {flatten_format(Key), flatten_format(Value)}
    end,
    maps:to_list(Map)
  ),

  {KeyWidth, ValueWidth} = lists:foldl(
    fun({Key, Value}, {KeyWidth, ValueWidth}) ->
      {max(length(Key), KeyWidth), max(length(Value), ValueWidth)}
    end,
    {length(?KEY_TITLE), length(?VALUE_TITLE)}, KeysValues),

  io:format("~n~s~n", [Title]),
  print_separator(KeyWidth, ValueWidth),
  print_row(?KEY_TITLE, ?VALUE_TITLE, KeyWidth, ValueWidth),
  print_separator(KeyWidth, ValueWidth),

  lists:foreach(
    fun({Key, Value}) -> print_row(Key, Value, KeyWidth, ValueWidth) end,
    KeysValues
  ),

  print_separator(KeyWidth, ValueWidth).

% Flatten the formatted term into a single-line string
flatten_format(Value) ->
  lists:flatten(io_lib:format("~w", [Value])).

% Prints a separator line
print_separator(KeyWidth, ValueWidth) ->
  io:format("~s~n", [string:copies("-", KeyWidth + ValueWidth + 7)]).

% Prints a row with formatted columns
print_row(Key, Value, KeyWidth, ValueWidth) ->
  io:format("| ~-*s | ~-*s |~n", [KeyWidth, Key, ValueWidth, Value]).

