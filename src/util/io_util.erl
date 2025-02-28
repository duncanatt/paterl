%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Feb 2025 14:59
%%%-------------------------------------------------------------------
-module(io_util).

-define(COL_INFO, green).

-define(COL_WARN, yellow).

-define(COL_ERROR, red).

-define(ROW_CHAR, $-).

-define(COL_CHAR, $|).

-define(MIN_WIDTH, 80).

-define(KEY_TITLE, "Key").

-define(VALUE_TITLE, "Value").

-compile(export_all).

log(Fmt, Args) ->
  io:fwrite(user, "~s~n", [io_lib:format(Fmt, Args)]).

log(Text) ->
  log(Text, []).

info(Fmt, Args) ->
  log(color:?COL_INFO(Fmt), Args).

info(Text) ->
  log(color:?COL_INFO(Text)).

warning(Fmt, Args) ->
  log(color:?COL_WARN(Fmt), Args).

warning(Text) ->
  log(color:?COL_WARN(Text)).

error(Fmt, Args) ->
  log(color:?COL_ERROR(Fmt), Args).

error(Text) ->
  log(color:?COL_ERROR(Text)).


banner(Fmt, Args) ->
  Text = flatten_format(Fmt, Args),
  Width = max(length(Text) + 4, ?MIN_WIDTH),
  Spacing = Width - 4,
  Line = lists:duplicate(Width, ?ROW_CHAR),
  io:fwrite(user, "~s~n~s ~-*s ~s~n~s~n", [
    Line, <<?COL_CHAR>>, Spacing, Text, <<?COL_CHAR>>, Line
  ]).

banner(Text) ->
  banner(Text, []).


%% TABLE.

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

  log(Title),
  print_separator(KeyWidth, ValueWidth),
  print_row(?KEY_TITLE, ?VALUE_TITLE, KeyWidth, ValueWidth),
  print_separator(KeyWidth, ValueWidth),

  lists:foreach(
    fun({Key, Value}) -> print_row(Key, Value, KeyWidth, ValueWidth) end,
    KeysValues
  ),

  print_separator(KeyWidth, ValueWidth).

% Flatten the formatted term into a single-line string
flatten_format(Fmt, Args) ->
  lists:flatten(io_lib:format(Fmt, Args)).
flatten_format(Term) ->
  lists:flatten(io_lib:format("~w", [Term])).

% Prints a separator line
print_separator(KeyWidth, ValueWidth) ->
  io:fwrite(user, "~s~n", [lists:duplicate(KeyWidth + ValueWidth + 7, ?ROW_CHAR)]).

% Prints a row with formatted columns
print_row(Key, Value, KeyWidth, ValueWidth) ->
  io:fwrite(user, "~s ~-*s ~s ~-*s ~s~n", [<<?COL_CHAR>>, KeyWidth, Key, <<?COL_CHAR>>, ValueWidth, Value, <<?COL_CHAR>>]).

