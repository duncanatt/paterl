%%
%% %CopyrightBegin%
%%
%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(io_util).
-moduledoc "Pretty printing and text-based formatting.".
-author("duncan").

%%% Public API.
-export([log/1, log/2, info/1, info/2, warning/1, warning/2, error/1, error/2]).
-export([banner/1, banner/2, table/2]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Information color.
-define(COL_INFO, green).

%% Warning color.
-define(COL_WARN, yellow).

%% Error color.
-define(COL_ERROR, red).

%% Row character.
-define(ROW_CHAR, $-).

%% Column character.
-define(COL_CHAR, $|).

%% Minimum screen width.
-define(MIN_WIDTH, 80).

%% Key header row title.
-define(KEY_TITLE, "Key").

%% Value header row title.
-define(VALUE_TITLE, "Value").


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Formats a log message with the specified `Fmt` and `Args`.

### Returns
- `ok` on success
""".
-spec log(Fmt, Args) -> ok
  when
  Fmt :: io:format(),
  Args :: [term()].
log(Fmt, Args) ->
  io:fwrite(user, "~s~n", [io_lib:format(Fmt, Args)]).

-doc """
Prints a log message with the specified `Text`.

### Returns
- `ok` on success
""".
-spec log(Text :: iolist()) -> ok.
log(Text) ->
  log(Text, []).

-doc """
Formats an information message with the specified `Fmt` and `Args`.

### Returns
- `ok` on success
""".
-spec info(Fmt, Args) -> ok
  when
  Fmt :: io:format(),
  Args :: [term()].
info(Fmt, Args) ->
  log(color:?COL_INFO(Fmt), Args).

-doc """
Prints an information message with the specified `Text`.

### Returns
- `ok` on success
""".
-spec info(Text :: iolist()) -> ok.
info(Text) ->
  log(color:?COL_INFO(Text)).

-doc """
Formats a warning message with the specified `Fmt` and `Args`.

### Returns
- `ok` on success
""".
-spec warning(Fmt, Args) -> ok
  when
  Fmt :: io:format(),
  Args :: [term()].
warning(Fmt, Args) ->
  log(color:?COL_WARN(Fmt), Args).

-doc """
Prints a warning message with the specified `Text`.

### Returns
- `ok` on success
""".
-spec warning(Text :: iolist()) -> ok.
warning(Text) ->
  log(color:?COL_WARN(Text)).

-doc """
Formats an error message with the specified `Fmt` and `Args`.

### Returns
- `ok` on success
""".
-spec error(Fmt, Args) -> ok
  when
  Fmt :: io:format(),
  Args :: [term()].
error(Fmt, Args) ->
  log(color:?COL_ERROR(Fmt), Args).

-doc """
Prints an error message with the specified `Text`.

### Returns
- `ok` on success
""".
-spec error(Text :: iolist()) -> ok.
error(Text) ->
  log(color:?COL_ERROR(Text)).

-doc """
Formats `Fmt` with the specified `Args` as a banner.

### Returns
- `ok` on success
""".
-spec banner(Fmt, Args) -> ok
  when
  Fmt :: io:format(),
  Args :: [term()].
banner(Fmt, Args) ->
  Text = flatten(Fmt, Args),
  Width = max(length(Text) + 4, ?MIN_WIDTH),
  Spacing = Width - 4,
  Line = lists:duplicate(Width, ?ROW_CHAR),
  io:fwrite(user, "~s~n~s ~-*s ~s~n~s~n", [
    Line, <<?COL_CHAR>>, Spacing, Text, <<?COL_CHAR>>, Line
  ]).

-doc """
Prints `Text` as a banner.

### Returns
- `ok` on success
""".
-spec banner(Text :: iolist()) -> ok.
banner(Text) ->
  banner(Text, []).

-doc """
Prints `Map` as a table with the specified `Title`.

### Returns
- `ok` on success
""".
-spec table(Title :: iolist(), Map :: map()) -> ok.
table(Title, Map) when is_map(Map) ->

  KeysValues = lists:map(
    fun({Key, Value}) ->
      {flatten(Key), flatten(Value)}
    end,
    maps:to_list(Map)
  ),

  {KeyWidth, ValueWidth} = lists:foldl(
    fun({Key, Value}, {KeyWidth, ValueWidth}) ->
      {max(length(Key), KeyWidth), max(length(Value), ValueWidth)}
    end,
    {length(?KEY_TITLE), length(?VALUE_TITLE)}, KeysValues),

  % Print title and header rows.
  log(Title),
  separator(KeyWidth, ValueWidth),
  row(?KEY_TITLE, ?VALUE_TITLE, KeyWidth, ValueWidth),
  separator(KeyWidth, ValueWidth),

  lists:foreach(
    fun({Key, Value}) -> row(Key, Value, KeyWidth, ValueWidth) end,
    KeysValues
  ),

  separator(KeyWidth, ValueWidth).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc "Returns a flat stringified formatted string with the speficied `Args`.".
-spec flatten(Fmt, Args) -> string()
  when
  Fmt :: io:format(),
  Args :: [term()].
flatten(Fmt, Args) ->
  lists:flatten(io_lib:format(Fmt, Args)).

-doc "Returns a flat stringified representation of `Term`.".
-spec flatten(Term :: term()) -> string().
flatten(Term) ->
  lists:flatten(io_lib:format("~w", [Term])).

-doc "Prints a separator line.".
-spec separator(KeyWidth, ValueWidth) -> ok
  when
  KeyWidth :: non_neg_integer(),
  ValueWidth :: non_neg_integer().
separator(KeyWidth, ValueWidth) ->
  io:fwrite(user, "~s~n", [lists:duplicate(KeyWidth + ValueWidth + 7, ?ROW_CHAR)]).

-doc "Prints a row with formatted columns.".
-spec row(Key, Value, KeyWidth, ValueWidth) -> ok
  when
  Key :: term(),
  Value :: term(),
  KeyWidth :: non_neg_integer(),
  ValueWidth :: non_neg_integer().
row(Key, Value, KeyWidth, ValueWidth) ->
  io:fwrite(user, "~s ~-*s ~s ~-*s ~s~n", [<<?COL_CHAR>>, KeyWidth, Key, <<?COL_CHAR>>, ValueWidth, Value, <<?COL_CHAR>>]).