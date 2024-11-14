%%% ----------------------------------------------------------------------------
%%% Duncan Paul Attard
%%%
%%% Module description (becomes module heading).
%%%
%%%
%%% 
%%% Copyright (c) 2021, Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify it 
%%% under the terms of the GNU General Public License as published by the Free 
%%% Software Foundation, either version 3 of the License, or (at your option) 
%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT 
%%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
%%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
%%% more details.
%%%
%%% You should have received a copy of the GNU General Public License along with 
%%% this program. If not, see <https://www.gnu.org/licenses/>.
%%% ----------------------------------------------------------------------------
-module(log).
-moduledoc """
Macro-based logger that can be switched off on releases.
""".
-author("Duncan Paul Attard").

%%% Includes.
-include("log.hrl").

%%% Public API.
-export([log_to_file/1, write/4, write/5]).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Configures the logger to write to the specified file.

- `File` is the full filename where the logger output is directed.

**Returns**

- `true` to indicate that log file is configured, `false` otherwise.
""".
-spec log_to_file(File :: file:filename()) -> true.
log_to_file(File) ->
  {ok, Log} = file:open(File, [write]),
  erlang:group_leader(Log, self()).

-doc """
Writes the log statement.

- `LogLabel` indicates the severity of the log statement.
- `Module` is the name of the module issuing the log statement.
- `Line` is the line number of the log statement.
- `Fmt` is the format of the log statement string.

**Returns**

- `ok` to acknowledge success.
""".
-spec write(LogLabel, Module, Line, Fmt) -> ok
  when
  LogLabel :: string(),
  Module :: atom(),
  Line :: integer(),
  Fmt :: string().
write(LogLabel, Module, Line, Fmt) ->
  write(LogLabel, Module, Line, Fmt, []).

-doc """
Outputs the log statement with formatting parameters.

- `LogLabel` indicated the severity of the log statement.
- `Module` is the name of the module issuing the log statement.
- `Line` is the line number of the log statement.
- `Fmt` is the format of the log statement string.
- `Params` are the parameters to be formatted in the log statement string.

**Returns**

- `ok` to acknowledge success.
""".
-spec write(LogLabel, Module, Line, Fmt, Params) -> ok
  when
  LogLabel :: string(),
  Module :: atom(),
  Line :: integer(),
  Fmt :: string(),
  Params :: list().
write(LogLabel, Module, Line, Fmt, Params) ->
  case can_log(?log_level, LogLabel) of
    true ->
      io:fwrite(user, "[~s - ~p - ~p:~p] - ~s~n",
        [LogLabel, self(), Module, Line, io_lib:format(Fmt, Params)]);
    false -> ok
  end.


%%% ----------------------------------------------------------------------------
%%% Private helper functions.
%%% ----------------------------------------------------------------------------

-doc """
Determines whether a log statement for the specified severity level can be
output.

- `LogLevel` is the log level from 1 to 5 that indicates the severity of the log
statement, where 1 = TRACE, 2 = DEBUG, 3 = INFO, 4 = WARN, 5 = ERROR.
- `LogLabel` indicates the the severity of the log statement.

**Returns**

- `true' if the log statement can be output, `false' otherwise.
""".
-spec can_log(LogLevel :: integer(), LogLabel :: string()) -> boolean().
can_log(?trace_level, ?trace_str) ->
  true;
can_log(?trace_level, ?debug_str) ->
  true;
can_log(?trace_level, ?info_str) ->
  true;
can_log(?trace_level, ?warn_str) ->
  true;
can_log(?trace_level, ?error_str) ->
  true;

can_log(?debug_level, ?debug_str) ->
  true;
can_log(?debug_level, ?info_str) ->
  true;
can_log(?debug_level, ?warn_str) ->
  true;
can_log(?debug_level, ?error_str) ->
  true;

can_log(?info_level, ?info_str) ->
  true;
can_log(?info_level, ?warn_str) ->
  true;
can_log(?info_level, ?error_str) ->
  true;

can_log(?warn_level, ?warn_str) ->
  true;
can_log(?warn_level, ?error_str) ->
  true;

can_log(?error_level, ?error_str) ->
  true;
can_log(_, _) -> false.