%%%
%%% %CopyrightBegin%
%%%
%%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% %CopyrightEnd%
%%%
-module(paterl_lib).
-moduledoc "Manages internal errors and function results.".
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("paterl_lib.hrl").

%%% Public API.
-export([reset_result/1, set_status/1, reset_status/1]).
-export([push_error/3, push_warning/3]).
-export([return/1]).

%%% Public types.
-export_type([analysis/0]).
-export_type([reason/0, detail/0]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Encapsulates internal analysis structure. Intended for analysis modules.".
-type analysis() :: #analysis{}.

-doc "Error or warning detail.".
-type detail() :: term().

-doc """
Error or warning reason.

Replicates the standard error structure in use by all Erlang I/O modules in the
`stdlib` application.
""".
-type reason() ::
{Anno :: erl_anno:location() | none, Mod :: module(), Detail :: detail()} |
{Mod :: module(), Detail :: detail()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Prepends the new error to the specified [`analysis()`](`t:analysis/0`).

### Returns
- updated [`analysis()`](`t:analysis/0`) with the new error appended
""".
-spec push_error(Mod, Detail, Analysis) -> Analysis0
  when
  Mod :: module(),
  Detail :: detail(),
  Analysis :: analysis(),
  Analysis0 :: analysis().
push_error(Mod, {Code, Node}, Analysis = #analysis{errors = Reasons}) ->
  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
  Analysis#analysis{errors = [Reason | Reasons], status = error}.

-doc """
Prepends the new warning to the specified [`analysis()`](`t:analysis/0`).

### Returns
- updated [`analysis()`](`t:analysis/0`) with the new warning appended
""".
-spec push_warning(Mod, Detail, Analysis) -> Analysis0
  when
  Mod :: module(),
  Detail :: detail(),
  Analysis :: analysis(),
  Analysis0 :: analysis().
push_warning(Mod, {Code, Node}, Analysis = #analysis{warnings = Reasons}) ->
  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
  Analysis#analysis{warnings = [Reason | Reasons]}.

-doc "Resets the [`analysis()`](`t:analysis/0`) status to `ok`.".
reset_status(Analysis = #analysis{}) ->
  Analysis#analysis{status = ok}.

-doc """
Sets the [`analysis()`](`t:analysis/0`) status.

### Returns
- [`analysis()`](`t:analysis/0`) with status `ok` in case of no errors
- [`analysis()`](`t:analysis/0`) with status `error` otherwise
""".
set_status(Analysis = #analysis{errors = []}) ->
  Analysis#analysis{status = ok};
set_status(Analysis = #analysis{}) ->
  Analysis#analysis{status = error}.

-doc "Resets the [`analysis()`](`t:analysis/0`) result to `undefined`.".
reset_result(Analysis = #analysis{}) ->
  Analysis#analysis{result = undefined}.


%%DO it with analysis and pattern matching on the analysis record which is not opaque. It is the
%%cleanest and requires the least amount of code. Use the status flag when returning results
%%to outside functions.

-doc """
Standardizes function return values following the Erlang I/O module convention.

### Return
- `{ok, Warnings}` is a successful result with possible warnings
- `{ok, Value, Warnings}` is a successful result with `Value` with posible
  warnings
- `{error, Errors, Warnings}` is an error result with errors and possible
  warnings
""".
-spec return(Analysis :: analysis()) -> Success | Error
  when
  Success :: {ok, Warnings} | {ok, Result, Warnings},
  Error :: {error, Errors, Warnings},
  Result :: term(),
  Errors :: paterl_errors:errors(),
  Warnings :: paterl_errors:warnings().
%%return(#analysis{status = ok, result = undefined}) ->
%%  ok;
return(#analysis{status = ok, result = undefined, warnings = Warnings}) ->
  {ok, Warnings};
return(#analysis{status = ok, result = Result, warnings = []}) ->
  {ok, Result, []};
return(#analysis{status = ok, file = File, result = Result, warnings = Warnings}) ->
  {ok, Result, [{File, lists:reverse(Warnings)}]};
return(#analysis{status = error, file = File, errors = Errors, warnings = []}) ->
  {error, [{File, lists:reverse(Errors)}], []};
return(#analysis{status = error, file = File, errors = Errors, warnings = Warnings}) ->
  {error, [{File, lists:reverse(Errors)}], [{File, lists:reverse(Warnings)}]}.