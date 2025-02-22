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
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%%% Encapsulates internal error structure.
%%-record(analysis, {
%%  file = "nofile" :: string(),
%%  result = undefined :: term(),
%%  errors = [] :: [reason()],
%%  warnings = [] :: [reason()]
%%}).


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


%%-type error_info() :: {erl_anno:location()|'none', module(), error_description()}.


%%Warnings :: [{SourceFile,[ErrorInfo]}],
%%Errors :: [{SourceFile,[ErrorInfo]}],
%%SourceFile :: file:filename(),
%%ErrorInfo :: error_info()).

%%-doc """
%%Function result.
%%
%%- `{error, Errors, Warnings}` is an error result with errors and potential
%%  warnings.
%%- `{ok, Value, Warnings}` is a successful result with `Value` with potential
%%  warnings.
%%- `{ok, Warnings}` is a successful result with potential warnings.
%%""".
%%-type result() ::
%%{error, Errors :: [reason()], Warnings :: [reason()]} |
%%error() |
%%{ok, Value :: term(), Warnings :: [reason()]} |
%%{ok, Warnings :: [reason()]}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%%-doc "Creates a new [`error()`](`t:error/0`).".
%%-spec new_error() -> error().
%%new_error() ->
%%  #error{}.


%%new_error(File) ->
%%  #error{file = File}.
%%
%%new_error(File, Error = #error{}) ->
%%  Error#error{file = File}.

%%-type new_error() -> {error, errors(), warnings()}.
%%new_error(#error{file = File, errors = Errors, warnings = Warnings}) ->
%%  {error}.

%%flat_warnings(File, )

%%-doc "Returns the list of error reasons.".
%%-spec get_errors(Error :: error()) -> errors().
%%get_errors(#error{errors = Reasons}) ->
%%  Reasons.
%%
%%-doc "Returns the list of warning reasons.".
%%-spec get_warnings(Error :: error()) -> warnings().
%%get_warnings(#error{warnings = Reasons}) ->
%%  Reasons.

-doc "Prepends the new error to the specified [`error()`](`t:error/0`).".
-spec push_error(Mod :: module(), Detail :: detail(), Analysis) -> Analysis
  when
  Analysis :: analysis().
push_error(Mod, {Code, Node}, Analysis = #analysis{errors = Reasons}) ->
  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
  Analysis#analysis{errors = [Reason | Reasons], status = error}.
%%push_error(Mod, {Code, Node}, Analysis = #error{errors = Reasons}) ->
%%  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
%%  Analysis#error{errors = [Reason | Reasons]}. % TODO: Remove after the very final refactoring.



-doc "Prepends the new warning to the specified [`error()`](`t:error/0`).".
-spec push_warning(Mod :: module(), Detail :: detail(), Analysis) -> Analysis
  when
  Analysis :: analysis().
push_warning(Mod, {Code, Node}, Analysis = #analysis{warnings = Reasons}) ->
  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
  Analysis#analysis{warnings = [Reason | Reasons]}.

reset_status(Analysis = #analysis{}) ->
  Analysis#analysis{status = ok}.

set_status(Analysis = #analysis{errors = []}) ->
  Analysis#analysis{status = ok};
set_status(Analysis = #analysis{}) ->
  Analysis#analysis{status = error}.

reset_result(Analysis = #analysis{}) ->
  Analysis#analysis{result = undefined}.

%%-doc """
%%Standardizes function return values following the Erlang internal conventions.
%%
%%**Returns**
%%- `{error, Errors, Warnings}` is an error result with errors and potential
%%  warnings.
%%- `{ok, Value, Warnings}` is a successful result with `Value` with potential
%%  warnings.
%%""".
%%-spec return(Value, Error :: error()) -> {ok, Value, Warnings} | error()
%%  when
%%  Value :: term(),
%%  Warnings :: [reason()].
%%return(Value, #error{errors = [], warnings = Warnings}) ->
%%  % Result with warnings.
%%  {ok, Value, Warnings};
%%return(_, #error{errors = Errors, warnings = Warnings}) ->
%%  % Unusable result with errors and warnings. Follows erl_lint module/1 return.
%%  {error, Errors, Warnings}.

%%-doc """
%%Standardizes function return values following the Erlang internal conventions.
%%
%%**Returns**
%%- `{error, Errors, Warnings}` is an error result with errors and potential
%%  warnings.
%%- `{ok, Value, Warnings}` is a successful result with `Value` with potential
%%  warnings.
%%- `{ok, Warnings}` is a successful result with potential warnings.
%%""".
%%-spec return(Error :: error() | Value) -> {ok, Value, Warnings} | error()
%%  when
%%  Value :: term(),
%%  Warnings :: [reason()].
%%return(#error{errors = [], warnings = Warnings}) ->
%%  % Warnings. Follows erl_lint module/1 return.
%%  {ok, Warnings};
%%return(Error = #error{errors = Errors, warnings = Warnings}) ->
%%  % Errors and warnings. Follows erl_lint module/1 return.
%%%%  {error, Errors, Warnings};
%%  Error;
%%return(Value) -> % TODO: Maybe remove this to have a standard interface and handle the cases where a value is involved by forcing the user to supply and empty error().
%%  % Result without warnings.
%%  {ok, Value, []}.


%% For internal use because there is no file information.
%%-spec return(Analysis :: analysis()) ->
%%  {ok, Result} | {ok, Warnings} | {ok, Result, Warnings} | {error, Warnings, Errors}
%%  when
%%  Result :: term(),
%%  Warnings :: [paterl_lib:reason()],
%%  Errors :: [paterl_lib:reason()].
%%return(#analysis{result = undefined, errors = [], warnings = Warnings}) ->
%%  % No result, no errors, but possible warnings.
%%  {ok, Warnings};
%%return(#analysis{result = Result, errors = [], warnings = []}) ->
%%  % Result with no errors and no warnings.
%%  {ok, Result};
%%return(#analysis{result = Result, errors = [], warnings = Warnings}) ->
%%  % Result with no errors but possible warnings.
%%  {ok, Result, Warnings};
%%return(#analysis{errors = Errors, warnings = Warnings}) ->
%%  {error, Warnings, Errors}.


%%returnx(#error{errors = [], warnings = Warnings}) ->
% No result, no errors and no warnings.

%%  WE need to reinstate the error model so that we can pattern match effectively and also bubble up messages
%%  Consider adding an ok record!
%%  Actually, we do not need to match against the error record, we do it just for fun.
%%  Error should be an opaque
%%  {ok, []}.

%% For internal use.
%%return(#error{errors = [], warnings = Warnings}) ->
%%  % No result, no errors, but possible warnings.
%%  {ok, Warnings};
%%return(Error = #error{}) ->
%%  % Error.
%%  Error;
%%return(Result) ->
%%  % Result with no errors and no warnings.
%%  {ok, Result}.

%% Till now, we do not need this case.
%%return(Result, #error{errors = [], warnings = Warnings}) ->
%%  % Result with no errors but possible warnings.
%%  {ok, Result, Warnings}.


%%DO it with analysis and pattern matching on the analysis record which is not opaque. It is the
%%cleanest and requires the least amount of code. Use the status flag when returning results
%%to outside functions.


%% For external use because there is file information.
%%-spec return_status(File :: string(), Analysis :: analysis()) ->
%%  {ok, Result} | {ok, Warnings} | {ok, Result, Warnings} | {error, Warnings, Errors}
%%  when
%%  Result :: term(),
%%  Warnings :: paterl_errors:warnings(),
%%  Errors :: paterl_errors:errors().
%%return_status(File, #analysis{result = undefined, errors = [], warnings = Warnings}) ->
%%  % No result, no errors, but possible warnings.
%%%%  {ok, [{File, Reason} || Reason <- Analysis#analysis.warnings]};
%%  {ok, [{File, Warnings}]};
%%return_status(_, #analysis{result = Result, errors = [], warnings = []}) ->
%%  % Result with no errors and no warnings.
%%  {ok, Result};
%%return_status(File, #analysis{result = Result, warnings = Warnings, errors = []}) ->
%%  % Result with no errors but possible warnings.
%%  {ok, Result, [{File, Warnings}]};
%%return_status(File, #analysis{warnings = Warnings, errors = Errors}) ->
%%  {error, [{File, Warnings}], [{File, Errors}]}.


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

%%TEST Different errors in the id_server_demo and make a list of the possible tests outside src.
