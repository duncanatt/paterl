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
-module(paterl_errors).
-moduledoc "Pretty prints errors and warnings on the standard error.".
-author("duncan").

%%% Public API.
-export([show_errors/1, show_warnings/1, show_error/1, show_warning/1]).

%%% Public types.
-export_type([source/0]).
-export_type([errors/0, warnings/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Error and warning cues.
-define(S_ERROR, 'Error').
-define(S_WARNING, 'Warning').


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Error severity.".
-type severity() :: ?S_ERROR | ?S_WARNING.


% This is how the internal structure of the EPP works. When it has no errors, it returns [],
% otherwise it returns [{file,list of errors}].
% I modelled the error display functions on the strange way EPP uses them to
% make everything consistent.
%% The source file containing errors.
%%-type source() ::
%%  [{Filename :: file:filename(), errors() | warnings()}] |
%%  {Filename :: file:filename(), errors() | warnings()}.

-doc "Error or warning source file and reasons.".
-type source() ::
%%[{Filename :: file:filename(), paterl_lib:reasons()}] | % Matches with erl_lint errors and warnings.
{Filename :: file:filename(), Reasons :: [paterl_lib:reason()]}. % Matches with paterl errors and warnings.

-type errors() :: [source()].
-type warnings() :: [source()].


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc "Pretty prints the list of source file errors on the standard error.".
-spec show_errors(Sources :: [source()]) -> ok.
show_errors([]) ->
  ok;
show_errors([Source = {_Filename, Reasons} | Errors]) when is_list(Reasons) ->
  print_reasons(?S_ERROR, Source),
  show_errors(Errors).

-doc "Pretty prints the list of source file warnings on the standard error.".
-spec show_warnings(Sources :: [source()]) -> ok.
show_warnings([]) ->
  ok;
show_warnings([Source = {_Filename, Reasons} | Warnings]) when is_list(Reasons) ->
  print_reasons(?S_WARNING, Source),
  show_warnings(Warnings).

-doc "Pretty prints the error reason on the standard error.".
-spec show_error(paterl_lib:reason()) -> ok.
show_error(Reason) ->
  print_reason(?S_ERROR, Reason).

%%-spec show_warning({file:name_all(), reason()} | reason()) -> ok.
-doc "Pretty prints the warning reason on the standard error.".
-spec show_warning(paterl_lib:reason()) -> ok.
show_warning(Reason) ->
  print_reason(?S_WARNING, Reason).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc "Pretty prints the specified list of reasons for file to the standard error.".
-spec print_reasons(severity(), source()) -> ok.
print_reasons(Severity, {Filename, Reasons}) ->
  Base = lists:last(filename:split(Filename)),
  [print_reason(Severity, Base, Reason) || Reason <- Reasons],
  ok.

-doc "Pretty prints the specified reason for file to the standard error.".
-spec print_reason(severity(), file:name_all(), paterl_lib:reason()) -> ok.
print_reason(Severity, Base, {ANNO, Mod, Detail}) ->
  % Error or warning reason with base file name. Line number is available.
  io:format(
    standard_error, "1. ~ts:~w: ~ts: ~ts~n", [Base, ANNO, Severity,
      Mod:format_error(Detail) % Delegate formatting to module showing error.
    ]).

% No filename and used to print lone errors.
-doc "Pretty prints the specified reason to the standard error.".
-spec print_reason(severity(), paterl_lib:reason()) -> ok.
print_reason(Severity, {ANNO, Mod, Detail}) ->
  % Error or warning reason without base file name. Line number is available.
  io:format(
    standard_error, "2. ~w: ~ts: ~ts~n", [ANNO, Severity,
      Mod:format_error(Detail) % Delegate formatting to module showing error.
    ]);
print_reason(Severity, {Mod, Detail}) ->
  % Error or warning reason without base file name. Line number unavailable.
  io:format(standard_error, "3. ~ts: ~ts~n", [Severity,
    Mod:format_error(Detail) % Delegate formatting to module showing error.
  ]).