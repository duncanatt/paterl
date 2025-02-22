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
-module(paterl_tools).
-moduledoc "Syntax tools.".
-author("duncan").

%%% Public API.
-export([fresh_var/1]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Variable start index.
-define(VAR__ID_START, 0).

%% Process dictionary variable key.
-define(VAR__ID, '@var_id').


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Returns a fresh variable name.

### Returns
- fresh variable name
""".
-spec fresh_var(Name) -> Name0
  when
  Name :: paterl_syntax:name(),
  Name0 :: paterl_syntax:name().
fresh_var(Name) when is_atom(Name) ->
  list_to_atom(
    string:to_lower(atom_to_list(Name)) ++ integer_to_list(next_var_id(Name))
  ).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc """
Returns the next ID associated with the specified variable `Name`.

### Returns
- next ID
""".
-spec next_var_id(Name :: paterl_syntax:name()) -> integer().
next_var_id(Name) when is_atom(Name) ->
  Key = {?VAR__ID, Name},
  case get(Key) of
    undefined ->
      put(Key, ?VAR__ID_START + 1),
      ?VAR__ID_START;
    Id ->
      put(Key, Id + 1)
  end.