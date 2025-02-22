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

%% Encapsulates internal analysis structure.
-record(analysis, {
  status = ok,
  file = "nofile" :: string(),
  result = undefined :: term(),
  errors = [] :: [paterl_lib:reason()],
  warnings = [] :: [paterl_lib:reason()]
}).

%% Type information used to annotate forms.
-record(type_info, {
  type_defs = #{} :: paterl_types:type_defs(), % Global type def names to AST.
  spec_defs = #{} :: paterl_types:spec_defs(), % Function signatures to AST.
  mb_funs = #{} :: paterl_types:mb_funs(),  % Mailbox names to mailbox modality and function signatures.
  mb_defs = [] :: paterl_types:mb_defs()
}).

%% Prepends the new error code with the AST node to the specified analysis.
-define(
pushError(Code, Node, Analysis),
  paterl_lib:push_error(?MODULE, {Code, Node}, Analysis)
).

%% Prepends the new warning code with the AST node to the specified analysis.
-define(
pushWarning(Code, Node, Analysis),
  paterl_lib:push_warning(?MODULE, {Code, Node}, Analysis)
).