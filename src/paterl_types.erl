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
-module(paterl_types).
-moduledoc "Erlang mailbox and type annotation extraction.".
-author("duncan").

%%% Includes.
-include("log.hrl").
-include("paterl_lib.hrl").
%%-include("paterl.hrl").

%%% Public API.
-export([module/1]).

%%% Public types.
-export_type([type_defs/0, spec_defs/0, mb_funs/0, mb_defs/0, type_info/0]).
-export_type([result/0]).

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

% TODO: Change names of types() to type_defs() and document them.

%% Erlang abstract syntax form information.
-record(form_info, {
  functions = [] :: [f_function()],
  types = [] :: [f_type()],
  specs = [] :: [f_spec()],
  mailboxes = [] :: [f_mailbox(paterl_syntax:fun_ref() | undefined)]
}).

%%% Error types.

%% Untyped function signature. e_mb_sig_type_undef
-define(E_UNDEF__FUN_SPEC, e_undef__fun_spec).

%% Fun reference associated with more than one mailbox interface.
-define(E_DUP__MB_FUN_REF, e_dup__mb_fun_ref).

%% Undefined function reference.
-define(E_UNDEF__FUN_REF, e_undef__fun_ref).

%% Undefined mailbox type.
-define(E_UNDEF__MB_TYPE, e_undef__mb_type).

%% Invalid message tag.
-define(E_BAD__MSG_TAG, e_bad__msg_tag).

%% No pid() associated with mailbox type.
-define(W_NO__MB_FUN_REF, w_no__mb_fun_ref).

%% No pid() associated with mailbox type.
-define(W_NO__PID, w_no__pid).

%% Mailbox not initialized with new modality.
-define(E_NO__MB_NEW, e_no__mb_new).

%% User-defined mailbox interface type contains types other than a message set.
-define(E_BAD__MSG_TYPE, e_bad__msg_type).

%% User-defined message type contains payload types other than a built-in or a
%% user-defined mailbox interface type.
-define(E_BAD__PAY_TYPE, e_bad__pay_type).

%% Undefined type.
-define(E_UNDEF__TYPE, e_undef__type).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Mailbox usage modality.".
-type modality() :: ?M_NEW | ?M_USE.

-doc "Function form.".
-type f_function() :: {function, paterl_syntax:anno(), paterl_syntax:fun_ref()}.

-doc "Type form.".
-type f_type() :: {type, paterl_syntax:anno(), {paterl_syntax:name(), Type :: paterl_syntax:type(), Vars :: []}}.

-doc "Function form.".
-type f_spec() :: {spec, paterl_syntax:anno(), {paterl_syntax:fun_ref(), Type :: paterl_syntax:type()}}.

-doc "Mailbox form.".
-type f_mailbox(FunRef) :: {mailbox, paterl_syntax:anno(), {{modality(), paterl_syntax:name()}, FunRef}}.





% TODO: change variables and function names that depend on these types.
-type type_defs() :: #{paterl_syntax:name() => {type | mbox, paterl_syntax:anno(), Type :: paterl_syntax:type(), Vars :: []}}.

-type spec_defs() :: #{paterl_syntax:fun_ref() => {spec, paterl_syntax:anno(), FunTypes :: [paterl_syntax:type()]}}.

-type mb_funs() :: #{paterl_syntax:fun_ref() => {modality(), paterl_syntax:anno(), paterl_syntax:name()}}.

-type mb_defs() :: #{paterl_syntax:name() => {paterl_syntax:anno(), modality()}}.

-doc "Module type information.".
-type type_info() :: #type_info{}.

-doc "Erlang abstract form information".
-type form_info() :: #form_info{}.

-doc "Return result.".
-type result() :: {ok, type_info(), Warnings :: paterl_errors:warnings()} |
{errors, Warnings :: paterl_errors:warnings(), Errors :: paterl_errors:errors()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% Errors that can arise in addition to analyse_type_info.
% 0. See the errors by analyze_type_info
% 1. Mailbox interface is used without new
% 2. Mailbox interface are not defined.
% 3. Mailbox interface types and message types are invalid defined.

-spec module(paterl_syntax:forms()) -> result().
module(Forms) when is_list(Forms) ->
  Analysis =
    maybe
    % Extract type, spec, and mailbox definition information.
      #analysis{status = ok, result = TInfo} ?= analyze_type_info(Forms),
      #type_info{type_defs = TypeDefs, mb_defs = MbDefs} = TInfo,

      % Check that mailbox interface uses are initialized with new.
      #analysis{status = ok} ?= check_mb_new(MbDefs),

      % Check that mailbox interface definitions have corresponding types.
      #analysis{status = ok} ?= check_mb_types_defined(MbDefs, TypeDefs),

      % Check validity of mailbox interface types.
      #analysis{status = ok, warnings = Warnings} ?= check_mb_types_valid(TypeDefs),
      #analysis{file = paterl_syntax:get_file(Forms), result = TInfo,
        warnings = Warnings
      }
    end,

  % Return analysis with possible errors as result.
  paterl_lib:return(Analysis).





-spec analyze_forms(paterl_syntax:forms()) -> form_info().
analyze_forms(Forms) ->
  lists:foldl(fun analyze_form/2, #form_info{}, Forms).

-spec analyze_form(Form, FormInfo) -> FormInfo0
  when
  Form :: paterl_syntax:form(),
  FormInfo :: form_info(),
  FormInfo0 :: form_info().
analyze_form(Form, FormInfo) ->
  Anno = erl_syntax:get_pos(Form),
  case erl_syntax_lib:analyze_form(Form) of
    {attribute, _} ->
      case erl_syntax_lib:analyze_attribute(Form) of
        Wild = {Name, _}
          when Name =:= type; Name =:= spec; Name =:= ?M_NEW; Name =:= ?M_USE ->
          ?TRACE("Attribute '~p'.", [Wild]),
          add_form_info(Wild, Anno, FormInfo);
        _ ->
          ?TRACE("Skip attribute '~s'", [erl_prettypr:format(Form)]),
          FormInfo
      end;
    Fun = {function, {_Name, _Arity}} ->
      % Function definition.
      ?TRACE("Function '~s/~b'.", [_Name, _Arity]),
      add_form_info(Fun, Anno, FormInfo);
    _ ->
      % Other form.
      ?TRACE("Skip form '~s'.", [erl_prettypr:format(Form)]),
      FormInfo
  end.

-spec add_form_info(InfoPair, Anno, FormInfo) -> FormInfo0
  when
  InfoPair :: {atom(), term()},
  Anno :: paterl_syntax:anno(),
  FormInfo :: form_info(),
  FormInfo0 :: form_info().
add_form_info({function, Info}, Anno, FormInfo = #form_info{functions = FunRefs}) ->
  {_Fun, _Arity} = Info,
  FormInfo#form_info{functions = [{function, Anno, Info} | FunRefs]};
add_form_info({type, Info}, Anno, FormInfo = #form_info{types = Types}) ->
  {_Name, _Type, _Vars = []} = Info,
  FormInfo#form_info{types = [{type, Anno, Info} | Types]};
add_form_info({spec, Info}, Anno, FormInfo = #form_info{specs = Specs}) ->
  {_FunRef = {_Fun, _Arity}, _Type} = Info,
  FormInfo#form_info{specs = [{spec, Anno, Info} | Specs]};
add_form_info({MbMod, Info}, Anno, FormInfo = #form_info{mailboxes = MbDefs})
  when MbMod =:= ?M_NEW; MbMod =:= ?M_USE ->
  FlatMbDefs =
    case Info of
      {MbName, []} ->
        % Mailbox interface definition with no fun references.
        [{mailbox, Anno, {{MbMod, MbName}, undefined}}];
      {MbName, FunRefs} ->
        % Mailbox interface definition with fun references.
        [{mailbox, Anno, {{MbMod, MbName}, FunRef}} || FunRef <- FunRefs]
    end,
  FormInfo#form_info{mailboxes = MbDefs ++ FlatMbDefs}.





% Errors can arise due to:
% One or more function signatures do not have specs.
% One or more function signatures are associated with the same mailbox.
% One or more function signatures in mailbox definitions are not
% declared as types.
% Warnings can arise due to:
% Mailbox interface definitions not being used by functions.


-spec analyze_type_info(paterl_syntax:forms()) -> paterl_lib:analysis().
analyze_type_info(Forms) ->

  % Read raw Erlang forms.
  FormInfo = #form_info{functions = Functions, mailboxes = Mailboxes} =
    analyze_forms(Forms),
  ?TRACE("Functions: ~p", [FormInfo#form_info.functions]),
  ?TRACE("Types: ~p", [FormInfo#form_info.types]),
  ?TRACE("Specs: ~p", [FormInfo#form_info.specs]),
  ?TRACE("Mailboxes: ~p", [FormInfo#form_info.mailboxes]),

  SpecDefs = make_spec_defs(FormInfo#form_info.specs),

  maybe
  % Check that functions have corresponding specs defined.
    #analysis{status = ok} ?= check_fun_specs(Functions, SpecDefs),

    % Check that mailbox interface definitions are used. %TODO: Move it to syntax checking module.
    #analysis{status = ok, warnings = Warnings} ?=
    check_unused_mb_defs(Mailboxes),
    UsedMailboxes = used_mailboxes(Mailboxes),

    % Check that mailbox interface definitions are used by at most one function.
    #analysis{status = ok} ?= check_mb_dup_fun_use(UsedMailboxes),
    MbFuns = make_mb_funs(UsedMailboxes),

    % Check that fun references in mailbox interface definitions are defined.
    #analysis{status = ok} ?= check_mb_fun_refs_defined(MbFuns, SpecDefs),
    MbDefs = make_mb_defs(Mailboxes),
    TypeInfo =
      #type_info{
        type_defs = make_type_defs(FormInfo#form_info.types, MbDefs),
        spec_defs = SpecDefs,
        mb_defs = MbDefs,
        mb_funs = MbFuns
      },
    #analysis{result = TypeInfo, warnings = Warnings}
  end.

% TODO: Move to syntax module.
check_unused_mb_defs(MbDefs) when is_list(MbDefs) ->
  Fun =
    fun({mailbox, _, {{_, _}, _FunRef = {_, _}}}, Analysis) ->
      Analysis;
      ({mailbox, Anno, {{_, MbName}, _FunRef = undefined}}, Analysis) ->
        ?pushWarning(?W_NO__MB_FUN_REF, paterl_syntax:name(MbName, Anno), Analysis)
    end,
  lists:foldl(Fun, #analysis{}, MbDefs).


%% TODO: eliminate the mb_defs from type_info record.
%% TODO: Move the warning of unused mailboxes to the syntax check.


%%% ----------------------------------------------------------------------------
%%% Program mailbox interface and type spec extraction.
%%% ----------------------------------------------------------------------------

-doc """
Creates the list of used mailbox interface names.

Used mailbox interface names are ones that are paired with **at least one** fun
reference. In code, unused mailbox interfaces look thus:
```erlang
-new({mb_name, []}). % Empty fun reference list.
```
whereas used mailbox interfaces are defined as follows:
```erlang
-new({mb_name, [f/0]}). % Fun reference list contains at least one item.
```
""".
-spec used_mailboxes(Mailboxes) -> Mailboxes0
  when
  Mailboxes :: [f_mailbox(paterl_syntax:fun_ref() | undefined)],
  Mailboxes0 :: [f_mailbox(paterl_syntax:fun_ref())].
used_mailboxes(Mailboxes) when is_list(Mailboxes) ->
  Fun = fun({mailbox, _, {{_, _}, _FunRef = {_, _}}}) -> true; (_) -> false end,
  lists:filter(Fun, Mailboxes).

-doc """
Creates a [`specs()`](`t:specs/0`) mapping from fun references to corresponding
type specs.

Assumes that function specs are unique, as enforced by the Erlang linter.

### Returns
- [`specs()`](`t:specs/0`) mapping
""".
-spec make_spec_defs([f_spec()]) -> spec_defs().
make_spec_defs(Specs) when is_list(Specs) ->
  Fun =
    fun({spec, Anno, {FunRef = {_, _}, Type}}, Ctx) ->
      Ctx#{FunRef => {spec, Anno, Type}}
    end,
  lists:foldl(Fun, #{}, Specs).

-doc """
Creates a [`mb_funs()`](`t:mb_funs/0`) mapping from fun references to
corresponding mailbox interface names.

### Returns
- [`mb_funs()`](`t:mb_funs/0`) [`mb_funs()`](`t:mb_funs/0`) mapping
""".
-spec make_mb_funs(Mailboxes) -> mb_funs()
  when Mailboxes :: [f_mailbox(paterl_syntax:fun_ref())].
make_mb_funs(Mailboxes) when is_list(Mailboxes) ->
  Fun =
    fun({mailbox, Anno, {{MbMod, MbName}, FunRef = {_, _}}}, Ctx) ->
      Ctx#{FunRef => {MbMod, Anno, MbName}}
    end,
  lists:foldl(Fun, #{}, Mailboxes).

-doc """
Creates a [`mb_defs()`](`t:mb_defs/0`) mapping from mailbox interface names to
corresponding `-new` or `-use` modalities.

Each mailbox interface definition name is tagged with its latest usage modality
`-new` or `-use` as it occurs lexically in code.

If a mailbox interface definition name with a `-use` modality is later followed
by a `-new` usage in the code, the latter modality supersedes the `-use` with
the updated position in the code.

Returned mailbox defintion interface names paired with the `-new` modality
indicate that the corresponding mailbox is properly utilized in code. Mailbox
interface definition names paired with the `-use` modality identify names that
are improperly uninitialized.

### Returns
- [`mb_defs()`](`t:mb_defs/0`) mapping
""".
-spec make_mb_defs(Mailboxes) -> mb_defs()
  when
  Mailboxes :: [f_mailbox(paterl_syntax:fun_ref() | undefined)].
make_mb_defs(Mailboxes) when is_list(Mailboxes) ->
  Fun =
    fun({mailbox, Anno, {{MbMod, MbName}, _FunRef}}, Ctx) ->
      maps:update_with(MbName,
        fun({new, _}) -> {new, Anno}; (_) -> {MbMod, Anno} end,
        {MbMod, Anno}, Ctx)
    end,
  lists:foldl(Fun, #{}, Mailboxes).

-doc """
Creates a [`types()`](`t:types/0`) mapping from type names to corresponding type
definitions.

### Returns
- [`types()`](`t:types/0`) mapping
""".
-spec make_type_defs([f_type()], mb_defs()) -> type_defs().
make_type_defs(Types, MbDefs) when is_list(Types), is_map(MbDefs) ->
  Fun =
    fun({type, Anno, {Name, Type, Vars = []}}, Ctx)
      when is_map_key(Name, MbDefs) ->
      % User-defined mailbox interface type.
      Ctx#{Name => {?T_MBOX, Anno, Type, Vars}};
      ({type, Anno, {Name, Type, Vars = []}}, Ctx) ->
        % User-defined data type.
        Ctx#{Name => {?T_TYPE, Anno, Type, Vars}}
    end,
  lists:foldl(Fun, #{}, Types).


%%% ----------------------------------------------------------------------------
%%% Type consistency checks.
%%% ----------------------------------------------------------------------------

-doc """
Checks that functions have corresponding specs.

# Returns
- `ok` if all functions have corresponding specs
- `error` with details otherwise
""".
-spec check_fun_specs([f_function()], spec_defs()) -> paterl_lib:analysis().
check_fun_specs(Functions, SpecDefs)
  when is_list(Functions), is_map(SpecDefs) ->
  Fun =
    fun({function, _, FunRef = {_, _}}, Analysis)
      when is_map_key(FunRef, SpecDefs) ->
      % Fun reference has corresponding spec.
      Analysis;
      ({function, Anno, FunRef = {_, _}}, Analysis) ->
        % Non-existent function spec.
        Node = paterl_syntax:fun_reference(FunRef, Anno),

        ?ERROR("Function '~s' has missing spec.", [erl_prettypr:format(Node)]),
        ?pushError(?E_UNDEF__FUN_SPEC, Node, Analysis)
    end,
  lists:foldl(Fun, #analysis{}, Functions).

-doc """
Checks that fun references use a mailbox interface name at most once.

This means that a fun reference is permitted the use of at most one mailbox name
in either the `-new` or `-use` modality.

### Returns
- `ok` if all fun references use a mailbox interface name at most once
- `error` with details otherwise
""".
-spec check_mb_dup_fun_use(Mailboxes) -> paterl_lib:analysis()
  when
  Mailboxes :: [f_mailbox(paterl_syntax:fun_ref() | undefined)].
check_mb_dup_fun_use(Mailboxes) when is_list(Mailboxes) ->
  Fun =
    fun({mailbox, Anno, {{_, MbName}, FunRef = {_, _}}}, {MbFunRefs, Analysis})
      when is_map_key({MbName, FunRef}, MbFunRefs) ->
      % Mailbox interface already used with another fun reference. Duplicate
      % usage of mailbox interfaces by fun references does not distinguish
      % between -new and -use modalities: both are treated as duplications.
      Node = paterl_syntax:fun_reference(FunRef, Anno),

      ?ERROR("Duplicate fun reference '~s' use in mailbox interface '~s'", [
        erl_prettypr:format(Node), MbName
      ]),
      {MbFunRefs, ?pushError(?E_DUP__MB_FUN_REF, Node, Analysis)};
      ({mailbox, _, {{_, MbName}, FunRef = {_, _}}}, {MbFunRefs, Analysis}) ->
        % Mailbox interface not yet used by a fun reference.
        MbFunRef = {MbName, FunRef},
        {MbFunRefs#{MbFunRef => MbName}, Analysis}
    end,
  {_, Analysis0} = lists:foldl(Fun, {#{}, #analysis{}}, Mailboxes),
  Analysis0.

-doc """
Checks that fun references using mailbox interfaces are defined.

### Returns
- `ok` if all fun references using mailbox interfaces are defined
- `error` with details otherwise
""".
-spec check_mb_fun_refs_defined(mb_funs(), spec_defs()) -> paterl_lib:analysis().
check_mb_fun_refs_defined(MbFuns, SpecDefs)
  when is_map(MbFuns), is_map(SpecDefs) ->
  Fun =
    fun(FunRef = {_, _}, {_, _, _MbName}, Analysis)
      when is_map_key(FunRef, SpecDefs) ->
      % Defined fun reference in mailbox interface.
      Analysis;
      (FunRef = {_, _}, {_, Anno, _MbName}, Analysis) ->
        % Undefined fun reference in mailbox interface.
        Node = paterl_syntax:fun_reference(FunRef, Anno),
        ?ERROR("Undefined fun reference '~s' in mailbox interface '~s'.", [
          erl_prettypr:format(Node), _MbName
        ]),
        ?pushError(?E_UNDEF__FUN_REF, Node, Analysis)
    end,
  maps:fold(Fun, #analysis{}, MbFuns).

-doc """
Checks that mailbox interface names have corresponding types defined.

### Returns
- `ok` if all mailbox interface names have corresponding types defined
- `error` with details otherwise
""".
-spec check_mb_types_defined(mb_defs(), type_defs()) -> paterl_lib:analysis().
check_mb_types_defined(MbDefs, TypeDefs)
  when is_map(MbDefs), is_map(TypeDefs) ->
  Fun =
    fun(MbName, {_, _}, Analysis) when is_map_key(MbName, TypeDefs) ->
      % Mailbox interface name defined as type.
      Analysis;
      (MbName, {_, Anno}, Analysis) ->
        % Undefined mailbox interface name type.
        ?ERROR("Undefined mailbox interface type '~s'.", [MbName]),
        ?pushError(?E_UNDEF__MB_TYPE, paterl_syntax:name(MbName, Anno), Analysis)
    end,
  maps:fold(Fun, #analysis{}, MbDefs).

-doc """
Checks that mailbox interface names are initialized with `-new`.

### Returns
- `ok` if all mailbox interface names are initialized with `-new`
- `error` with details otherwise
""".
-spec check_mb_new(mb_defs()) -> paterl_lib:analysis().
check_mb_new(MbDefs) when is_map(MbDefs) ->
  Fun =
    fun(_, {?M_NEW, _}, Analysis) ->
      % Mailbox interface definition utilized with new.
      Analysis;
      (MbName, {?M_USE, Anno}, Analysis) ->
        % Mailbox interface definition not utilized with new.
        ?ERROR("Mailbox interface definiton '~s' uninitialized with '-new'.", [
          MbName
        ]),
        ?pushError(?E_NO__MB_NEW, paterl_syntax:name(MbName, Anno), Analysis)
    end,
  maps:fold(Fun, #analysis{}, MbDefs).

-doc """
Checks that mailbox interface types are correctly defined.

See `check_msg_type_set/3` for details on correct mailbox interface type
definitions.

### Returns
- `ok` if all mailbox interface types are correctly defined
- `error` with details otherwise

A warning is emitted whenever [`pid()`](`t:pid/0`) is missing in the mailbox
type definition.
""".
-spec check_mb_types_valid(type_defs()) -> paterl_lib:analysis().
check_mb_types_valid(TypeDefs) when is_map(TypeDefs) ->
  Fun =
    fun(Name, {?T_MBOX, Anno, Type, _Vars = []}, Analysis) ->
      ?TRACE("Check mailbox interface type '~s() :: ~s'.", [
        Name, erl_prettypr:format(Type)]
      ),
      % Reset success status. This is required since the called functions reuse
      % the analysis record to track success or error. Failing to reset the
      % status would mean that a possible error status in the previous analysis
      % is carried in this one.
      Analysis0 = paterl_lib:reset_status(Analysis),
      check_mb_type_valid(Name, Anno, Type, TypeDefs, Analysis0);
      (_, {?T_TYPE, _, _, _}, Analysis) ->
        % Skip user-defined non-mailbox interface types.
        Analysis
    end,
  Analysis = maps:fold(Fun, #analysis{}, TypeDefs),


  % Result is internal and should not be visible to callers. Only status is
  % relevant.
  paterl_lib:reset_result(paterl_lib:set_status(Analysis)).

-doc """
Checks that a mailbox interface type is correctly defined.

The built-in [`pid()`](`t:pid/0`) type in mailbox interface type definitions is
required by tools, such as Dialyzer, to successfully type check them.

This recursive check is performed to detect if the mailbox interface type
includes [`pid()`](`t:pid/0`) in its definition hierarchy.

See `check_msg_type_set/3` for details.

### Returns
- `ok` if the mailbox interface type is correctly defined
- `error` with details otherwise

A warning is emitted whenever [`pid()`](`t:pid/0`) is missing.
""".
-spec check_mb_type_valid(Name, Anno, Type, TypeDefs, Analysis) -> Analysis0
  when
  Name :: paterl_syntax:name(),
  Anno :: paterl_syntax:anno(),
  Type :: paterl_syntax:type(),
  TypeDefs :: type_defs(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_mb_type_valid(Name, Anno, Type, TypeDefs, Analysis) ->
  maybe
  % Set pid() type found flag to false.
    Analysis0 = #analysis{result = false} ?=
    check_msg_type_set(Type, TypeDefs, Analysis#analysis{result = false}),
    ?WARN("Missing built-in 'pid()' type."),
    ?pushWarning(?W_NO__PID, paterl_syntax:name(Name, Anno), Analysis0)
  end.

-doc """
Checks that a message type set is correctly defined.

A mailbox interface type definition consists of a set of messages, namely the:
1. empty set, expressed by the built-in [`pid()`](`t:pid/0`) type
2. singleton set, expressed by an inline message type
3. non-trivial set, expressed as a type union
4. another messsage set, expressed as another type abiding by 1, 2, and 3.

A message type `{msg, Elements}` is a tuple with an atomic tag and list of
payload types. Payload types can themselves be:
1. primitive built-in types, such as, [`integer()`](`t:integer/0`),
[`float`](`t:float/0`), and [`string`](`t:string/0`)
2. mailbox types

Any other primitieve type, in particular, [`pid()`](`t:pid/0`), and other
user-defined types are invalid message payload types.

### Returns
- `ok` if the message type set is correctly defined
- `error` with details otherwise

A warning is emitted whenever [`pid()`](`t:pid/0`) is missing.
""".
-spec check_msg_type_set(Type, TypeDefs, Analysis) -> Analysis0
  when
  Type :: paterl_syntax:type(),
  TypeDefs :: type_defs(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_msg_type_set(_Type = {type, _, pid, _}, _, Analysis) ->
  % Built-in pid type. Denotes the empty message set.
  ?TRACE("Valid built-in type '~s'.", [erl_prettypr:format(_Type)]),
  Analysis#analysis{result = _HasPidType = true};
check_msg_type_set(Type = {type, _, tuple, _}, TypeDefs, Analysis) ->
  % Tuple type. Check message type validity. Denotes the singleton message set.
  ?TRACE("Check inline message type '~s'.", [erl_prettypr:format(Type)]),
  check_msg_type(Type, TypeDefs, Analysis);
check_msg_type_set(_Type = {user_type, _, Name, _Vars}, TypeDefs, Analysis) ->
  % User-defined type. Check message type validity. Denotes another message type
  % which is itself a message set.
  ?TRACE("Check user-defined type '~s'.", [erl_prettypr:format(_Type)]),

  case maps:get(Name, TypeDefs, undefined) of
    {?T_TYPE, _, Type0, _} ->
      % Possibly valid message type.
      check_msg_type_set(Type0, TypeDefs, Analysis);
    {?T_MBOX, _, Type0, _} ->
      % Invalid mailbox interface type used as a message type.
      ?ERROR("Mailbox interface type '~s' is a bad message type.", [
        erl_prettypr:format(_Type)
      ]),
      ?pushError(?E_BAD__MSG_TYPE, _Type, Analysis);
    undefined ->
      % Undefined type. Ruled out be the Erlang preprocessor. This case arises
      % only when erl_lint is not used in prior passes.
      ?ERROR("Undefined type '~s'.", [erl_prettypr:format(_Type)]),
      ?pushError(?E_UNDEF__TYPE, _Type, Analysis)
  end;
check_msg_type_set(_Type = {type, _, union, MsgTypes}, TypeDefs, Analysis) ->
  % User-defined type union. Check message set validity. This case is handled
  % using check_msgs_types because MsgTypes is a list.
  ?TRACE("Check union type '~s'.", [erl_prettypr:format(_Type)]),
  check_msg_types(MsgTypes, TypeDefs, Analysis);
check_msg_type_set(Type, _, Analysis) ->
  % Invalid type.
  ?ERROR("Bad type '~s'", [erl_prettypr:format(Type)]),
  ?pushError(?E_BAD__MSG_TYPE, Type, Analysis).

-doc """
Checks that a message type list is correctly defined.

### Returns
- `ok` if the message type list is correctly defined
- `error` with details otherwise

A warning is emitted whenever [`pid()`](`t:pid/0`) is missing.
""".
-spec check_msg_types([paterl_syntax:type()], type_defs(), Analysis) -> Analysis0
  when
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_msg_types([], _, Analysis) ->
  Analysis;
check_msg_types([Type | Types], TypeDefs, Analysis) ->
  Analysis0 = check_msg_type_set(Type, TypeDefs, Analysis),
  check_msg_types(Types, TypeDefs, Analysis0).

-doc """
Checks that a message type is correctly defined.

### Returns
- `ok` if the message type is correctly defined
- `error` with details otherwise
""".
-spec check_msg_type(Type, TypeDefs, Analysis) -> Analysis0
  when
  Type :: paterl_syntax:type(),
  TypeDefs :: type_defs(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_msg_type({type, _, tuple, [Tag | Elems]}, TypeDefs, Analysis) ->
  maybe
  % Check message tag validity.
    #analysis{status = ok} ?= check_valid_tag(Tag, Analysis),

    % Check message elements validity. Only primitive built-in or user-defined
    % mailbox interface types are valid.
    check_msg_elems(Elems, TypeDefs, Analysis)
  end.

-doc """
Checks that a types in a message element list are correctly defined.

### Returns
- `ok` if the types in the message element list are correctly defined
- `error` with details otherwise
""".
-spec check_msg_elems(Elems, TypeDefs, Analysis) -> Analysis0
  when
  Elems :: [paterl_syntax:type()],
  TypeDefs :: type_defs(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_msg_elems([], _, Analysis) ->
  Analysis;
check_msg_elems([_Type = {type, _, integer, _} | Elems], TypeDefs, Analysis) ->
  % Built-in primitive type.
  ?TRACE("Valid built-in primitive type '~s'.", [erl_prettypr:format(_Type)]),
  check_msg_elems(Elems, TypeDefs, Analysis);
check_msg_elems([Type = {user_type, _, Name, _} | Elems], TypeDefs, Analysis) ->
  % User-defined type.
  ?TRACE("Check user-defined type '~s'.", [erl_prettypr:format(Type)]),

  case maps:get(Name, TypeDefs, undefined) of
    {?T_MBOX, _, _, _Vars} ->
      % Valid mailbox interface type.
      ?TRACE("Valid mailbox interface type '~s'.", [erl_prettypr:format(Type)]),
      check_msg_elems(Elems, TypeDefs, Analysis);
    {?T_TYPE, _, _, _Vars} ->
      % Invalid type used as a mailbox interface type.
      ?ERROR("Bad mailbox interface type '~s'.", [erl_prettypr:format(Type)]),
      % User-defined type is a normal type.
      ?pushError(?E_BAD__PAY_TYPE, Type, Analysis);
    undefined ->
      % Undefined type. Ruled out be the Erlang preprocessor. This case arises
      % only when erl_lint is not used in prior passes.
      ?ERROR("Undefined type '~s'.", [erl_prettypr:format(Type)]),
      ?pushError(?E_UNDEF__TYPE, Type, Analysis)
  end;
check_msg_elems([Elem | Elems], TypeDefs, Analysis) ->
  % Invalid type.
  ?TRACE("Bad type '~s'.", [erl_prettypr:format(Elem)]),
  Analysis0 = ?pushError(?E_BAD__PAY_TYPE, Elem, Analysis),
  check_msg_elems(Elems, TypeDefs, Analysis0).

-doc """
Checks that a message tag is valid.

### Returns
- `ok` if the message tag is valid
- `error` with details otherwise
""".
-spec check_valid_tag(Term, Analysis) -> Analysis0
  when Term :: term(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_valid_tag(_Tag = {atom, _, _}, Analysis) ->
  ?TRACE("In check_valid_tag analysis ~p", [Analysis]),
  ?TRACE("Valid '~s' tag.", [erl_prettypr:format(_Tag)]),
  Analysis;
check_valid_tag(Term, Analysis) ->
  ?ERROR("Bad '~s' tag.", [erl_prettypr:format(Term)]),
  ?pushError(?E_BAD__MSG_TAG, Term, Analysis).


%% What Phil said: The receive annotations having a mailbox name must check that
%% that mailbox name is in scope. ie, I cannot refer to a future_mb mailbox from
%% a duncan_mb mailbox scope. This would be in another file for semantic checks
%% once we implement multiple mailboxes.


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
format_error({?E_UNDEF__FUN_SPEC, Node}) ->
  io_lib:format(
    "function '~s' has missing spec",
    [erl_prettypr:format(Node)]
  );
format_error({?E_DUP__MB_FUN_REF, Node}) ->
  io_lib:format(
    "fun reference '~s' associated with more than one mailbox definition",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__FUN_REF, Node}) ->
  io_lib:format(
    "mailbox interface definition contains undefined fun reference '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__MB_TYPE, Node}) ->
  io_lib:format(
    "undefined mailbox interface type '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MSG_TAG, Node}) ->
  io_lib:format(
    "bad message tag '~s'",
    [erl_prettypr:format(Node)]
  );
%%format_error({?E_BAD__MSG_ELEM_TYPE, Node}) -> %TODO: See if this is used. It is but remove it and use ?E_BAD__MB_TYPE instead.
%%  io_lib:format(
%%    "bad message element type '~s'",
%%    [erl_prettypr:format(Node)]
%%  );
format_error({?W_NO__PID, Node}) ->
  io_lib:format(
    "mailbox interface type '~s' does not contain pid(), which makes it incompatible with Dialyzer",
    [erl_prettypr:format(Node)]
  );
format_error({?E_NO__MB_NEW, Node}) ->
  io_lib:format(
    "mailbox interface definiton '~s' uninitialized with '-new'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MSG_TYPE, Node}) ->
  io_lib:format(
    "bad type '~s' in mailbox interface type; use a message or pid() type",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__PAY_TYPE, Node}) ->
  io_lib:format(
    "bad type '~s' in message type; use a built-in or mailbox interface type",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__TYPE, Node}) ->
  io_lib:format(
    "undefined type '~s'",
    [erl_prettypr:format(Node)]
  ).


%% TODO: REFACTOR WITH NON anonymous functions

%% TODO: REFACTOR WITH maybe

%%% TODO: Move to paterl syntax.
%%to_erl_af(ANNO, Name) when is_atom(Name) ->
%%  revert(set_pos(atom(Name), ANNO));
%%to_erl_af(ANNO, {Name, Arity})
%%  when is_atom(Name), is_integer(Arity) ->
%%  revert(set_pos(implicit_fun(atom(Name), integer(Arity)), ANNO)).







