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
%%-include("errors.hrl").
-include("paterl.hrl").

%%% Imports.
-import(erl_syntax, [
type/1,
concrete/1,
get_pos/1,
copy_pos/2,
attribute_name/1,
function_name/1,
function_arity/1,
erl_parse/0,
atom/1,
integer/1,
implicit_fun/2,
revert/1,
set_pos/2
]).

%%% Public API.
-export([module/1]).

%%% Public types.
-export_type([types/0, specs/0, mb_funs/0, mb_defs/0, type_info/0]).
-export_type([result/0]).

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

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

%% Invalid message element type. Supported types are integers and strings.
-define(E_BAD__MSG_ELEM_TYPE, e_bad__msg_elem_type).

%% No pid() associated with mailbox type.
-define(W_NO__MB_FUN_REF, w_no__mb_fun_ref).

%% No pid() associated with mailbox type.
-define(W_NO__PID, w_no__pid).

%% Mailbox not initialized with new modality.
-define(E_NO__MB_NEW, e_no__mb_new).

%% User-defined mailbox interface type contains types other than a message.
-define(E_BAD__MSG_TYPE, e_bad__msg_type).

%% User-defined message type contains types other than a built-in or a
%% user-defined mailbox interface type.
-define(E_BAD__MB_TYPE, e_bad__mb_type).

%% Undefined type.
-define(E__UNDEF_TYPE, e__undef_type).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Mailbox usage modality.".
-type modality() :: ?M_NEW | ?M_USE.

-doc "Erlang abstract syntax type.".
-type type() :: erl_parse:abstract_type().

-doc "Function form.".
-type f_function() :: {function, paterl_syntax:anno(), paterl_syntax:fun_ref()}.

-doc "Type form.".
-type f_type() :: {type, paterl_syntax:anno(), {paterl_syntax:name(), Type :: type(), Vars :: []}}.

-doc "Function form.".
-type f_spec() :: {spec, paterl_syntax:anno(), {paterl_syntax:fun_ref(), Type :: type()}}.

-doc "Mailbox form.".
-type f_mailbox(FunRef) :: {mailbox, paterl_syntax:anno(), {{modality(), paterl_syntax:name()}, FunRef}}.



-doc "Erlang abstract form information".
-type form_info() :: #form_info{}.


% TODO.
-type types() :: #{paterl_syntax:name() => {type | mbox, paterl_syntax:anno(), Type :: type(), Vars :: []}}.

-type specs() :: #{paterl_syntax:fun_ref() => {spec, paterl_syntax:anno(), FunTypes :: [type()]}}.

-type mb_funs() :: #{paterl_syntax:fun_ref() => {modality(), paterl_syntax:anno(), paterl_syntax:name()}}.

-type mb_defs() :: #{paterl_syntax:name() => {paterl_syntax:anno(), modality()}}.

-doc "Module type information.".
-type type_info() :: #type_info{}.
%% Program type information type.

-doc "Return result.".
-type result() :: {ok, type_info(), Warnings :: paterl_errors:warnings()} |
{errors, Warnings :: paterl_errors:warnings(), Errors :: paterl_errors:errors()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------


%%-spec table(erl_syntax:forms()) -> result().
%%table2(Forms) when is_list(Forms) ->
%%  _File = paterl_syntax:get_file(Forms),
%%
%%  A =
%%
%%    % Extract type information from AST to table.
%%  case get_t_info(Forms) of
%%    #analysis{status = ok, result = TInfo} ->
%%%%        {ok, TInfo = #t_info{}, []} ->
%%
%%      #t_info{types = Types, specs = _Specs, mb_funs = _MbDefs, mb_defs = MbNames} = TInfo,
%%
%%%%          io:format("~n~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%%%          io:format("Types: ~p~n", [Types]),
%%%%          io:format("Specs: ~p~n", [Specs]),
%%%%          io:format("MbDefs: ~p~n", [MbDefs]),
%%%%          io:format("MbNames: ~p~n", [MbNames]),
%%%%          io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%
%%
%%      case check_mb_types_defined(MbNames, Types) of
%%%%            {ok, []} ->
%%        #analysis{status = ok} ->
%%          case check_mb_types_valid(Types) of
%%            #analysis{status = ok, warnings = Warnings} ->
%%%%                {ok, Warnings} ->
%%
%%              %% TODO: Check mb is new (we have no use without new). Uses MbNames.
%%              case check_mb_new(MbNames) of
%%%%                    {ok, []} ->
%%                #analysis{status = ok} ->
%%
%%                  #analysis{
%%                    file = paterl_syntax:get_file(Forms),
%%                    result = TInfo,
%%                    warnings = Warnings
%%                  };
%%%%                      {ok, TInfo, Warnings}; % TODO: Here we should create the analysis record and return_pack it.
%%                Analysis = #analysis{status = error} ->
%%                  % One or more mailbox definitions are used without new.
%%                  Analysis
%%              end;
%%            Analysis = #analysis{status = error} ->
%%              % One or more invalid messages types in mailbox definitions.
%%              Analysis
%%          end;
%%        Analysis = #analysis{status = error} ->
%%          % One or more undefined mailbox type names.
%%          Analysis
%%      end;
%%    Analysis = #analysis{status = error} ->
%%      % One or more function signatures associated with the same mailbox
%%      % definition or undeclared function signatures in mailbox definitions.
%%      Analysis
%%  end,
%%
%%  paterl_lib:return(A).

-spec module(paterl_syntax:forms()) -> result().
module(Forms) when is_list(Forms) ->
  Analysis =
    maybe
    % Extract type, spec, and mailbox definition information.
      #analysis{status = ok, result = TInfo} ?= analyze_type_info(Forms),
      #type_info{types = Types, mb_defs = MbDefs} = TInfo,

      % Check that mailbox interface definitions have corresponding types.
      #analysis{status = ok} ?= check_mb_types_defined(MbDefs, Types),

      % Check that mailbox interface uses are initialized with new.
      #analysis{status = ok} ?= check_mb_new(MbDefs),

      % Check validity of mailbox interface types.
      #analysis{status = ok, warnings = Warnings} ?= check_mb_types_valid(Types),
      #analysis{file = paterl_syntax:get_file(Forms), result = TInfo,
        warnings = Warnings
      }
    end,

  % Return analysis with possible errors as result.
  paterl_lib:return(Analysis).



%%-spec read_attribs(erl_syntax:forms()) -> {FunSigs, MbDefs, Types, FunSpecs}
%%  when
%%  FunSigs :: term(),
%%  MbDefs :: term(),
%%  Types :: term(),
%%  FunSpecs :: term().
%%read_attribs(Forms) ->
%%  lists:foldl(
%%    fun(_Form = {function, ANNO, Fun, Arity, _}, {Sigs, MbDefs, Types, Specs}) ->
%%      ?TRACE("Fun: ~p", [_Form]),
%%      ?DEBUG("~b: ~s/~b", [ANNO, Fun, Arity]),
%%      {add_fun_sig(ANNO, {Fun, Arity}, Sigs), MbDefs, Types, Specs};
%%
%%      (_Form = {attribute, ANNO, Modality, {Mailbox, Sigs0}}, {Sigs, MbDefs, Types, Specs})
%%        when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
%%        ?TRACE("MbDef: ~p", [_Form]),
%%        ?DEBUG("~b: -~s ~s with ~p", [ANNO, Modality, Mailbox, Sigs0]),
%%        {Sigs, add_mb_def(ANNO, Sigs0, Modality, Mailbox, MbDefs), Types, Specs};
%%
%%      (_Form = {attribute, ANNO, type, {Name, Type, Vars}}, {Sigs, MbDefs, Types, Specs}) ->
%%        ?TRACE("Type: ~p", [_Form]),
%%        ?DEBUG("~b: -type ~s", [ANNO, Name]),
%%        {Sigs, MbDefs, add_type(ANNO, Name, Type, Vars, Types), Specs};
%%
%%      (_Form = {attribute, ANNO, spec, {Sig = {_Fun, _Arity}, Types0}}, {Sigs, MbDefs, Types, Specs}) ->
%%        ?TRACE("Spec: ~p", [_Form]),
%%        ?DEBUG("~b: -spec ~s/~b", [ANNO, _Fun, _Arity]),
%%        {Sigs, MbDefs, Types, add_fun_spec(ANNO, Sig, Types0, Specs)};
%%
%%      (_Form, Data) ->
%%        ?TRACE("Form: ~p", [_Form]),
%%        ?DEBUG("~b: ~s ~p", [element(2, _Form), element(1, _Form),
%%          if size(_Form) > 2 -> element(3, _Form); true -> undefined end]),
%%        Data
%%    end, {[], [], [], []}, Forms).


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
add_form_info({function, Info}, ANNO, FormInfo = #form_info{functions = FunRefs}) ->
  {_Fun, _Arity} = Info,
  FormInfo#form_info{functions = [{function, ANNO, Info} | FunRefs]};
add_form_info({type, Info}, ANNO, FormInfo = #form_info{types = Types}) ->
  {_Name, _Type, _Vars = []} = Info,
  FormInfo#form_info{types = [{type, ANNO, Info} | Types]};
add_form_info({spec, Info}, ANNO, FormInfo = #form_info{specs = Specs}) ->
  {_FunRef = {_Fun, _Arity}, _Type} = Info,
  FormInfo#form_info{specs = [{spec, ANNO, Info} | Specs]};
add_form_info({MbMod, Info}, ANNO, FormInfo = #form_info{mailboxes = MbDefs})
  when MbMod =:= ?M_NEW; MbMod =:= ?M_USE ->
  FlatMbDefs =
    case Info of
      {MbName, []} ->
        % Mailbox interface definition with no fun references.
        [{mailbox, ANNO, {{MbMod, MbName}, undefined}}];
      {MbName, FunRefs} ->
        % Mailbox interface definition with fun references.
        [{mailbox, ANNO, {{MbMod, MbName}, FunRef}} || FunRef <- FunRefs]
    end,
  FormInfo#form_info{mailboxes = MbDefs ++ FlatMbDefs}.


%%-spec get_t_info(erl_syntax:forms()) -> paterl_lib:analysis().
%%get_t_info(Forms) ->
%%
%%  % Read raw attributes from AST. The consistency checking of type and function
%%  % specs w.r.t functions is already performed by the Erlang preprocessor and
%%  % we do not need to check that.
%%
%%  #form_info{functions = Functions, types = Types, specs = Specs, mailboxes = Mailboxes} = analyze_forms(Forms),
%%  ?TRACE("Functions: ~p", [Functions]),
%%  ?TRACE("Types: ~p", [Types]),
%%  ?TRACE("Specs: ~p", [Specs]),
%%  ?TRACE("MbDefs: ~p", [Mailboxes]),
%%
%%  SpecsCtx = #{} = make_specs(Specs),
%%  ?DEBUG("SpecsCtx:~n~p~n", [SpecsCtx]),
%%
%%  case check_fun_specs(Functions, SpecsCtx) of
%%    Analysis0 = #analysis{status = ok} ->
%%      % Get mailbox definition map for convenient use for type consistency checking.
%%      % Check that each function signature is associated to at most one mailbox
%%      % definition and modality.
%%
%%      % HERE STARTS MAILBOX TYPE CHECKING.
%%      case check_unused_mb_defs(Mailboxes) of
%%        #analysis{status = ok} ->
%%
%%          case check_mb_dup_fun_use(Mailboxes) of
%%            #analysis{status = ok} ->
%%              ?TRACE("check_mb_dup_fun_use = ok"),
%%
%%              MbFuns = make_mb_funs(Mailboxes),
%%              ?DEBUG("MbDefsCtx:~n~p~n", [MbFuns]),
%%
%%              % Check that function signatures used in mailbox definitions are defined.
%%              case check_mb_fun_refs_defined(MbFuns, SpecsCtx) of
%%                #analysis{status = ok} ->
%%                  % Get unique mailbox names.
%%                  MbDefs = make_mb_defs(Mailboxes),
%%                  ?DEBUG("MbDefs:~n~p~n", [MbDefs]),
%%
%%
%%                  % Type info record.
%%                  TInfo =
%%                    #type_info{
%%                      types = make_types(Types, MbDefs),
%%                      specs = SpecsCtx,
%%                      mb_defs = MbDefs,
%%                      mb_funs = MbFuns
%%                    },
%%                  X = #analysis{result = TInfo},
%%                  ?TRACE("Analysis record = ~p", [X]),
%%                  X;
%%
%%                Analysis2 = #analysis{status = error} -> % Here we do not need to match against, we can just use the variable Error.
%%                  % One or more function signatures in mailbox definitions are not
%%                  % declared as types.
%%                  Analysis2
%%              end;
%%            Analysis1 = #analysis{status = error} ->
%%              % One or more function signatures are associated with the same mailbox.
%%              ?TRACE("check_mb_dup_fun_use = error"),
%%              Analysis1
%%          end;
%%        AnalysisX = #analysis{status = error} ->
%%          AnalysisX
%%      end;
%%    Analysis0 = #analysis{status = error} ->
%%      % One or more function signatures do not have specs.
%%      ?TRACE("check_fun_specs2 = error"),
%%      Analysis0
%%  end.


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

  Specs = make_specs(FormInfo#form_info.specs),
  ?DEBUG("CTX Specs:~n~p~n", [Specs]),

  maybe
  % Check that functions have corresponding specs defined.
    #analysis{status = ok} ?= check_fun_specs(Functions, Specs),

    % Check that mailbox interface definitions are used. %TODO: Move it to syntax checking module.
    #analysis{status = ok, warnings = Warnings} ?=
    check_unused_mb_defs(Mailboxes),
    UsedMailboxes = used_mailboxes(Mailboxes),
    ?TRACE("FunRefMailboxes = ~p", [UsedMailboxes]),

    % Check that mailbox interface definitions are used by at most one function.
    #analysis{status = ok} ?= check_mb_dup_fun_use(UsedMailboxes),
    MbFuns = make_mb_funs(UsedMailboxes),

    % Check that fun references in mailbox interface definitions are defined.
    #analysis{status = ok} ?= check_mb_fun_refs_defined(MbFuns, Specs),
    MbDefs = make_mb_defs(Mailboxes),
    TypeInfo =
      #type_info{
        types = make_types(FormInfo#form_info.types, MbDefs),
        specs = Specs,
        mb_defs = MbDefs,
        mb_funs = MbFuns
      },
    X = #analysis{result = TypeInfo, warnings = Warnings},
    ?TRACE("Analysis record = ~p", [X]),
    X
  end.

% TODO: Move to syntax module.
check_unused_mb_defs(MbDefs) when is_list(MbDefs) ->
  Fun =
    fun({mailbox, _, {{_, _}, _FunRef = {_, _}}}, Analysis) ->
      Analysis;
      ({mailbox, Anno, {{_, MbName}, _FunRef = undefined}}, Analysis) ->
        ?pushWarning(?W_NO__MB_FUN_REF, to_erl_af(Anno, MbName), Analysis)
    end,
  lists:foldl(Fun, #analysis{}, MbDefs).


%% TODO: eliminate the mb_defs from type_info record.
%% TODO: Move the warning to the syntax check.


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
-spec make_specs([f_spec()]) -> specs().
make_specs(Specs) when is_list(Specs) ->
  Fun =
    fun({spec, ANNO, {FunRef = {_, _}, Type}}, Ctx) ->
      Ctx#{FunRef => {spec, ANNO, Type}}
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
-spec make_types([f_type()], mb_defs()) -> types().
make_types(Types, MbDefs) when is_list(Types), is_map(MbDefs) ->
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
-spec check_fun_specs([f_function()], specs()) -> paterl_lib:analysis().
check_fun_specs(Functions, Specs) when is_list(Functions), is_map(Specs) ->
  Fun =
    fun({function, _, FunRef = {_, _}}, Analysis)
      when is_map_key(FunRef, Specs) ->
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
-spec check_mb_fun_refs_defined(mb_funs(), specs()) -> paterl_lib:analysis().
check_mb_fun_refs_defined(MbFuns, Specs) when is_map(MbFuns), is_map(Specs) ->
  Fun =
    fun(FunRef = {_, _}, {_, _, _MbName}, Analysis)
      when is_map_key(FunRef, Specs) ->
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
-spec check_mb_types_defined(mb_defs(), types()) -> paterl_lib:analysis().
check_mb_types_defined(MbDefs, Types)
  when is_map(MbDefs), is_map(Types) ->
  Fun =
    fun(MbName, {_, _}, Analysis) when is_map_key(MbName, Types) ->
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





-spec check_mb_types_valid(#{}) -> paterl_lib:analysis().
check_mb_types_valid(Types = #{}) ->
  MbTypeCheckFun =
    fun(Name, {?T_MBOX, ANNO, Type, _TypeVars}, Analysis) ->
      ?TRACE("Check user-defined mailbox interface type '~s() :: ~s'.", [Name, erl_prettypr:format(Type)]),
      % Check user-defined mailbox interface type.

      % Do not check for nor stop in case of errors: these are handled by
      % caller functions.
%%      case check_msg_type_valid(Type, Types, Analysis, false) of
%%        Analysis0 = #analysis{status = error} ->
%%          % User-defined message type invalid.
%%          Analysis0;
%%
%%        Analysis0 = #analysis{status = ok, result = true} ->
%%%%        {true, Error0} ->
%%
%%          ?TRACE("Pid present in type defintion."),
%%          % PID present in mailbox definition.
%%          Analysis0;
%%        Analysis0 = #analysis{status = ok, result = false} ->
%%%%        {false, Error0} ->
%%
%%          % PID missing.
%%          ?TRACE("Pid MISSING ~p", [Analysis0]),
%%
%%%%            Node = revert(set_pos(atom(Name), ANNO)),
%%          Node = to_erl_af(ANNO, Name),
%%          ?pushWarning(?W_MB_NO_PID, Node, Analysis0)
%%      end;
      Analysis0 = paterl_lib:reset_status(Analysis),
      Anal = check_msg_type_valid3(Name, ANNO, Type, Types, Analysis0),
      ?TRACE("--- Anal: ~p", [Anal]),
      Anal;
      (_, {?T_TYPE, _, _, _}, Analysis) ->
        % Skip user-defined non-mailbox interface types.
        Analysis
    end,

  % Check validity of mailbox types definition including missing PIDs.
  Analysis = maps:fold(MbTypeCheckFun, #analysis{}, Types),


  % Result is internal and should not be visible to callers. Only status is
  % relevant.
%%  Analysis#analysis{result = undefined}.
  paterl_lib:reset_result(paterl_lib:set_status(Analysis)).


check_msg_type_valid3(MbName, ANNO, MsgType, Types, Analysis) ->

%%  case check_msg_type_valid2(MsgType, Types, Analysis#analysis{result = false}) of
%%    Analysis0 = #analysis{status = error} ->
%%      % User-defined message type invalid.
%%      ?ERROR("Invalid user-defined mailbox interface type '~s'.", [MbName]),
%%      Analysis0;
%%    Analysis0 = #analysis{status = ok, result = HadPidType} ->
%%      % User-defined message type valid and the extra build-in Pid type is
%%      % included in the message type definition.
%%      if HadPidType -> Analysis0;
%%        true ->
%%          ?WARN("Missing built-in 'pid()' type."),
%%          ?pushWarning(?W_MB_NO_PID, to_erl_af(ANNO, MbName), Analysis0)
%%      end
%%  end.
  case check_msg_type_valid2(MsgType, Types, Analysis#analysis{result = false}) of
    Analysis0 = #analysis{result = false} ->
      ?WARN("Missing built-in 'pid()' type."),
      ?pushWarning(?W_NO__PID, to_erl_af(ANNO, MbName), Analysis0);
    Analysis0 ->
      Analysis0
  end.

check_msg_type_valid2(_Type = {type, _ANNO, pid, _}, #{}, Analysis) ->
  % Built-in pid type found.
  ?TRACE("Check built-in type '~s'.", [erl_prettypr:format(_Type)]),
  Analysis#analysis{result = _HasPidType = true};
check_msg_type_valid2(_Type = {type, _ANNO, tuple, [Tag | Elems]}, Types = #{}, Analysis) ->
  % Inline message type.
  ?TRACE("Check inline message type '~s'.", [erl_prettypr:format(_Type)]),

  % Check that first element is a valid tag.
  case check_valid_tag(Tag, Analysis) of
    #analysis{status = ok} ->
      % Check that rest of tuple elements are valid types. Only primitive,
      % user-defined message, or user-defined mailbox interface types are
      % permitted as elements.

      ?TRACE("After validating the tag ~p", [Tag]),
      check_valid_tuple_elems2(Elems, Types, Analysis);


    Analysis0 = #analysis{status = error} ->
      ?TRACE("After invalidating the tag ~p", [Tag]),
      % First element not a valid tag.
      Analysis0
  end;
check_msg_type_valid2(_Type = {user_type, _ANNO, Name, _TypeVars}, Types = #{}, Analysis) ->
  % User-defined message type. Only user-defined message types are permitted in
  % mailbox interface definitions.
  ?TRACE("Check user-defined message type '~s'.", [erl_prettypr:format(_Type)]),


  case maps:get(Name, Types, undefined) of
    {?T_TYPE, _, Type0, _} ->
      % User-defined type is a message type.
      ?TRACE("Valid user-defined message type '~s'.", [erl_prettypr:format(_Type)]),
      ?TRACE(">> Type: ~p", [Name]),

%%      check_msg_type_valid3(MbName, ANNO, MsgType, Types, Analysis)
      check_msg_type_valid2(Type0, Types, Analysis);
    {?T_MBOX, _, Type0, _} ->
      % User-defined type is a mailbox interface type.
      ?ERROR("Invalid user-defined message type '~s'.", [erl_prettypr:format(_Type)]),
      %TODO: Unexpected mailbox interface type error.
%%      ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Elem, Analysis),
      ?pushError(?E_BAD__MSG_TYPE, _Type, Analysis);

%%      error(lists:flatten(io_lib:format("'~s' is not normal type.", [erl_prettypr:format(_Type)])));
    undefined ->
      ?ERROR("Undefined type '~s'.", [erl_prettypr:format(_Type)]),
      % Undefined types are ruled out by the Erlang preprocessor/liner. This
      % case should never arise if erl_lint was used in prior passes.
      ?pushError(?E__UNDEF_TYPE, _Type, Analysis)
%%      error("Undefined types are ruled out by the preprocessor/linter; use epp or erl_lint")
  end;
check_msg_type_valid2(_Type = {type, _ANNO, union, MsgTypes}, Types = #{}, Analysis) ->
  % Inline message types or user-defined message types. User-defined mailbox
  % interface types are not permitted in type unions.
  ?TRACE("Check union type '~s'.", [erl_prettypr:format(_Type)]),
  check_msgs_types_valid2(MsgTypes, Types, Analysis);

check_msg_type_valid2(Type, _, Analysis) ->
  % Invalid type.
  ?ERROR("Invalid type '~s'", [erl_prettypr:format(Type)]),
  ?pushError(?E_BAD__MSG_TYPE, Type, Analysis).



check_msgs_types_valid2([], _Types = #{}, Analysis) ->
  Analysis;
check_msgs_types_valid2([MsgType | MsgTypes], Types = #{}, Analysis) ->
  Analysis0 = check_msg_type_valid2(MsgType, Types, Analysis),
  check_msgs_types_valid2(MsgTypes, Types, Analysis0).


check_valid_tuple_elems2([], #{}, Analysis) ->
  Analysis;
check_valid_tuple_elems2([_Type = {type, _ANNO, integer, _} | Elems], Types = #{}, Analysis) ->
  % Built-in primitive type.
  ?TRACE("Check built-in primitive type '~s'.", [erl_prettypr:format(_Type)]),
  check_valid_tuple_elems2(Elems, Types, Analysis);
check_valid_tuple_elems2([_Type = {user_type, _ANNO, Name, _} | Elems], Types = #{}, Analysis) ->
  % User-defined mailbox interface type. Only primitive and user-defined mailbox
  % interface types are permitted in message tuple elements.
  ?TRACE("Check user-defined mailbox interface type '~s'.", [erl_prettypr:format(_Type)]),
  ?TRACE(">> Types map: ~p", [Types]),


  case maps:get(Name, Types, undefined) of
    {?T_MBOX, _, Type0, _Vars} ->
      % User-defined type is a mailbox interface type.
      ?TRACE("Valid user-defined mailbox interface type '~s'.", [erl_prettypr:format(_Type)]),
      check_valid_tuple_elems2(Elems, Types, Analysis);
    {?T_TYPE, _, Type0, _Vars} ->
      ?ERROR("Invalid user-defined mailbox interface type '~s'.", [erl_prettypr:format(_Type)]),
      % User-defined type is a normal type.
      % TODO: Unexpected type.
      ?pushError(?E_BAD__MB_TYPE, _Type, Analysis);
%%      error(lists:flatten(io_lib:format("'~s' is not a mailbox interface type.", [erl_prettypr:format(_Type)])));
    undefined ->
      ?ERROR("Undefined type '~s'.", [erl_prettypr:format(_Type)]),
      % Undefined types are ruled out by the Erlang preprocessor/liner. This
      % case should never arise if erl_lint was used in prior passes.
      ?pushError(?E__UNDEF_TYPE, _Type, Analysis)
%%      error("Undefined types are ruled out by the preprocessor/linter; use epp or erl_lint")
  end;


check_valid_tuple_elems2([Elem | Elems], Types = #{}, Analysis) ->
  ?TRACE("Invalid type '~s' in message.", [erl_prettypr:format(Elem)]),
  Analysis0 = ?pushError(?E_BAD__MSG_ELEM_TYPE, Elem, Analysis),
  check_valid_tuple_elems2(Elems, Types, Analysis0).


%%-spec check_msg_type_valid(Type, Types, Analysis, HasPid) -> paterl_lib:analysis()
%%  when
%%  Type :: {type, anno(), atom(), term()},
%%  Types :: map(),
%%  Analysis :: paterl_lib:analysis(),
%%  HasPid :: boolean().
%%check_msg_type_valid(_Type = {type, _ANNO, pid, _}, #{}, Analysis, _) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  Analysis#analysis{result = true};
%%%%  {true, Error};
%%
%%check_msg_type_valid(_Type = {type, _ANNO, tuple, [Node]}, #{}, Analysis, HasPid) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  % Message type with tag and no payload.
%%
%%%%  case is_valid_tag(Elem) of
%%%%    true ->
%%%%      ?TRACE("~p is a valid tag", [Elem]),
%%%%      {HasPid, Error};
%%%%    false ->
%%%%      ?TRACE("~p is NOT a valid tag", [Elem]),
%%%%      {HasPid, ?pushError(?E_MB_MSG_TAG_BAD, Elem, Error)}
%%%%  end;
%%  Analysis0 = check_valid_tag(Node, Analysis),
%%  Analysis0#analysis{result = HasPid};
%%%%  {HasPid, check_valid_tag(Node, Analysis)};
%%
%%
%%check_msg_type_valid(_Type = {type, _, tuple, [Elem | Elems]}, Types = #{}, Analysis, HasPid) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  % Message type with tag and payload.
%%
%%
%%%%  case is_valid_tag(Elem) of
%%%%    true ->
%%%%      ?TRACE("~p is a valid tag", [Elem]),
%%%%
%%%%      case is_valid_payload(Elems, Types) of
%%%%        true ->
%%%%          ?TRACE("~p is a valid payload", [Elems]),
%%%%          {HasPid, Error};
%%%%        false ->
%%%%          ?TRACE("~p is NOT a valid payload", [Elems]),
%%%%          {HasPid, Error}
%%%%      end;
%%%%    false ->
%%%%      ?TRACE("~p is NOT a valid tag", [Elem]),
%%%%      {false, Error}
%%%%  end;
%%
%%  case check_valid_tag(Elem, Analysis) of
%%    #analysis{status = ok} ->
%%      Analysis0 = check_valid_tuple_elems(Elems, Types, Analysis),
%%      Analysis0#analysis{result = HasPid};
%%%%      {HasPid, check_valid_tuple_nodes(Nodes, Types, Analysis)};
%%    Analysis0 = #analysis{status = error} ->
%%      Analysis0#analysis{result = HasPid}
%%%%      {HasPid, Error0}
%%  end;
%%check_msg_type_valid(_Type = {user_type, _ANNO, Type, _TypeVars}, Types = #{}, Analysis, HasPid) ->
%%  ?TRACE("USER Type: ~p", [_Type]),
%%
%%  % Query types map.
%%%%  case maps:is_key(Type, Types) of
%%%%    true ->
%%%%      {HasPid, Error};
%%%%    false ->
%%%%      {HasPid, Error}
%%%%  end;
%%  case maps:get(Type, Types, undefined) of
%%    {_, _, Type0, _} ->
%%
%%      ?TRACE("The found usertype is ~p", [Type0]),
%%
%%%%      {A, B} = check_msg_type_valid(Type0, Types, Error, HasPid),
%%%%      ?TRACE("HasPid in user type? ~p", [A]),
%%
%%      check_msg_type_valid(Type0, Types, Analysis, HasPid);
%%    undefined ->
%%
%%      % TODO: maybe use an assert.
%%      % This case is handled by the Erlang preprocessor and should never happen.
%%      Analysis#analysis{result = HasPid}
%%%%      {HasPid, Analysis}
%%  end;
%%check_msg_type_valid(_Type = {type, _ANNO, union, MsgTypes}, Types = #{}, Analysis, HasPid) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  check_msgs_types_valid(MsgTypes, Types, Analysis, HasPid);
%%check_msg_type_valid(Term, _Types = #{}, Analysis, HasPid) ->
%%  ?ERROR("Type: ~p", [Term]),
%%  % Type not permitted.
%%  ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Term, Analysis).
%%%%  Analysis#analysis{result = HasPid}.
%%%%  {HasPid, Analysis}.


%%-spec check_msgs_types_valid(MsgTypes, Types, Analysis, HasPid) ->
%%  paterl_lib:analysis()
%%  when
%%  MsgTypes :: list(),
%%  Types :: map(),
%%  Analysis :: paterl_lib:analysis(),
%%  HasPid :: boolean().
%%check_msgs_types_valid([], _Types = #{}, Analysis, HasPid) ->
%%  Analysis#analysis{result = HasPid};
%%%%  {HasPid, Analysis};
%%check_msgs_types_valid([MsgType | MsgTypes], Types = #{}, Analysis, HasPid) ->
%%  Analysis0 = #analysis{result = HasPid0} =
%%    check_msg_type_valid(MsgType, Types, Analysis, HasPid),
%%  check_msgs_types_valid(MsgTypes, Types, Analysis0, HasPid0).
%%%%  {HasPid0, Error0} = check_msg_type_valid(Elem, Types, Analysis, HasPid),
%%%%  check_msgs_types_valid(Elems, Types, Error0, HasPid0).


%% Checks that the specified term is an atom.
-spec check_valid_tag(term(), paterl_lib:analysis()) -> paterl_lib:analysis().
check_valid_tag(_Tag = {atom, _, _}, Analysis) ->
  ?TRACE("Valid '~s' tag.", [erl_prettypr:format(_Tag)]),
  Analysis;
check_valid_tag(Term, Analysis) ->
  ?ERROR("Invalid '~s' tag.", [erl_prettypr:format(Term)]),
  ?pushError(?E_BAD__MSG_TAG, Term, Analysis).


%%-spec check_valid_tuple_elems(Elems, Types, Analysis) -> paterl_lib:analysis()
%%  when
%%  Elems :: list(),
%%  Types :: map(),
%%  Analysis :: paterl_lib:analysis().
%%check_valid_tuple_elems([], #{}, Analysis) ->
%%  Analysis;
%%check_valid_tuple_elems([{type, _ANNO, integer, _} | Elems], Types = #{}, Analysis) ->
%%  % Type is built-in.
%%  check_valid_tuple_elems(Elems, Types, Analysis);
%%check_valid_tuple_elems([Node = {user_type, _ANNO, Type, _} | Elems], Types = #{}, Analysis) ->
%%  % User defined-type name must refer to a mailbox type definition. Check that
%%  % the mailbox type is defined as a user-defined type in its turn.
%%  %% TODO: Check whether this consistency check that checks the types of
%%  %% individual tuple elements is required. It could be that the Erlang
%%  %% preprocessor also does this, in which case we can remove the map:is_key
%%  %% check.
%%
%%  % TODO: Checking that the type of the user-defined type in the tuple is not
%%  % TODO: enough: it may exist but be an atom, say. It must be a message, so
%%  % TODO: we must check the type of that mailbox type as well.
%%  % HOW: 1. get Type from Types, 2. Check the obtained type.
%%%%  check_msg_type_valid(Type, Types, Analysis, false)..maybe! See on monday!
%%%%  case maps:is_key(Type, Types) of
%%%%    true ->
%%%%      % User-defined mailbox type definition exists. Check rest.
%%%%
%%%%      check_valid_tuple_elems(Elems, Types, Analysis);
%%%%    false ->
%%%%      % Non-existent user-defined mailbox type definition. This never occurs
%%%%      % because the Erlang preprocessor ensures types are valid.
%%%%
%%%%      % TODO: This should never occur because the Erlang preprocessor ensures that types are valid.
%%%%      check_valid_tuple_elems(Elems, Types, ?pushError(?E_MB_TYPE_UNDEF, Node, Analysis))
%%%%  end;
%%
%%  case is_map_key(Type, Types) of
%%    true ->
%%      % User-defined mailbox interface type exists. Check rest of tuple
%%      % elements.
%%      ?DEBUG("User-defined mailbox interface type ~p found as type.", [Type]),
%%      ?DEBUG("Checking rest of elements"),
%%
%%      ?DEBUG("~n~n~n~nTypes table: ~p", [Types]),
%%      {?T_TYPE, _, TypeDef, _} = maps:get(Type, Types),
%%      ?DEBUG("User-defined message type is: ~p", [TypeDef]),
%%      Analysis0 = check_msg_type_valid(TypeDef, Types, Analysis, false),
%%      ?DEBUG("Answer is: ~p", [Analysis0]),
%%
%%      check_valid_tuple_elems(Elems, Types, Analysis0);
%%
%%    false ->
%%      error("Undefined types are ruled out by the preprocessor or linter; use epp or erl_lint"),
%%      check_valid_tuple_elems(Elems, Types, ?pushError(?E_MB_TYPE_UNDEF, Node, Analysis))
%%  end;
%%
%%check_valid_tuple_elems([Elem | Elems], Types = #{}, Analysis) ->
%%  Analysis0 = ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Elem, Analysis),
%%  check_valid_tuple_elems(Elems, Types, Analysis0).


%% @private Checks that function signatures have a corresponding function spec
%% defined.
%% returns `{ok, Warnings}` if all function signatures have a corresponding
%% spec defined, otherwise `{error, Errors, Warnings}`. Warnings is always the
%% empty list.
%%-spec check_sigs_have_specs(map(), map()) -> {ok, []} | {ok, term(), term()}.
%%check_fun_specs(FunSigs = #{}, FunSpecs = #{}) ->
%%  FunSpecCheckFun =
%%    fun(FunSig = {_, _}, {ANNO}, Analysis) ->
%%      if is_map_key(FunSig, FunSpecs) ->
%%        % Function signature has corresponding function spec.
%%        Analysis;
%%        true ->
%%          % Non-existent function spec.
%%          Node = to_erl_af(ANNO, FunSig),
%%          ?pushError(?E_UNDEF__FUN_SPEC, Node, Analysis)
%%      end
%%    end,
%%
%%  % Check that function signatures have a corresponding function spec defined.
%%  maps:fold(FunSpecCheckFun, #analysis{}, FunSigs).


%% Checks that the types that are mailbox specs include at least the pid. This
%% means that a mailbox type spec can be just a pid or a type union that
%% includes a pid.
%% Must check that the union consists of just tuples whose first element is an atom.
%% We also need to check that the elements of the tuple are either mailbox types
%% or primitive types.
%%check_mb_types_valid2(Types = #{}, MbSpecs = #{}) ->
%%
%%
%%  MbNames = maps:fold(fun({_, _}, {_, _, Mailbox}, Acc) -> Acc#{Mailbox => ok} end, #{}, MbSpecs),
%%  ?TRACE("MbNames = ~p", [MbNames]),
%%
%%  maps:fold(
%%    fun(Name, {_, Type, _Vars}, Error) ->
%%      ?TRACE("Name ~p, Type = ~p", [Name, Type]),
%%      case maps:is_key(Name, MbNames) of
%%        true ->
%%          ?TRACE("Checking mailbox type: ~p", [Name]),
%%
%%          % Type used in mailbox spec. Determine whether the type definition
%%          % includes the built-in pid(). There are three cases (i) the mailbox
%%          % type is a pid(), (ii) the mailbox type is type union that includes a
%%          % pid(), or (iii) the mailbox type does not include a pid(), in which
%%          % case issue a warning.
%%          case Type of
%%            {type, _, union, Elems} ->
%%
%%              ?TRACE("Elements: ~p returns: ~p", [Elems, lists:any(fun({{type, _, pid, _}}) -> true; (_) ->
%%                false end, Elems)]),
%%              % Mailbox type is a union that might or might not contain a pid().
%%
%%              case lists:any(fun({{type, _, pid, _}}) -> true; (_) -> false end, Elems) of
%%                true ->
%%                  % Not error.
%%                  ?TRACE("Contains PID"),
%%                  Error;
%%                false ->
%%                  ?TRACE("DOES NOT Contain PID"),
%%                  Error
%%              end;
%%
%%            {type, _, pid, []} ->
%%
%%              % Mailbox type is a pid().
%%              Error;
%%            Other ->
%%              % Mailbox type is a product, which is an invalid definition.
%%              % Product = incorrectly defined mailbox type.
%%
%%              % Mailbox type is a singleton which is not a pid().
%%              ?TRACE("Other = ~p", [Other]),
%%
%%              %WARN
%%              ?TRACE("DOES NOT Contain PID"),
%%              Error
%%          end;
%%        false ->
%%
%%          % Type is not used in mailbox spec.
%%          Error
%%      end
%%    end,
%%    #error{}, Types).


%% Checks that the specified term is a valid mailbox name.
%%check_is_mb_name(Name) when is_atom(Name) ->
%%  ok;
%%check_is_mb_name(_) ->
%%  {error, error_tbd}.

%%check_is_sig({Fun, Arity}) when is_atom(Fun), is_integer(Arity) ->
%%  ok;
%%check_is_sig(_) ->
%%  {error, error_tbd}.

% Duplicate use.
% Non existing signature.
% Invalid signature.
%% Checks that functions are associated with at most one mailbox modality.
%% The function signature is validated by the Erlang preprocessor.
%%check_mb_sig_redef(MbSigs = #{}) ->
%%  ok.

%% Checks that all typespecs that are mailbox types must also have the built-in
%% type pid() declared. This makes the mailbox types correct w.r.t Dialyzer.
%% Empty mailbox typespec definitions consisting of the singleton type pid()
%% are allowed.
%%check_mb_has_pid(TSpecs = #{}, MbSigs = #{}) ->
%%  maps:fold(
%%    fun(Key, {_, Type}, AccIn) ->
%%%%      case maps:is_key(Key, )
%%
%%      % Type can be a singleton or a union.
%%%%      case erl_syntax:type(Type) of
%%%%        type_union ->
%%%%          ok;
%%%%        type_application ->
%%%%          case erl_syntax:concrete(erl_syntax:type_application_name(Type)) =/= pid of
%%%%
%%%%          end;
%%%%      end,
%%
%%
%%
%%      io:format("Key: ~p, Type: ~p~n", [Key, Type]),
%%      AccIn
%%    end,
%%    #error{}, TSpecs).


%% TODO: After this we need to visit the tree and decorate it with annotations in preparation for the translation function on paper.
%% TODO: 1. Collect type specs.
%% TODO: 2. Collect mailbox function definition.
%% TODO: 3. Collect fun specs.

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
format_error({?E_BAD__MSG_ELEM_TYPE, Node}) -> %TODO: See if this is used. It is but remove it and use ?E_BAD__MB_TYPE instead.
  io_lib:format(
    "bad message element type '~s'",
    [erl_prettypr:format(Node)]
  );
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
format_error({?E_BAD__MB_TYPE, Node}) ->
  io_lib:format(
    "bad type '~s' in message type; use a built-in or mailbox interface type",
    [erl_prettypr:format(Node)]
  );
format_error({?E__UNDEF_TYPE, Node}) ->
  io_lib:format(
    "undefined type '~s'",
    [erl_prettypr:format(Node)]
  ).


%%REARRANGE check_msgs_types_valid2 to use list
%%
%%REFACTOR WITH NON anonymous functions

%% REFACTOR WITH maybe

% TODO: Move to paterl syntax.
to_erl_af(ANNO, Name) when is_atom(Name) ->
  revert(set_pos(atom(Name), ANNO));
to_erl_af(ANNO, {Name, Arity})
  when is_atom(Name), is_integer(Arity) ->
  revert(set_pos(implicit_fun(atom(Name), integer(Arity)), ANNO)).







