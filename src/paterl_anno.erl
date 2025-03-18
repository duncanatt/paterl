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
-module(paterl_anno).
-moduledoc "Erlang abstract syntax representation annotation.".
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl"). % TODO: Eliminate this.
-include("log.hrl").
-include("paterl_lib.hrl").
-include("paterl_syntax.hrl").

%%% Public API.
-export([module/3]).

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% TODO: To be used for multiple mailboxes.
-define(call_info, {
  call_graph = #{},
  mb_scopes = []
}).

%%% Mailbox annotation types.

%% Mailbox annotation primitive type.
-define(MA_TYPE, type).

%% Mailbox annotation interface.
-define(MA_INTERFACE, interface).

-define(MA_INTERFACES, interfaces).

-define(MA_SCOPE, scope).

-define(MA_SCOPES, scopes).

%% Mailbox annotation regular expression state.
-define(MA_PATTERN, state).


%% Mailbox annotation capability.
%%-define(MA_READ, read).

%% Mailbox annotation interface usage modality.
%%-define(MA_MODALITY, modality).


%%% Error types.

%% Expected mailbox-annotated expression.
-define(E_EXP__EXPR, e_exp__expr).

%% Expected mailbox annotation.
-define(E_EXP__ANNO, e_exp__anno).

%% Unexpected mailbox annotation before expression.
-define(E_BAD__ANNO_ON, e_bad__anno_on).

%% Unsupported expression.
-define(E_BAD__EXPR, e_bad__expr).

%% Unsupported function clause.
-define(E_BAD__CLAUSE, e_bad__clause).

%% Expression not in mailbox interface scope.
-define(E_NO__MB_SCOPE, e_no__mb_scope).

%% Mailbox interface not in scope.
-define(E_UNDEF__MB_SCOPE, e_undef__mb_scope).

%% Undefined mailbox interface name.
-define(E_UNDEF__MB, e_undef__mb).

%% Undefined fun reference.
-define(E_UNDEF__FUN_REF, e_undef__fun_ref).

%% Function omits corresponding spec.
-define(E_UNDEF__FUN_SPEC, e_undef__fun_spec).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type ext_anno() :: {'file', file:filename_all()}
| {'generated', boolean()}
| {'location', erl_anno:location()}
| {'record', boolean()}
| {'text', string()}
| erl_anno:location()
| {?MA_TYPE, paterl_syntax:type()}
| {?MA_INTERFACE, interface()}
| {?MA_INTERFACES, [interface()]}
| {?MA_SCOPE, MbName :: paterl_syntax:name()}
| {?MA_SCOPES, [MbName :: paterl_syntax:name()]}
| {?MA_PATTERN, Pattern :: string()}.

-doc "Mailbox interface.".
-type interface() :: {Modality :: paterl_types:modality(), MbName :: paterl_syntax:name()}.

-doc "Return result.".
-type result() :: {ok, Forms :: paterl_syntax:forms(), Warnings :: paterl_errors:warnings()} |
{error, Errors :: paterl_errors:errors(), Warnings :: paterl_errors:warnings()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Analyzes the list of `Forms` with the specified type information.

Note that the type information, [`type_info()`](`t:paterl_types:type_info/0`), contains
`m:erl_parse` terms, rather than `m:erl_syntax`terms.

Form type annotation can fail with the following errors.

| Code                | Error                                                   |
| ------------------- | ------------------------------------------------------- |
| `e_exp__expr`       | expected mailbox annotated expression                   |
| `e_exp__anno`       | expected mailbox annotation                             |
| `e_bad__anno_on`    | unexpected mailbox annotation before expression         |
| `e_bad__expr`       | unsupported expression                                  |
| `e_bad__clause`     | unsupported function clause                             |
| `e_no__mb_scope`    | expression is not in mailbox interface scope            |
| `e_undef__mb_scope` | mailbox interface not in scope                          |
| `e_undef__mb`       | undefined mailbox interface name                        |
| `e_undef__fun_ref`  | undefined `fun` reference                               |
| `e_undef__fun_spec` | function omits corresponding spec                       |

Form annotation tries to report all possible errors, rather than stopping at the
first error it encounters.

See also `format_error/1`.

### Returns
- `{ok, AnnotatedFroms, Warnings}` if type annotation is successful
- `{error, Errors, Warnings}` otherwise
""".
-spec module(Forms, RecFunInfo, TypeInfo) -> Result
  when
  Forms :: paterl_syntax:forms(),
  RecFunInfo :: paterl_call_graph:rec_fun_info(),
  TypeInfo :: paterl_types:type_info(),
  Result :: result().
module(Forms, RecFunInfo = #{}, TypeInfo = #type_info{}) when is_list(Forms) ->
  % Return annotated Erlang AST with as result.
  Analysis = annotate_forms(Forms, RecFunInfo, TypeInfo, #analysis{}),
  paterl_lib:return(Analysis#analysis{file = paterl_syntax:get_file(Forms)}).


%%% ----------------------------------------------------------------------------
%%% Interfaces.
%%% ----------------------------------------------------------------------------

-doc """
Creates the list of mailbox interface types.

Nested message type definition references are recursively expanded in place.
Note that currently, mutually-recursive type defintions result in an infinite
loop. Will be fixed later.

### Returns
- list of [`form()`](`t:paterl_syntax:forms/0`).
""".
get_interfaces(TypeInfo) ->

  % Get type definition names.
  Names = paterl_types:type_defs(TypeInfo),
  ?TRACE("Got type defintion names ~p.", [Names]),

  % TODO: Document here, rest of the function, and delete the get_interfaces function.
  % Creates interface attributes from type definitions that are used as mailbox
  % interface names, omitting non-mailbox interface type definitions otherwise.
  Fun =
    fun(Name, Attrs) ->
      case paterl_types:type_def(Name, TypeInfo) of
        {?T_MBOX, Anno, _, Vars = []} ->
          % Type definition attribute used as mailbox interface.
          ?TRACE("Create interface for mailbox from type '~s'.", [Name]),

          MsgType =
            case expand_msg_type_set(Name, TypeInfo) of
              [Type] ->
                % Mailbox interface type definition is a singleton set.
                Type;
              Types when is_list(Types) ->
                % Mailbox interface type definition is a list that must be
                % expanded and recombined as a type union.
                paterl_syntax:set_anno(erl_syntax:type_union(Types), Anno)
            end,

          % Create mailbox interface attribute.
          ?TRACE("Expanded mailbox interface type '~s() :: ~s'.", [
            Name, erl_prettypr:format(MsgType)
          ]),
          [interface(paterl_syntax:name(Name, Anno), MsgType, Anno) | Attrs];
        {?T_TYPE, _, _, _Vars = []} ->
          % Eat other type definition attributes.
          ?TRACE("Skip non-mailbox interface type '~s'.", [Name]),
          Attrs
      end
    end,

  % Create mailbox interface attribute definitions.
  lists:reverse(lists:foldl(Fun, [], Names)).

-doc """
Creates a mailbox interface defintion wild attribute abstract syntax
representation with the specified [annotation](`m:erl_anno`).

### Returns
- mailbox interface definition wild attribute abstract syntax representation
""".
-spec interface(Name, MsgType, Anno) -> Tree
  when
  Name :: paterl_syntax:tree(),
  MsgType :: paterl_syntax:type(),
  Anno :: paterl_syntax:anno(),
  Tree :: paterl_syntax:tree().
interface(Name, MsgType, Anno) ->
  InterfaceDef = erl_syntax:tuple([
    Name,
    erl_syntax:abstract(MsgType), % Lifted message type to abstract syntax.
    erl_syntax:abstract([]) % Lifted type variables to abstract syntax.
  ]),
  paterl_syntax:set_anno(
    erl_syntax:attribute(erl_syntax:atom(interface), [InterfaceDef]), Anno
  ).

-doc """
Expands message types to their full tuple definition form.

### Returns
List of [`type()`](`t:paterl_syntax:type/0`).
""".
-spec expand_msg_type_set(Name, TypeInfo) -> Types
  when
  Name :: paterl_syntax:name(),
  TypeInfo :: paterl_types:type_info(),
  Types :: [paterl_syntax:type()].
expand_msg_type_set(Name, TypeInfo) ->
  % Mailbox message set type. Type variables not supported.
  {?T_MBOX, _, Type, _Vars = []} = paterl_types:type_def(Name, TypeInfo),
  lists:reverse(expand_msg_type(Type, TypeInfo, [])).

-doc """
Expands a message type to its full tuple defintion form.

### Returns
List of [`type()`](`t:paterl_syntax:type/0`).
""".
-spec expand_msg_type(Type, TypeInfo, Acc) -> Types
  when
  Type :: paterl_syntax:type(),
  TypeInfo :: paterl_types:type_info(),
  Acc :: [paterl_syntax:type()],
  Types :: [paterl_syntax:type()].
expand_msg_type(Type = {type, _, pid, _Vars = []}, _TypeInfo, Acc) ->
  % Built-in pid type. Denotes the empty message set.
  ?assertEqual(erl_syntax:type(Type), type_application),
  ?TRACE("Found type '~s'.", [erl_prettypr:format(Type)]),
  [Type | Acc];
expand_msg_type(Type = {type, _, tuple, _}, _TypeInfo, Acc) ->
  % Tuple type. Denotes the singleton message set.
  ?assertEqual(erl_syntax:type(Type), tuple_type),
  ?TRACE("Found type '~s'.", [erl_prettypr:format(Type)]),
  [Type | Acc];
expand_msg_type(_Type = {type, _, union, Union}, TypeInfo, Acc) ->
  % User-defined type union. Denotes a non-trivial message set.
  ?assertEqual(erl_syntax:type(_Type), type_union),
  ?TRACE("Inspect type '~s'.", [erl_prettypr:format(_Type)]),
  lists:foldl(
    fun(Type0, Acc0) ->
      expand_msg_type(Type0, TypeInfo, Acc0)
    end,
    Acc, Union);
expand_msg_type(_Type = {user_type, _, Name, _Vars = []}, TypeInfo, Acc) ->
  % User-defined type. Denotes another message type which is a message set
  % alias.
  ?TRACE("Inspect type '~s'.", [erl_prettypr:format(_Type)]),
  {?T_TYPE, _, UserType, _} = paterl_types:type_def(Name, TypeInfo),
  expand_msg_type(UserType, TypeInfo, Acc).


%%% ----------------------------------------------------------------------------
%%% Annotation.
%%% ----------------------------------------------------------------------------

-doc """
Annotates Erlang forms.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis) -> Analysis0
  when
  Forms :: paterl_syntax:forms(),
  RecFunInfo :: paterl_call_graph:rec_fun_info(),
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_forms([], _, _, Analysis) ->
  % Empty forms.
  Analysis#analysis{result = []};
annotate_forms([Form = {attribute, _, module, _} | Forms], RecFunInfo, TypeInfo, Analysis) ->
  % Module attribute. Expand interfaces after it.
  Interfaces = get_interfaces(TypeInfo),
  Analysis0 = annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Interfaces ++ Analysis0#analysis.result]};
annotate_forms([Form = {attribute, _, export, _} | Forms], RecFunInfo, TypeInfo, Analysis) ->
  % Export attribute. Leave in so that Erlang compiler compiles successfully.
  Analysis0 = annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Analysis0#analysis.result]};
annotate_forms([Form = {attribute, _, import, _} | Forms], RecFunInfo, TypeInfo, Analysis) ->
  % Import attribute. Leave in so that Erlang compiler compiles successfully.
  Analysis0 = annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Analysis0#analysis.result]};
annotate_forms([{attribute, _, _, _} | Forms], RecFunInfo, TypeInfo, Analysis) ->
  % Eat other attributes.
  annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis);
annotate_forms([Form = {function, _, _, _, _} | Forms], RecFunInfo, TypeInfo, Analysis) ->
  % Function.
  Analysis0 = annotate_function(Form, RecFunInfo, TypeInfo, Analysis),
  Analysis1 = annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis0),
  Analysis1#analysis{
    result = [Analysis0#analysis.result | Analysis1#analysis.result]
  };
annotate_forms([_ | Forms], RecFunInfo, TypeInfo, Analysis) ->
  % Eat other forms.
  annotate_forms(Forms, RecFunInfo, TypeInfo, Analysis).

-doc """
Annotates unguarded functions with a single clause.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_function(Form, RecFunInfo, TypeInfo, Analysis) -> Analysis0
  when
  Form :: paterl_syntax:form(),
  RecFunInfo :: paterl_call_graph:rec_fun_info(),
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_function({function, Anno, Name, Arity, Clauses}, RecFunInfo, TypeInfo, Analysis) ->
  % Recover fun reference.
  FunRef = {Name, Arity},

  % Initialize fun references that are directly or indirectly call this function
  % recursively. Tracks the functions that should be annotated with a ?use
  % modality to reuse the same mailbox. The ?use modality overrides a possibly
  % -new modality defined by the user for the specific function under
  % consideration.
  RecFuns = maps:get(FunRef, RecFunInfo),

  maybe
  % Only one function clause, and therefore, one spec is assumed.
    {?T_SPEC, _, Types} ?= paterl_types:spec_def(FunRef, TypeInfo),
    ?assert(length(Clauses) == 1 andalso length(Types) == 1),

    % Determine if function has an associated mailbox scope.
    case paterl_types:mb_fun(FunRef, TypeInfo) of
%%      {MbMod, _, MbName} ->
      Mbs when is_list(Mbs) ->
        % Function inside mailbox scope (at least one mailbox interface).
        % Initialize mailbox scopes. Tracks accessible mailbox interface names.
%%        MbScopes = [{MbMod, MbName} || {MbMod, _, MbName} <- Mbs],
%%        MbScopes = [MbName || {_, _, MbName} <- Mbs],
        MbScopes = paterl_types:mb_names(Mbs),

        ?DEBUG("Annotate function '~s' inside mailbox scopes '~w'.", [
          erl_prettypr:format(paterl_syntax:fun_reference(FunRef, Anno)),
          MbScopes
        ]),

        % Annotate function clauses.
        Analysis1 = annotate_fun_clauses(Clauses, Types, RecFuns, MbScopes, TypeInfo, Analysis),

%%        Anno1 = set_modality(MbMod, set_interfaces(MbScopes, Anno)),
%%        Interfaces = [{MbMod, MbName} || {MbMod, _, MbName} <- Mbs],
        Anno0 = set_interfaces(extract_interfaces(Mbs), Anno),

        Form0 = map_anno(fun(_) ->
          Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Analysis1#analysis.result))),
        Analysis1#analysis{result = Form0};

      undefined_mb ->
        % Function outside mailbox scope (no mailbox interface).
        ?DEBUG("Annotate function '~s' outside mailbox scope.", [
          erl_prettypr:format(paterl_syntax:fun_reference(FunRef, Anno))
        ]),

        % No mailbox interface names to track.
        MbScopes = undefined,

        % Annotate function clauses.
        Analysis1 = annotate_fun_clauses(Clauses, Types, RecFuns, MbScopes, TypeInfo, Analysis),
        Form0 = erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Analysis1#analysis.result)),
        Analysis1#analysis{result = Form0}
    end
  else
    undefined_spec ->
      % Non-existent function spec. Should not happen because every fun
      % reference must have a corresponding spec, checked in earlier passes.
      ErrNode = paterl_syntax:fun_reference(FunRef, Anno),

      ?ERROR("Function '~s' has missing spec.", [erl_prettypr:format(ErrNode)]),
      ?pushError(?E_UNDEF__FUN_SPEC, ErrNode, Analysis)
  end.

-doc """
Annotates a function clause list.

See `annotate_fun_clause/6` for details on supported clauses.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_fun_clauses(Clauses, Types, RecFuns, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clauses :: [paterl_syntax:clause()],
  Types :: [paterl_syntax:type()],
  RecFuns :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_fun_clauses([], [], _, _, _, Analysis) ->
  Analysis#analysis{result = []};
annotate_fun_clauses([Clause | Clauses], [Type | Types], RecFuns, MbScopes, TypeInfo, Analysis) ->
  Analysis0 = annotate_fun_clause(Clause, Type, RecFuns, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_fun_clauses(Clauses, Types, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result =
  [Analysis0#analysis.result | Analysis1#analysis.result]
  }.

-doc """
Annotates a function clause.

The following clauses are annotated:
- Unguarded function clause

The following clauses are unsupported:
- Guarded function clause

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_fun_clause(Clause, Type, RecFuns, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clause :: paterl_syntax:clause(),
  Type :: paterl_syntax:type(),
  RecFuns :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body},
    _ArgType = {type, _, 'fun', [{type, _, product, TypeSeq}, RetType]}, RecFuns, MbScopes, TypeInfo, Analysis)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq),
  is_list(Body) ->
  % Unguarded function clause.
  ?DEBUG("Annotate unguarded function clause '~s :: ~s'.", [
    erl_prettypr:format(erl_syntax:clause(PatSeq, _GuardSeq, [])),
    erl_prettypr:format(RetType)
  ]),

  % Annotate function pattern sequence and set function return type.
  AnnPatSeq = annotate_pat_seq(PatSeq, TypeSeq),
  Anno0 = set_type(RetType, Anno),

  % Set mailbox interface if function is inside mailbox scope.
%%  TODO: Is this actually needed?
  Anno1 =
    if
      MbScopes =:= undefined -> Anno0; true ->
%%      set_interfaces(hd(MbScopes), Anno0) % TODO: This hd() would be removed when we tackle multiple mailboxes
      set_interfaces(MbScopes, Anno0) % TODO: This hd() would be removed when we tackle multiple mailboxes
%%      Anno0
    end,

  % Annotate function body.
  Analysis0 = annotate_expr_seq(Body, RecFuns, MbScopes, TypeInfo, Analysis),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(AnnPatSeq, _GuardSeq, Analysis0#analysis.result), Anno1
  ),
  Analysis0#analysis{result = Clause0};
annotate_fun_clause({clause, Anno, PatSeq, GuardSeq, _}, _ArgType, _RecFuns, _MbScopes, _, Analysis) ->
  % Guarded function clause. Unsupported.
  ErrNode = paterl_syntax:set_anno(erl_syntax:clause(PatSeq, GuardSeq, []), Anno),
  ?ERROR("Unsupported guarded function clause '~s'.", [
    erl_prettypr:format(ErrNode)
  ]),
  ?pushError(?E_BAD__CLAUSE, ErrNode, Analysis#analysis{result = ErrNode}).

-doc """
Annotates a non-function clause list.

See `annotate_clause/5` for details on supported clauses.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clauses :: [paterl_syntax:clause()],
  RecFuns :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_clauses([], _, _, _, Analysis) ->
  Analysis#analysis{result = []};
annotate_clauses([Clause | Clauses], RecFuns, MbScopes, TypeInfo, Analysis) ->
  Analysis0 = annotate_clause(Clause, RecFuns, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result =
  [Analysis0#analysis.result | Analysis1#analysis.result]
  }.

-doc """
Annotates a non-function clause.

The following clauses are annotated:
- `if`
- `receive`

The following clauses are skipped:
- `case`
- guarded `case`
- guarded `receive`
- `catch`
- `maybe`

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_clause(Clause, RecFuns, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clause :: paterl_syntax:clause(),
  RecFuns :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    RecFuns, MbScopes, TypeInfo, Analysis)
  when is_list(GuardSeq), is_list(Body) ->
  % If clause.
  ?TRACE("Annotate clause '~s'.", [erl_syntax:type(Clause)]),

  Analysis0 = annotate_expr_seq(Body, RecFuns, MbScopes, TypeInfo, Analysis),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(GuardSeq, Analysis0#analysis.result), Anno
  ),
  Analysis0#analysis{result = Clause0};
annotate_clause(Clause = {clause, Anno, PatSeq = [_], GuardSeq = [], Body}, RecFuns, MbScopes, TypeInfo, Analysis) ->
  % Receive or case clause
  ?TRACE("Annotate clause '~s'.", [erl_syntax:type(Clause)]),

  Analysis0 = annotate_expr_seq(Body, RecFuns, MbScopes, TypeInfo, Analysis),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(PatSeq, GuardSeq, Analysis0#analysis.result), Anno
  ),
  Analysis0#analysis{result = Clause0};
annotate_clause(Clause, _RecFuns, _MbScopes, _, Analysis) ->
  ?TRACE("Skip clause '~s'.", [erl_syntax:type(Clause)]),
  Analysis.

-doc """
Annotates a pattern sequence.

### Returns
List of [`pattern()`](`t:paterl_syntax:pattern/0`).
""".
-spec annotate_pat_seq(PatSeq, TypeSeq) -> PatSeq0
  when
  PatSeq :: [paterl_syntax:pattern()],
  TypeSeq :: [paterl_syntax:type()],
  PatSeq0 :: [paterl_syntax:pattern()].
annotate_pat_seq(PatSeq, TypeSeq)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq) ->
  lists:zipwith(fun(Pat, Type) -> annotate_pat(Pat, Type) end, PatSeq, TypeSeq).
annotate_pat(Pat, Type = {Qualifier, _, _, _})
  when Qualifier == type; Qualifier == user_type ->
  ?TRACE("Annotate function pattern '~s :: ~s'.", [
    erl_prettypr:format(Pat), erl_prettypr:format(Type)
  ]),
  map_anno(fun(Anno) -> set_type(Type, Anno) end, Pat).
%% HERE tyring to get rid of map_anno. I don't think it can be done at this
%% stage.

-doc """
Annotates an expression sequence.

The function takes into account the mailbox annotation expressions, which are
expressed as macros in the surface-level syntax. These macros, `?new`, `?use`,
`?as`, and `?expects`, decorate specific expressions, and are represented as
special tuples internally. Each tuple carries macro-specific details.

Errors are generated whenever these macros are wrongly used. These conditions
apply:
1. A mailbox annotation expression at the end of an expression sequence is
   invalid
2. Two successive mailbox annotation expressions are invalid

In the rest of the cases, a mailbox annotation expressions is eaten by the
function, which it uses as a lookahead expression to annotate the next
expression in the sequence.

See `annotate_expr/6` for details on supported expressions.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_expr_seq(Exprs, RecFuns, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Exprs :: [paterl_syntax:expr()],
  RecFuns :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_expr_seq([], _, _, _, Analysis) ->
  % Empty expression sequence.
  Analysis#analysis{result = []};
annotate_expr_seq([MbAnno], _, _, _, Analysis) when ?isMbAnno(MbAnno) ->
  % Annotation at end of expression list. Invalid.
  Name = paterl_syntax:mb_anno_name(MbAnno),
  Args = paterl_syntax:mb_anno_args(MbAnno),
  Anno = erl_syntax:get_pos(MbAnno),

  ErrNode = paterl_syntax:mb_anno(Name, Args, Anno),
  ?ERROR("Expected expression after '~s'.", [erl_prettypr:format(ErrNode)]),
  ?pushError(?E_EXP__EXPR, ErrNode, Analysis#analysis{result = []});
annotate_expr_seq([MbAnno0, MbAnno1 | ExprSeq], RecFuns, MbScopes, TypeInfo, Analysis) when ?isMbAnno(MbAnno0), ?isMbAnno(MbAnno1) ->
  % Two successive annotations. Invalid.
  Name = paterl_syntax:mb_anno_name(MbAnno0),
  Args = paterl_syntax:mb_anno_args(MbAnno0),
  Anno = erl_syntax:get_pos(MbAnno0),

  ErrNode = paterl_syntax:mb_anno(Name, Args, Anno),
  ?ERROR("Expected expression after '~s'.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_EXP__EXPR, ErrNode, Analysis),

  % Annotate rest of expressions and uncover further possible errors.
  annotate_expr_seq(ExprSeq, RecFuns, MbScopes, TypeInfo, Analysis0);
annotate_expr_seq([MbAnno, Expr | ExprSeq], RecFuns, MbScopes, TypeInfo, Analysis) when ?isMbAnno(MbAnno) ->
  % Annotated expression. May be valid.
  Name = paterl_syntax:mb_anno_name(MbAnno),
  Args = paterl_syntax:mb_anno_args(MbAnno),
  Anno = erl_syntax:get_pos(MbAnno),

  ?TRACE("'~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(Name, Args, Anno)),
    erl_syntax:type(Expr)
  ]),

  MbAnno0 = list_to_tuple([Name | Args]), %TODO: Include annotation anno() as well for error reporting purposes.
  ?TRACE("MbAnno0 = ~p", [MbAnno0]),

  % Annotate expression and rest of expressions.
  Analysis0 = annotate_expr(Expr, MbAnno0, RecFuns, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_expr_seq(ExprSeq, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = [
    Analysis0#analysis.result | Analysis1#analysis.result
  ]};
annotate_expr_seq([Expr | ExprSeq], RecFuns, MbScopes, TypeInfo, Analysis) ->
  % Unannotated expression. May be valid.
  Analysis0 = annotate_expr(Expr, undefined, RecFuns, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_expr_seq(ExprSeq, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = [
    Analysis0#analysis.result | Analysis1#analysis.result
  ]}.

-doc """
Annotates an expression.

Errors are generated whenever mailbox annotations are missing when expected or
vice versa. These conditions apply:
1. A `spawn` expression must be unannotated
2. A `self` expression may be unannotated when its enclosing mailbox interface
  scope comprises one interface, in which case the mailbox interface is inferred
3. A `self` expression must be annotated when its enclosing mailbox scope
  comprises multiple mailboxes
4. A static function call must be unannotated
5. A `receive` expression must be annotated
6. An `if` expression must be unannotated
7. A `match` expression must be annotated whenever its RHS expression is 3 or 5
  and unannotated otherwise

The following expressions are annotated:
- `spawn` call
- `self` call
- Internal static function call
- `if` with two clauses
- `match`
- Unguarded `receive` without `after`

The following expressions are skipped:
- Atomic literal
- Bitstring comprehension
- Bitstring constructor
- Block
- `case`
- `catch`
- Cons skeleton
- Internal `fun`
- External `fun`
- Anonymous `fun`
- Named `fun`
- External function call
- List comprehension
- Map comprehension
- Map creation
- Map update
- Conditional match operator
- `maybe`
- `maybe` `else`
- Nil
- Binary operator
- Unary operator
- Parenthesized
- `receive` `after`
- Record creation
- Record field access
- Record field index
- Record update
- Tuple skeleton
- `try` `catch`
- `try` `of` `catch`
- `try` `after`
- `try` `of` `after`
- `try` `catch` `after`
- `try` `of` `catch` `after`
- Variable

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_expr(Expr, MbAnno, RecFuns, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Expr :: paterl_syntax:expr(),
  MbAnno :: paterl_syntax:mb_anno(),
  RecFuns :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_expr(Expr = {call, Anno, Operator = {atom, _, spawn}, MFArgs}, _MbAnno = undefined, _RecFuns, _MbScopes, TypeInfo, Analysis) ->
  % Unannotated spawn expression. MFArgs can be a static or dynamic function.
  % Static functions in spawn expressions must be unannotated, whereas dynamic
  % functions must be annotated. The latter are currently unsupported.
  case paterl_syntax:get_fun_ref_mfa(MFArgs) of
    {ok, _, FunRef = {_, _}} ->
      % MFArgs is a static function. May be valid.
      ?DEBUG("Annotate '~s'.", [erl_prettypr:format(Expr)]),

      maybe
      % Check fun reference is defined.
        {?T_SPEC, _, _} ?= paterl_types:spec_def(FunRef, TypeInfo),

        % Check that mailbox interface is defined.
%%        {_MbMod, _Anno, MbName} ?= paterl_types:mb_fun(FunRef, TypeInfo),
        Mbs = [_ | _] ?= paterl_types:mb_fun(FunRef, TypeInfo),


        % Override modality with ?new for spawn call.
        ?DEBUG("Override '~s' with '~s' on '~s'.", [

          lists:foldr(
            fun({MbMod, Anno, MbName}, Acc) ->
              [Acc | erl_prettypr:format(paterl_syntax:mb_anno(MbMod, [MbName], Anno))]
            end, "", Mbs
          ),

          lists:foldr(
            fun({_, Anno, MbName}, Acc) ->
              [Acc | erl_prettypr:format(paterl_syntax:mb_anno(?MOD_NEW, [MbName], Anno))]
            end, "", Mbs
          ),
%%          erl_prettypr:format(paterl_syntax:mb_anno(_MbMod, [MbName], _Anno)),
%%          erl_prettypr:format(paterl_syntax:mb_anno(?MOD_NEW, [MbName], Anno)),
          erl_prettypr:format(Expr)
        ]),
%%        Anno0 = set_modality(?MOD_NEW, set_interfaces(MbName, Anno)),
%%        Anno0 = set_interfaces([{?MOD_NEW, Anno, MbName} || {_, Anno, MbName} <- Mbs], Anno),
        Anno0 = set_interfaces(override_interfaces(?MOD_NEW, Mbs), Anno),

        Expr0 = paterl_syntax:set_anno(
          erl_syntax:application(Operator, MFArgs), Anno0
        ),
        Analysis#analysis{result = Expr0}
      else
        undefined_spec ->
          % Undefined fun reference.
          ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
          ?ERROR("Undefined function '~s'.", [erl_prettypr:format(ErrNode)]),
          ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis#analysis{result = ErrNode});
        undefined_mb ->
          % Undefined mailbox interface.
          ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
          ?ERROR("Undefined mailbox interface for '~s'.", [
            erl_prettypr:format(ErrNode)
          ]),
          ?pushError(?E_UNDEF__MB, ErrNode, Analysis#analysis{result = ErrNode})
      end;
    {error, Term} ->
      % MFArgs is a dynamic function. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Term), erl_prettypr:format(Expr)
      ]),
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Term})
  end;
annotate_expr(Expr = {call, Anno, {atom, _, OpName = spawn}, _MFArgs}, MbAnno, _RecFuns, _MbScopes, _, Analysis) ->
  % Annotated spawn expression. Invalid.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(MbAnno)),
    erl_prettypr:format(Expr)
  ]),
  ErrNode = paterl_syntax:application(OpName, [], Anno),
  ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis#analysis{result = ErrNode});

annotate_expr(Expr = {call, _, {atom, _, self}, []}, _MbAnno, _RecFuns, _MbScopes = undefined, _, Analysis) ->
  % Annotated self expression outside mailbox scope. Invalid.
  ?ERROR("'~s' not in mailbox interface scope.", [erl_prettypr:format(Expr)]),
  ?pushError(?E_NO__MB_SCOPE, Expr, Analysis#analysis{result = Expr});
annotate_expr(Expr = {call, _, _Self = {atom, _, self}, []}, _MbAnno = undefined, RecFuns, MbScopes = [MbScope], TypeInfo, Analysis) ->
  % Unannotated self expression inside mailbox interface scope with single
  % interface. Valid, since mailbox interface can be inferred from enclosing
  % mailbox scope.
  ?TRACE("Annotate '~s' with implicit mailbox interface '~s'.", [
    erl_prettypr:format(Expr), MbScope
  ]),
  MbAnno0 = {?ANNO_AS, MbScope},
  annotate_expr(Expr, MbAnno0, RecFuns, MbScopes, TypeInfo, Analysis);
annotate_expr(Expr = {call, Anno, Self = {atom, _, self}, []}, {?ANNO_AS, MbName}, _RecFuns, MbScopes, _, Analysis) ->
  % Annotated self expression inside mailbox scope with multiple interfaces.
  % May be valid. Mailbox interface inferred from the enclosing mailbox scope
  % must match the mailbox interface in the annotation.
  case is_mb_in_scope(MbName, MbScopes) of
    true ->
      % Mailbox interface name in scope.
      ?TRACE("Annotate '~s' with inferred matching mailbox interface '~s'.", [
        erl_prettypr:format(Expr), MbName
      ]),
%%      Anno0 = set_interfaces([MbName], Anno), %TODO: Should we
      Anno0 = set_scope(MbName, Anno),

      Expr0 = paterl_syntax:set_anno(erl_syntax:application(Self, []), Anno0),
      Analysis#analysis{result = Expr0};
    false ->
      % Mailbox interface name out of scope. Invalid.
      ?ERROR("Mailbox interface '~s' not in scope.", [MbName]),
      ?pushError(?E_UNDEF__MB_SCOPE, paterl_syntax:name(MbName, Anno), Analysis#analysis{result = Expr})
  end;
annotate_expr(Expr = {call, _, {atom, _, self}, []}, _MbAnno, _RecFuns, _MbScopes, _, Analysis) ->
  % Annotated self expression with invalid annotation (i.e., not ?as). Invalid.
  ?ERROR("Unexpected '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)), %TODO: Here we can benefit from storing the anno() of the mailbox annotation.
    erl_prettypr:format(Expr)
  ]),
  ?pushError(?E_BAD__ANNO_ON, Expr, Analysis#analysis{result = Expr});

annotate_expr(Expr = {call, Anno, Operator, Exprs}, _MbAnno = undefined, RecFuns, MbScopes, TypeInfo, Analysis) ->
  ?TRACE("--> Unannotated local function call mbscopes = ~w", [MbScopes]),
  % Unannotated local function call.
  case paterl_syntax:get_fun_ref(Expr) of
    {ok, FunRef = {_, _}} ->
      % Static function call. Check if fun reference is defined.
      {Anno0, Analysis0} =
        case paterl_types:spec_def(FunRef, TypeInfo) of
          {spec, _, _} ->
            % Check if mailbox interface is defined.
            case paterl_types:mb_fun(FunRef, TypeInfo) of
%%              {MbMod, _Anno, MbName} ->
              Mbs when is_list(Mbs) ->
                % Called function inside mailbox interface context.
                % Check if recursive function call. TODO properly later
                case is_rec_fun_ref(FunRef, RecFuns) of
                  true ->
                    % Recursive call. Override usage modality with ?use.
                    ?DEBUG("Annotate recursive '~s' with inferred mailbox interfaces '~w'.", [
                      erl_prettypr:format(Expr), Mbs
                    ]),
%%                    {set_modality(?MOD_USE, set_interfaces(MbName, Anno)), Analysis};
%%                    {override_interfaces(?MOD_USE, )}
                    {set_interfaces(override_interfaces(?MOD_USE, Mbs), Anno), Analysis};
                  false ->
                    % Non-recursive call.
                    ?DEBUG("Annotate '~s' with inferred mailbox interfaces '~w'.", [
                      erl_prettypr:format(Expr), Mbs
                    ]),
%%                    {set_modality(MbMod, set_interfaces(MbName, Anno)), Analysis}
                    {set_interfaces(extract_interfaces(Mbs), Anno), Analysis}
                end;
              undefined_mb ->
                % Called function outside mailbox interface context.
                ?DEBUG("Skip '~s'.", [erl_prettypr:format(Expr)]),
                {Anno, Analysis}
            end;
          undefined_spec ->
            % Undefined fun reference. Invalid.
            ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
            ?ERROR("Undefined function '~s'.", [erl_prettypr:format(ErrNode)]),
            {Anno, ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis#analysis{result = Expr})}
        end,

      Analysis1 = annotate_expr_seq(Exprs, RecFuns, MbScopes, TypeInfo, Analysis0),
      Expr0 = paterl_syntax:set_anno(
        erl_syntax:application(Operator, Analysis1#analysis.result), Anno0
      ),
      Analysis1#analysis{result = Expr0};
    {error, Term} ->
      % Dynamic function call. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Term), erl_prettypr:format(Expr)
      ]),
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Expr})
  end;
annotate_expr(Expr = {call, _, _, _}, _MbAnno, _RecFuns, _MbScopes, _, Analysis) ->
  % Annotated local function call. Invalid.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Expr)
  ]),
  ?pushError(?E_BAD__ANNO_ON, Expr, Analysis#analysis{result = Expr});

annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno, RecFuns, MbScopes = undefined, TypeInfo, Analysis) ->
  % Receive expression outside mailbox interface scope. Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:receive_expr([]), Anno),
  ?ERROR("'~s' not in mailbox interface scope.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_NO__MB_SCOPE, ErrNode, Analysis),

  % Annotate rest of clauses to uncover further possible errors.
  Analysis1 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = Expr};
annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno = undefined, RecFuns, MbScopes, TypeInfo, Analysis) ->
  % Unannotated receive expression. Invalid.
  ErrNode = paterl_syntax:mb_anno(?ANNO_EXPECTS, [], Anno),
  ?ERROR("Expected annotation '~s'.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_EXP__ANNO, ErrNode, Analysis),

  % Annotate rest of clauses to uncover further possible errors.
  Analysis1 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = Expr};
annotate_expr(Expr = {'receive', _, _}, _MbAnno = {?ANNO_EXPECTS, Pattern}, RecFuns, MbScopes = [MbScope], TypeInfo, Analysis) ->
  % Annotated receive expression without interface name inside mailbox
  % interface scope with single interface. Valid, since mailbox interface can
  % be inferred from enclosing mailbox scope.
  ?DEBUG("Annotate '~s' with implicit mailbox interface '~s'.", [
    erl_prettypr:format(erl_syntax:receive_expr([])), MbScope]
  ),
  MbAnno0 = {?ANNO_EXPECTS, MbScope, Pattern},
  annotate_expr(Expr, MbAnno0, RecFuns, MbScopes, TypeInfo, Analysis);
annotate_expr(Expr = {'receive', Anno0, Clauses}, _MbAnno = {?ANNO_EXPECTS, MbName, Pattern}, RecFuns, MbScopes, TypeInfo, Analysis) ->
  % Annotated receive expression with interface name inside mailbox interface
  % scope with multiple interfaces. Mailbox interface inferred from the
  % enclosing mailbox scope must match the mailbox interface in the annotation.
  ?DEBUG("Annotate '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),

  case is_mb_in_scope(MbName, MbScopes) of
    true ->
      % Get function return type from spec. Return type of most recent fun
      % reference is used since it necessarily encloses the receive (call stack).
      FunRef = hd(RecFuns),
      {?T_SPEC, _, [FunType]} = paterl_types:spec_def(FunRef, TypeInfo),
      RetType = erl_syntax:function_type_return(FunType),

      % Annotate receive clauses.
      Analysis0 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis),

      % Function type is used to annotate the receive. Type annotation helps
      % inform the annotation phase when generating 'free' guard patterns in Pat
      % guard expressions. The function return type is required since the state
      % passing translation returns a pair whose first component is a value
      % returned by 'free'. Freeing in Erlang does not exist, which is why the
      % type is set such that 'free' returns a dummy value that has the same
      % return type of the enclosing function in Pat.
%%      Anno2 = set_type(RetType, set_state(Pattern, set_interfaces(MbName, Anno0))),
      Anno2 = set_type(RetType, set_pattern(Pattern, set_scope(MbName, Anno0))),
      Expr0 = paterl_syntax:set_anno(
        erl_syntax:receive_expr(Analysis0#analysis.result), Anno2
      ),
      Analysis0#analysis{result = Expr0};
    false ->
      % Mailbox interface not in scope. Invalid.
      ErrNode = paterl_syntax:name(MbName, Anno0),
      ?ERROR("mailbox interface '~s' not in scope", [erl_prettypr:format(ErrNode)]),
      Analysis0 = ?pushError(?E_UNDEF__MB_SCOPE, ErrNode, Analysis),

      % Annotate rest of clauses to uncover further possible errors.
      Analysis1 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis0),
      Analysis1#analysis{result = Expr}
  end;
annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno, RecFuns, MbScopes, TypeInfo, Analysis) ->
  % Annotated self expression with invalid annotation (i.e., not ?as). Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:receive_expr([]), Anno),
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(ErrNode)
  ]),
  Analysis0 = ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis),

  % Annotate rest of clauses to uncover further possible errors.
  Analysis1 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = Expr};

annotate_expr(_Expr = {'if', Anno, Clauses}, _MbAnno = undefined, RecFuns, MbScopes, TypeInfo, Analysis) ->
  % Unannotated if expression. Valid.
  ?DEBUG("Annotate '~s'.", [erl_prettypr:format(erl_syntax:if_expr([]))]),
  Analysis0 = annotate_clauses(Clauses, RecFuns, MbScopes, TypeInfo, Analysis),
  Expr0 =
    paterl_syntax:set_anno(erl_syntax:if_expr(Analysis0#analysis.result), Anno),
  Analysis0#analysis{result = Expr0};
annotate_expr(Expr0 = {'if', Anno, _}, _MbAnno, _RecFuns, _MbScopes, _, Analysis) ->
  % Annotated if expression. Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:if_expr([]), Anno),
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(ErrNode)
  ]),
  ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis#analysis{result = Expr0});

annotate_expr(Expr0 = {match, Anno, Pat, Expr1}, MbAnno, RecFuns, MbScopes, TypeInfo, Analysis)
  when is_tuple(MbAnno);
  MbAnno == undefined ->
  % Annotated or unannotated match expression. Annotation is passed through to
  % the RHS expression.
  case erl_syntax:type(Pat) of
    variable ->
      % Match pattern is a variable.
      ?DEBUG("Annotate '~s'.", [
        erl_prettypr:format(erl_syntax:match_expr(Pat, erl_syntax:underscore()))
      ]),

      Analysis0 = annotate_expr(Expr1, MbAnno, RecFuns, MbScopes, TypeInfo, Analysis),
      Expr3 =
        paterl_syntax:set_anno(erl_syntax:match_expr(Pat, Analysis0#analysis.result), Anno),
      Analysis0#analysis{result = Expr3};
    _ ->
      % Match pattern not a variable. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Pat),
        erl_prettypr:format(erl_syntax:match_expr(Pat, erl_syntax:underscore()))
      ]),
      Analysis0 = ?pushError(?E_BAD__EXPR, Expr0, Analysis),

      % Annotate expression to uncover further possible errors.
      Analysis1 = annotate_expr(Expr1, MbAnno, RecFuns, MbScopes, TypeInfo, Analysis0),
      Analysis1#analysis{result = Expr0}
  end;
annotate_expr(Expr, undefined, _RecFuns, _MbScopes, _, Analysis) ->
  % Non mailbox-annotated expression.
  ?TRACE("Skip '~s'.", [erl_prettypr:format(Expr)]),
  Analysis#analysis{result = Expr};
annotate_expr(Expr, _MbAnno, _RecFuns, _MbScopes, _, Analysis) ->
  % Unexpected annotated expression.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Expr)
  ]),
  ?pushError(?E_BAD__ANNO_ON, Expr, Analysis#analysis{result = Expr}).

-doc """
Determines whether the specified fun reference is contained in the recursive
functions list.
""".
is_rec_fun_ref(FunRef = {_, _}, RecFuns) when is_list(RecFuns) ->
  lists:member(FunRef, RecFuns).


%% TODO: Still to use.
-doc """
Determines whether the specified mailbox is contained in list of mailboxes in
scope.
""".
is_mb_in_scope(_, undefined) ->
  false;
is_mb_in_scope(MbName, MbScopes) when is_atom(MbName), is_list(MbScopes) ->
  lists:member(MbName, MbScopes).


extract_interfaces(Mbs) when is_list(Mbs) ->
  [{MbMod, MbName} || {MbMod, _, MbName} <- Mbs].

override_interfaces(Modality, Mbs)
  when
  Modality =:= ?MOD_NEW, is_list(Mbs);
  Modality =:= ?MOD_USE, is_list(Mbs) ->
  [{Modality, MbName} || {_, _, MbName} <- Mbs].


%%% ----------------------------------------------------------------------------
%%% Annotations.
%%% ----------------------------------------------------------------------------

-doc """
Retrieves the **type** annotation value from the specified annotation.

See `get_anno_val/3` for details.
""".
-spec type(Anno :: ext_anno()) -> paterl_syntax:type().
type(Anno) ->
  get_anno_val(Anno, ?MA_TYPE, undefined).

-spec scope(Anno :: ext_anno()) -> MbName :: paterl_syntax:name().
scope(Anno) ->
  get_anno_val(Anno, ?MA_SCOPE, undefined).

-spec scopes(Anno :: ext_anno()) -> [MbNames :: paterl_syntax:name()].
scopes(Anno) ->
  get_anno_val(Anno, ?MA_SCOPES, undefined).

-spec interface(Anno :: ext_anno()) -> Interface :: interface().
interface(Anno) ->
  get_anno_val(Anno, ?MA_INTERFACE, undefined).

-doc """
Retrieves the **interface** annotation value from the specified annotation.

See `get_anno_val/3` for details.
""".
-spec interfaces(Anno :: ext_anno()) -> [Interface :: interface()].
interfaces(Anno) ->
  get_anno_val(Anno, ?MA_INTERFACES, undefined).

-doc """
Retrieves the **state** annotation value from the annotation.

See `get_anno_val/3` for details.
""".
-spec pattern(Anno :: ext_anno()) -> Pattern :: string().
pattern(Anno) ->
  get_anno_val(Anno, ?MA_PATTERN, undefined).




-doc """
Stores the specified `Type` annotation value in the annotation.

See `set_anno_val/3` for details.
""".
-spec set_type(Type, Anno) -> Anno0
  when
  Type :: paterl_syntax:type(),
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_type(Type, Anno) when is_tuple(Type) ->
  set_anno_val(Anno, ?MA_TYPE, Type).

-spec set_scope(MbName, Anno) -> Anno0
  when
  MbName :: paterl_syntax:name(),
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_scope(MbName, Anno) when is_atom(MbName) ->
  set_anno_val(Anno, ?MA_SCOPE, MbName).

-spec set_scopes(MbNames, Anno) -> Anno0
  when
  MbNames :: [paterl_syntax:name()],
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_scopes(MbNames, Anno) when is_list(MbNames) ->
  set_anno_val(Anno, ?MA_SCOPES, MbNames).

-spec set_interface(Interface, Anno) -> Anno0
  when
  Interface :: interface(),
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_interface(Interface, Anno) when
  is_tuple(Interface), element(1, Interface) =:= ?MOD_NEW, is_atom(element(2, Interface));
  is_tuple(Interface), element(1, Interface) =:= ?MOD_USE, is_atom(element(2, Interface)) ->
  set_anno_val(Anno, ?MA_INTERFACE, Interface).

-doc """
Stores the specified `Interface` annotation value in the annotation.

See `set_anno_val/3` for details.
""".
-spec set_interfaces(Interfaces, Anno) -> Anno0
  when
  Interfaces :: [interface()],
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_interfaces(Interfaces, Anno) when is_list(Interfaces) ->
  set_anno_val(Anno, ?MA_INTERFACES, Interfaces).

-spec set_interfaces(Modality, Interfaces, Anno) -> Anno0
  when
  Modality :: paterl_types:modality(),
  Interfaces :: [interface()],
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_interfaces(Modality, Interfaces, Anno) when
  Modality =:= ?MOD_NEW, is_list(Interfaces);
  Modality =:= ?MOD_USE, is_list(Interfaces) ->
  Interfaces0 = [{Modality, MbName} || {_, _, MbName} <- Interfaces],
  set_anno_val(Anno, ?MA_INTERFACES, Interfaces0).

-doc """
Stores the specified `State` annotation value in the annotation.

See `set_anno_val/3` for details.
""".
-spec set_pattern(Pattern, Anno) -> Anno0
  when
  Pattern :: string(),
  Anno :: ext_anno(),
  Anno0 :: ext_anno().
set_pattern(State, Anno) when is_list(State) ->
  set_anno_val(Anno, ?MA_PATTERN, State).





-doc """
Stores the specified `Value` associated with `Key` in the annotation.

This function extends the functionality of the `m:erl_anno` module.

`Anno` can be a line number, line number-column pair, or a list of arbitrary
key-value elements; see [`anno()`](`t:erl_anno:anno/0`).

### Returns
- updated annotation containing the new key-value element.

### Throws
-`badarg` if `Anno` is not a line number, line number-column pair or a list of
key-value elements.
""".
-spec set_anno_val(Anno, Key, Value) -> Anno0
  when
  Anno :: ext_anno(),
  Key :: term(),
  Value :: term(),
  Anno0 :: ext_anno().
set_anno_val(Anno, Key, Value) ->
  Anno0 =
    case Anno of
      Line when is_integer(Line) ->
        [{location, Line}];
      {Line, Column} when is_integer(Line), is_integer(Column) ->
        [{location, {Line, Column}}];
      Anno when is_list(Anno) ->
        Anno;
      _ ->
        erlang:error(badarg, [Anno, Key, Value])
    end,
  lists:keystore(Key, 1, Anno0, {Key, Value}).

-doc """
Retrieves the `Value` associated with the specified 'Key' from the annotation.

This function extends the functionality of the `m:erl_anno` module.

`Anno` can be a line number, line number-column pair, or a list of arbitrary
key-value elements; see [`anno()`](`t:erl_anno:anno/0`).

### Returns
- value associated with `Key`
- `Default` when `Key` is not found
- `undefined` when `Anno` is a line number or line number-column pair

### Throws
-`badarg` if `Anno` is not a line number, line number-column pair or a list of
key-value elements.
""".
-spec get_anno_val(Anno, Key, Default) -> Value
  when
  Anno :: ext_anno(),
  Key :: term(),
  Default :: term(),
  Value :: term().
get_anno_val(Anno, Key, Default) ->
  case Anno of
    Line when is_integer(Line) ->
      undefined;
    {Line, Column} when is_integer(Line), is_integer(Column) ->
      undefined;
    Anno when is_list(Anno) ->
      case lists:keyfind(Key, 1, Anno) of
        false ->
          Default;
        {Key, Value} ->
          Value
      end;
    _ ->
      erlang:error(badarg, [Anno, Key, Default])
  end.

-doc """
Maps all the annotations at the root of the specified `Tree` using `Fun`.

### Returns
- updated `Tree`
""".
-spec map_anno(Fun, Tree) -> Tree0
  when
  Fun :: fun((Anno :: erl_anno:anno()) -> Anno0 :: erl_anno:anno()),
  Tree :: paterl_syntax:tree(),
  Tree0 :: paterl_syntax:tree().
map_anno(Fun, Tree) ->
  map_anno(Fun, Tree, 0).

-doc """
Maps all the annotations in the specified `Tree` using `Fun` up to and including
`Depth`.

### Returns
- updated `Tree`
""".
-spec map_anno(Fun, Tree, Depth) -> Tree0
  when
  Fun :: fun((Anno :: erl_anno:anno()) -> Anno0 :: erl_anno:anno()),
  Tree :: paterl_syntax:tree(),
  Depth :: non_neg_integer(),
  Tree0 :: paterl_syntax:tree().
map_anno(Fun, Tree, Depth)
  when is_function(Fun, 1), is_integer(Depth), Depth >= 0 ->
  {Tree0, _} = erl_parse:mapfold_anno(
    fun(Anno, D) when D == Depth ->
      {Fun(Anno), D + 1};
      (Anno, D) -> {Anno, D + 1}
    end,
    0, Tree),
  Tree0.

-doc """
Maps the annotations in the specified `Tree` using `Fun` up to but not including
`Depth`.

### Returns
- updated `Tree`
""".
-spec map_anno_lt(Fun, Tree, Depth) -> Tree0
  when
  Fun :: fun((Anno :: erl_anno:anno()) -> Anno0 :: erl_anno:anno()),
  Tree :: paterl_syntax:tree(),
  Depth :: non_neg_integer(),
  Tree0 :: paterl_syntax:tree().
map_anno_lt(Fun, Tree, Depth) ->
  erl_parse:mapfold_anno(
    fun(Anno, D) when D < Depth ->
      {Fun(Anno), D + 1};
      (Anno, D) -> {Anno, D + 1}
    end,
    0, Tree).


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

-doc """
Formats `Detail` to human-readable form.

See `module/2` for error and warnings codes.

### Returns
- message as a possibly deep [`io_lib:chars()`](`t:io_lib:chars/0`) list
""".
-spec format_error(Detail :: paterl_lib:detail()) -> io_lib:chars().
format_error({?E_UNDEF__FUN_SPEC, Node}) ->
  io_lib:format(
    "function '~s' has missing spec",
    [erl_prettypr:format(Node)]
  );
format_error({?E_EXP__EXPR, Node}) ->
  io_lib:format(
    "expected expression after '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_EXP__ANNO, Node}) ->
  io_lib:format(
    "expected annotation '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__ANNO_ON, Node}) ->
  io_lib:format(
    "unexpected annotation on '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__EXPR, Node}) ->
  io_lib:format(
    "unsupported expression '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__CLAUSE, Node}) ->
  io_lib:format(
    "unsupported function clause '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_NO__MB_SCOPE, Node}) ->
  io_lib:format(
    "'~s' not in mailbox interface scope",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__MB_SCOPE, Node}) ->
  io_lib:format(
    "mailbox interface '~s' not in scope",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__MB, Node}) ->
  io_lib:format(
    "undefined mailbox interface for '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__FUN_REF, Node}) ->
  io_lib:format(
    "undefined function '~s'",
    [erl_prettypr:format(Node)]
  ).