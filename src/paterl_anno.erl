%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang AST annotator.
%%% @end
%%% Created : 29. Jan 2024 15:20
%%%-------------------------------------------------------------------
-module(paterl_anno).
-feature(maybe_expr, enable).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl"). % TODO: Eliminate this.
-include("log.hrl").
-include("paterl_lib.hrl").
-include("paterl_syntax.hrl").

%% API
-export([module/2]).

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(call_info, {
  call_graph = #{},
  mb_scopes = []
}).

% {tuple, _, []}
%%-define(isAnnoTuple(Expr), (
%%    element(1, Expr) =:= tuple andalso is_list(element(3, Expr))
%%)).
%%
%%
%%
%%% [{atom, _, new}, {atom, _, MbName}]
%%% [{atom, _, use}, {atom, _, MbName}]
%%% [{atom, _, state}, {string, _, Pattern}]
%%-define(isAnno(Expr, Type), ).
%%
%%-define(isModAnno(Expr),
%%  % Is a tuple.
%%  % Is name is new or use.
%%  ?isAnnoTuple(Expr) andalso
%%  length(element(3, Expr)) =:= 2 andalso % Length.
%%    ?annoVal(hd(element(3, Expr))) =:= tuple andalso
%%
%%).
%%
%%-define(isAssertAnno(Expr),
%%  ok
%%).
%%
%%-define(isMbAnno(Expr),
%%  ok
%%).
%%
%%-ifdef(test).
%%is_anno_tuple(Expr) when ?isAnnoTuple(Expr) ->
%%  true;
%%is_anno_tuple(_) ->
%%  false.
%%
%%%
%%-endif.

%%% Mailbox annotation types.

%% Mailbox annotation primitive type.
-define(MA_TYPE, type).

%% Mailbox annotation interface.
-define(MA_INTERFACE, interface).

%% Mailbox annotation capability.
-define(MA_READ, read).

%% Mailbox annotation interface usage modality.
-define(MA_MODALITY, modality).

%% Mailbox annotation regular expression state.
-define(MA_STATE, state).

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

%% Undefined mailbox scope.
-define(E_UNDEF__MB_SCOPE, e_undef__mb_scope).


%% Undefined mailbox interface.
-define(E_UNDEF__MB, e_undef__mb).

%% Undefined fun reference.
-define(E_UNDEF__FUN_REF, e_undef__fun_ref).

%% Function omits corresponding spec.
-define(E_UNDEF__FUN_SPEC, e_undef__fun_spec).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Annotates the Erlang AST with the type information contained in TInfo.
%%
%% TInfo is assumed to contain terms conforming to the erl_parse specification,
%% and not erl_syntax.
%%
%% The error reporting algorithm is greedy. It tries to annotate the rest of the
%% AST even if errors are found to report as much as it can to the user. This is
%% a design decision that makes sense.
%%
%% @param Forms: AST to annotate.
%% @param TInfo: Type annotations.
%%
%% @returns Annotated AST.
-spec module([erl_syntax:syntaxTree()], paterl_types:type_info()) ->
  {ok, erl_syntax:forms()} | errors:error().
module(Forms, TypeInfo = #type_info{}) when is_list(Forms) ->
  % Return annotated Erlang AST with as result.
  Analysis = annotate_forms(Forms, TypeInfo, #analysis{}),
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
List of [`form()`](`t:paterl_syntax:forms/0`).
""".
-spec get_interfaces(paterl_types:type_defs()) -> [erl_syntax:syntaxTree()].
get_interfaces(#type_info{type_defs = TypeDefs}) when is_map(TypeDefs) ->
  lists:reverse(
    maps:fold(
      fun(Name, {?T_MBOX, Anno, _, Vars = []}, Attrs) ->
        ?TRACE("Create interface for mailbox '~p'.", [Name]),

        MsgTypesAS =
          case expand_msg_type_set(Name, TypeDefs) of
            [Type] ->
              % Mailbox type is a singleton.
              Type;
            Types when is_list(Types) ->
              % Mailbox type is a list that must be recombined as a type
              % union.
              paterl_syntax:set_anno(erl_syntax:type_union(Types), Anno)
          end,

        % Create interface attribute.
        ?TRACE("Expanded mailbox interface type '~s() :: ~s'.", [Name, erl_prettypr:format(MsgTypesAS)]),
        Interface = paterl_syntax:set_anno(
          erl_syntax:attribute(
            erl_syntax:atom(interface),
            [erl_syntax:tuple([
              erl_syntax:atom(Name), % Mailbox type name.
              erl_syntax:abstract(MsgTypesAS), % Lifted message types AS.
              erl_syntax:abstract(Vars)] % Lifted variable types AS.
            )]
          ),
          Anno),
        [Interface | Attrs];
        (_, _, Attrs) ->
          % Eat other attributes.
          Attrs
      end,
      [], TypeDefs)
  ).

-doc """
Expands message types to their full tuple definition form.

### Returns
List of [`type()`](`t:paterl_syntax:type/0`).
""".
-spec expand_msg_type_set(Name, TypeDefs) -> Types
  when
  Name :: paterl_syntax:name(),
  TypeDefs :: paterl_types:type_defs(),
  Types :: [paterl_syntax:type()].
expand_msg_type_set(Name, TypeDefs) ->
  % Mailbox message set type. Type variables not supported.
  {?T_MBOX, _, Type, _Vars = []} = maps:get(Name, TypeDefs),
  lists:reverse(expand_msg_type(Type, TypeDefs, [])).

-doc """
Expands a message type to its full tuple defintion form.

### Returns
List of [`type()`](`t:paterl_syntax:type/0`).
""".
-spec expand_msg_type(Type, TypeDefs, Acc) -> Types
  when
  Type :: paterl_syntax:type(),
  TypeDefs :: paterl_types:type_defs(),
  Acc :: [paterl_syntax:type()],
  Types :: [paterl_syntax:type()].
expand_msg_type(Type = {type, _, pid, _Vars = []}, TypeDefs, Acc) when is_map(TypeDefs) ->
  % Built-in pid type. Denotes the empty message set.
  ?assertEqual(erl_syntax:type(Type), type_application),
  ?TRACE("Found type '~s'.", [erl_prettypr:format(Type)]),
  [Type | Acc];
expand_msg_type(Type = {type, _, tuple, _}, TypeDefs, Acc) when is_map(TypeDefs) ->
  % Tuple type. Denotes the singleton message set.
  ?assertEqual(erl_syntax:type(Type), tuple_type),
  ?TRACE("Found type '~s'.", [erl_prettypr:format(Type)]),
  [Type | Acc];
expand_msg_type(_Type = {type, _, union, Union}, TypeDefs, Acc) when is_map(TypeDefs) ->
  % User-defined type union. Denotes a non-trivial message set.
  ?assertEqual(erl_syntax:type(_Type), type_union),
  ?TRACE("Inspect type '~s'.", [erl_prettypr:format(_Type)]),
  lists:foldl(
    fun(Type0, Acc0) ->
      expand_msg_type(Type0, TypeDefs, Acc0)
    end,
    Acc, Union);
expand_msg_type(_Type = {user_type, _, Name, _Vars = []}, TypeDefs, Acc) when is_map(TypeDefs) ->
  % User-defined type. Denotes another message type which is a message set
  % alias.
  ?TRACE("Inspect type '~s'.", [erl_prettypr:format(_Type)]),
  {?T_TYPE, _, UserType, _} = maps:get(Name, TypeDefs),
  expand_msg_type(UserType, TypeDefs, Acc).


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
-spec annotate_forms(Forms, TypeInfo, Analysis) -> Analysis0
  when
  Forms :: paterl_syntax:forms(),
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_forms([], _, Analysis) ->
  % Empty forms.
  Analysis#analysis{result = []};
annotate_forms([Form = {attribute, _, module, _} | Forms], TypeInfo, Analysis) ->
  % Module attribute. Expand interfaces after it.
  Interfaces = get_interfaces(TypeInfo),
  Analysis0 = annotate_forms(Forms, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Interfaces ++ Analysis0#analysis.result]};
annotate_forms([Form = {attribute, _, export, _} | Forms], TypeInfo, Analysis) ->
  % Export attribute. Leave in so that Erlang compiler compiles successfully.
  Analysis0 = annotate_forms(Forms, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Analysis0#analysis.result]};
annotate_forms([Form = {attribute, _, import, _} | Forms], TypeInfo, Analysis) ->
  % Import attribute. Leave in so that Erlang compiler compiles successfully.
  Analysis0 = annotate_forms(Forms, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Analysis0#analysis.result]};
annotate_forms([{attribute, _, _, _} | Forms], TypeInfo, Analysis) ->
  % Eat other attributes.
  annotate_forms(Forms, TypeInfo, Analysis);
annotate_forms([Form = {function, _, _, _, _} | Forms], TypeInfo, Analysis) ->
  % Function.
  Analysis0 = annotate_function(Form, TypeInfo, Analysis),
  Analysis1 = annotate_forms(Forms, TypeInfo, Analysis0),
  Analysis1#analysis{
    result = [Analysis0#analysis.result | Analysis1#analysis.result]
  };
annotate_forms([_ | Forms], TypeInfo, Analysis) ->
  % Eat other forms.
  annotate_forms(Forms, TypeInfo, Analysis).

%% @private Annotates functions with a single clause and without guards.

-doc """
Annotates unguarded functions with a single clause.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if annotation is successful
- `status=error` with details otherwise
""".
-spec annotate_function(Form, TypeInfo, Analysis) -> Analysis0
  when
  Form :: paterl_syntax:form(),
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_function({function, Anno, Name, Arity, Clauses}, TypeInfo, Analysis) ->

  % Recover fun reference.
  FunRef = {Name, Arity},

  % Initialize function scopes. Scopes are added when other functions are called
  % from within this function body. Tracks called functions to determine when a
  % ?use annotation is applied for direct or mutually recursive calls.
  FunScopes = [FunRef],

  maybe
  % Only one function clause, and therefore, one spec is assumed.
    {?T_SPEC, _, Types} ?= paterl_types:spec_def(FunRef, TypeInfo),
    ?assert(length(Clauses) == 1 andalso length(Types) == 1),

    % Determine if function has an associated mailbox scope.
    case paterl_types:mb_fun(FunRef, TypeInfo) of
      {MbMod, _, MbName} ->
        % Function inside mailbox scope (at least one mailbox interface).
        ?DEBUG("Annotate function '~s' inside mailbox scope '~s'.", [
          erl_prettypr:format(paterl_syntax:fun_reference(FunRef, Anno)), MbName
        ]),

        % Initialize mailbox scopes. Tracks accessible mailbox interface names.
        MbScopes = [MbName],

        % Annotate function clauses.
        Analysis1 = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TypeInfo, Analysis),
        Anno1 = set_modality(MbMod, set_interface(MbName, Anno)),
        Form0 = map_anno(fun(_) ->
          Anno1 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Analysis1#analysis.result))),
        Analysis1#analysis{result = Form0};

      undefined_mb ->
        % Function outside mailbox scope (no mailbox interface).
        ?DEBUG("Annotate function '~s' outside mailbox scope.", [
          erl_prettypr:format(paterl_syntax:fun_reference(FunRef, Anno))
        ]),

        % No mailbox interface names to track.
        MbScopes = undefined,

        % Annotate function clauses.
        Analysis1 = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TypeInfo, Analysis),
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
-spec annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clauses :: [paterl_syntax:clause()],
  Types :: [paterl_syntax:type()],
  FunScopes :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_fun_clauses([], [], _, _, _, Analysis) ->
  Analysis#analysis{result = []};
annotate_fun_clauses([Clause | Clauses], [Type | Types], FunScopes, MbScopes, TypeInfo, Analysis) ->
  Analysis0 = annotate_fun_clause(Clause, Type, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TypeInfo, Analysis0),
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
-spec annotate_fun_clause(Clause, Type, FunScopes, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clause :: paterl_syntax:clause(),
  Type :: paterl_syntax:type(),
  FunScopes :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body},
    _ArgType = {type, _, 'fun', [{type, _, product, TypeSeq}, RetType]}, FunScopes, MbScopes, TypeInfo, Analysis)
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

  % Set mailbox interface if function inside mailbox scope.
  Anno1 =
    if
      MbScopes =:= undefined -> Anno0; true ->
      set_interface(hd(MbScopes), Anno0) % TODO: This hd() would be removed when we tackle multiple mailboxes
    end,

  % Annotate function body.
  Analysis0 = annotate_expr_seq(Body, FunScopes, MbScopes, TypeInfo, Analysis),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(AnnPatSeq, _GuardSeq, Analysis0#analysis.result), Anno1
  ),
  Analysis0#analysis{result = Clause0};
annotate_fun_clause({clause, Anno, PatSeq, GuardSeq, _}, _ArgType, _FunScopes, _MbScopes, _, Analysis) ->
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
-spec annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clauses :: [paterl_syntax:clause()],
  FunScopes :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_clauses([], _, _, _, Analysis) ->
  Analysis#analysis{result = []};
annotate_clauses([Clause | Clauses], FunScopes, MbScopes, TypeInfo, Analysis) ->
  Analysis0 = annotate_clause(Clause, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
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
-spec annotate_clause(Clause, FunScopes, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Clause :: paterl_syntax:clause(),
  FunScopes :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    Signature, MbScope, TypeInfo, Analysis)
  when is_list(GuardSeq), is_list(Body) ->
  % If clause.
  ?TRACE("Annotate clause '~s'.", [erl_syntax:type(Clause)]),

  Analysis0 = annotate_expr_seq(Body, Signature, MbScope, TypeInfo, Analysis),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(GuardSeq, Analysis0#analysis.result), Anno
  ),
  Analysis0#analysis{result = Clause0};
annotate_clause(Clause = {clause, Anno, PatSeq = [_], GuardSeq = [], Body}, Signature, MbScope, TypeInfo, Analysis) ->
  % Receive or case clause
  ?TRACE("Annotate clause '~s'.", [erl_syntax:type(Clause)]),

  Analysis0 = annotate_expr_seq(Body, Signature, MbScope, TypeInfo, Analysis),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(PatSeq, GuardSeq, Analysis0#analysis.result), Anno
  ),
  Analysis0#analysis{result = Clause0};
annotate_clause(Clause, _Signature, _MbScope, _, Analysis) ->
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
-spec annotate_expr_seq(Exprs, FunScopes, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Exprs :: [paterl_syntax:expr()],
  FunScopes :: [paterl_syntax:fun_ref()],
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
annotate_expr_seq([MbAnno0, MbAnno1 | ExprSeq], FunScopes, MbScopes, TypeInfo, Analysis) when ?isMbAnno(MbAnno0), ?isMbAnno(MbAnno1) ->
  % Two successive annotations. Invalid.
  Name = paterl_syntax:mb_anno_name(MbAnno0),
  Args = paterl_syntax:mb_anno_args(MbAnno0),
  Anno = erl_syntax:get_pos(MbAnno0),

  ErrNode = paterl_syntax:mb_anno(Name, Args, Anno),
  ?ERROR("Expected expression after '~s'.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_EXP__EXPR, ErrNode, Analysis),

  % Annotate rest of expressions and uncover further possible errors.
  annotate_expr_seq(ExprSeq, FunScopes, MbScopes, TypeInfo, Analysis0);
annotate_expr_seq([MbAnno, Expr | ExprSeq], FunScopes, MbScopes, TypeInfo, Analysis) when ?isMbAnno(MbAnno) ->
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
  Analysis0 = annotate_expr(Expr, MbAnno0, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_expr_seq(ExprSeq, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = [
    Analysis0#analysis.result | Analysis1#analysis.result
  ]};
annotate_expr_seq([Expr | ExprSeq], FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated expression. May be valid.
  Analysis0 = annotate_expr(Expr, undefined, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis1 = annotate_expr_seq(ExprSeq, FunScopes, MbScopes, TypeInfo, Analysis0),
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
-spec annotate_expr(Expr, MbAnno, FunScopes, MbScopes, TypeInfo, Analysis) -> Analysis0
  when
  Expr :: paterl_syntax:expr(),
  MbAnno :: paterl_syntax:mb_anno(),
  FunScopes :: [paterl_syntax:fun_ref()],
  MbScopes :: [paterl_syntax:name()],
  TypeInfo :: paterl_types:type_info(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
annotate_expr(Call = {call, Anno, Operator = {atom, _, spawn}, MFArgs}, _MbAnno = undefined, _FunScopes, _MbScopes, TypeInfo, Analysis) ->
  % Unannotated spawn expression. MFArgs can be a static or dynamic function.
  % Static functions in spawn expressions must be unannotated, whereas dynamic
  % functions must be annotated. The latter are currently unsupported.
  case get_fun_ref_mfa(MFArgs) of
    {ok, _, FunRef = {_, _}} ->
      % MFArgs is a static function. May be valid.
      ?DEBUG("Annotate '~s'.", [erl_prettypr:format(Call)]),

      maybe
      % Check fun reference is defined.
        {?T_SPEC, _, _} ?= paterl_types:spec_def(FunRef, TypeInfo),

        % Check that mailbox interface is defined.
        {_MbMod, _Anno, MbName} ?= paterl_types:mb_fun(FunRef, TypeInfo),

        % Override modality with ?new for spawn call.
        ?DEBUG("Override '~s' with '~s' on '~s'.", [
          erl_prettypr:format(paterl_syntax:mb_definition(_MbMod, [MbName], _Anno)),
          erl_prettypr:format(paterl_syntax:mb_anno(?MOD_NEW, [MbName], Anno)),
          erl_prettypr:format(Call)
        ]),
        Anno0 = set_modality(?MOD_NEW, set_interface(MbName, Anno)),
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
        erl_prettypr:format(Term), erl_prettypr:format(Call)
      ]),
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Term})
  end;
annotate_expr(Call = {call, Anno, {atom, _, OpName = spawn}, _MFArgs}, MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated spawn expression. Invalid.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(MbAnno)),
    erl_prettypr:format(Call)
  ]),
  ErrNode = paterl_syntax:application(OpName, [], Anno),
  ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis#analysis{result = ErrNode});

annotate_expr(Call = {call, _, {atom, _, self}, []}, _MbAnno, _FunScopes, _MbScopes = undefined, _, Analysis) ->
  % Annotated self expression outside mailbox scope. Invalid.
  ?ERROR("'~s' not in mailbox interface scope.", [erl_prettypr:format(Call)]),
  ?pushError(?E_NO__MB_SCOPE, Call, Analysis#analysis{result = Call});
annotate_expr(_Call = {call, Anno, Self = {atom, _, self}, []}, _MbAnno = undefined, _FunScopes, [MbScope], _, Analysis) ->
  % Unannotated self expression inside mailbox interface scope. Valid. Mailbox
  % interface name can be inferred from the enclosing mailbox scope.
  % TODO: Eventually remove? No. If the mailbox scope is a singleton list, then
  % you can infer it so don't enforce it on the user, but if not, then error out
  % and the user must specify it via the ?as annotation.
  ?TRACE("Annotate '~s' with inferred mailbox interface '~s'.", [
    erl_prettypr:format(_Call), MbScope
  ]),
  Anno0 = set_interface(MbScope, Anno),
  Expr0 = paterl_syntax:set_anno(erl_syntax:application(Self, []), Anno0),
  Analysis#analysis{result = Expr0};
annotate_expr(Call = {call, Anno, Self = {atom, _, self}, []}, {?ANNO_AS, MbName}, _FunScopes, MbScopes, _, Analysis) ->
  % Annotated self expression inside mailbox scope. May be valid. Mailbox
  % interface inferred from the enclosing mailbox scope must match the mailbox
  % interface in the annotation.
  case is_mb_in_scope(MbName, MbScopes) of
    true ->
      % Mailbox interface name in scope.
      ?TRACE("Annotate '~s' with inferred matching mailbox interface '~s'.", [
        erl_prettypr:format(Call), MbName
      ]),
      Anno0 = set_interface(MbName, Anno),
      Expr0 = paterl_syntax:set_anno(erl_syntax:application(Self, []), Anno0),
      Analysis#analysis{result = Expr0};
    false ->
      % Mailbox interface name out of scope. Invalid.
      ?ERROR("Mailbox interface '~s' not in scope.", [MbName]),
      ?pushError(?E_UNDEF__MB_SCOPE, paterl_syntax:name(MbName, Anno), Analysis#analysis{result = Call})
  end;
annotate_expr(Call = {call, _, {atom, _, self}, []}, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated self expression with invalid annotation (i.e., not ?as). Invalid.
  ?ERROR("Unexpected '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)), %TODO: Here we can benefit from storing the anno() of the mailbox annotation.
    erl_prettypr:format(Call)
  ]),
  ?pushError(?E_BAD__ANNO_ON, Call, Analysis#analysis{result = Call});

annotate_expr(Call = {call, Anno, Operator, Exprs}, _MbAnno = undefined, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated local function call.
  case get_fun_ref(Call) of
    {ok, FunRef = {_, _}} ->
      % Static function call. Check if fun reference is defined.
      {Anno0, Analysis0} =
        case paterl_types:spec_def(FunRef, TypeInfo) of
          {spec, _, _} ->
            % Check if mailbox interface is defined.
            case paterl_types:mb_fun(FunRef, TypeInfo) of
              {MbMod, _Anno, MbName} ->
                % Called function inside mailbox interface context.
                % Check if recursive function call. TODO properly later
                case is_fun_ref_in_scope(FunRef, FunScopes) of
                  true ->
                    % Recursive call. Override usage modality with ?use.
                    ?DEBUG("Annotate recursive '~s' with inferred mailbox interface '~s'.", [
                      erl_prettypr:format(Call), MbName
                    ]),
                    {set_modality(?MOD_USE, set_interface(MbName, Anno)), Analysis};
                  false ->
                    % Non-recursive call.
                    ?DEBUG("Annotate '~s' with inferred mailbox interface '~s'.", [
                      erl_prettypr:format(Call), MbName
                    ]),
                    {set_modality(MbMod, set_interface(MbName, Anno)), Analysis}
                end;
              undefined_mb ->
                % Called function outside mailbox interface context.
                ?DEBUG("Skip '~s'.", [erl_prettypr:format(Call)]),
                {Anno, Analysis}
            end;
          undefined_spec ->
            % Undefined fun reference. Invalid.
            ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
            ?ERROR("Undefined function '~s'.", [erl_prettypr:format(ErrNode)]),
            {Anno, ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis#analysis{result = Call})}
        end,

      Analysis1 = annotate_expr_seq(Exprs, FunScopes, MbScopes, TypeInfo, Analysis0),
      Expr0 = paterl_syntax:set_anno(
        erl_syntax:application(Operator, Analysis1#analysis.result), Anno0
      ),
      Analysis1#analysis{result = Expr0};
    {error, Term} ->
      % Dynamic function call. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Term), erl_prettypr:format(Call)
      ]),
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Call})
  end;
annotate_expr(Call = {call, _, _, _}, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated local function call. Invalid.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Call)
  ]),
  ?pushError(?E_BAD__ANNO_ON, Call, Analysis#analysis{result = Call});

annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno, FunScopes, MbScopes = undefined, TypeInfo, Analysis) ->
  % Receive expression outside mailbox interface scope. Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:receive_expr([]), Anno),
  ?ERROR("'~s' not in mailbox interface scope.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_NO__MB_SCOPE, ErrNode, Analysis),

  % Annotate rest of clauses to uncover further possible errors.
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = Expr};
annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno = undefined, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated receive expression. Invalid.
  ErrNode = paterl_syntax:mb_anno(?ANNO_EXPECTS, [], Anno),
  ?ERROR("Expected annotation '~s'.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_EXP__ANNO, ErrNode, Analysis),

  % Annotate rest of clauses to uncover further possible errors.
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = Expr};
annotate_expr(Expr = {'receive', Anno0, Clauses}, _MbAnno = {?ANNO_EXPECTS, MbName, Pattern}, FunScopes, MbScopes, TypeInfo = #type_info{spec_defs = Specs}, Analysis) ->
  % Annotated receive expression inside mailbox interface scope. May be valid.
  ?DEBUG("Annotate '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),

  case is_mb_in_scope(MbName, MbScopes) of
    true ->
      % Get function return type from spec. Return type of most recent fun
      % reference is used since it necessarily encloses the receive (call stack).
      FunRef = hd(FunScopes),
      {?T_SPEC, _, [FunType]} = paterl_types:spec_def(FunRef, TypeInfo),
      RetType = erl_syntax:function_type_return(FunType),

      % Annotate receive clauses.
      Analysis0 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),

      % Function type is used to annotate the receive. Type annotation helps
      % inform the annotation phase when generating 'free' guard patterns in Pat
      % guard expressions. The function return type is required since the state
      % passing translation returns a pair whose first component is a value
      % returned by 'free'. Freeing in Erlang does not exist, which is why the
      % type is set such that 'free' returns a dummy value that has the same
      % return type of the enclosing function in Pat.
      Anno2 = set_type(RetType, set_state(Pattern, set_interface(MbName, Anno0))),
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
      Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
      Analysis1#analysis{result = Expr}
  end;
% TODO: Next tackle this. Remove this function clause and update all the examples with ?expects.
annotate_expr({'receive', Anno, Clauses}, _MbAnno = {state, Regex}, FunScopes, MbScopes, TypeInfo = #type_info{spec_defs = Specs}, Analysis) ->
  % Annotated blocking receive expression inside mailbox interface scope. %TODO: This clause should be removed eventually once the annotations are updated to use ?expects
  % Mailbox name can be inferred from mailbox enclosing scope. FOR NOW!
  Analysis0 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),

  ?TRACE("Annotating receive with return spec in '~s/~b'.", [element(1, hd(FunScopes)), element(2, hd(FunScopes))]),

  % This pattern match must always succeed since every signature has a type spec
  % associated with it.
  {spec, _, [{type, _, 'fun', [_ParamTypes, ReturnType]}]} = maps:get(hd(FunScopes), Specs),

  % The type of the receive is set to the return type of the function in order
  % to enable us to generate the 'free' guard in Pat receive statements.
  Anno0 = set_type(ReturnType, set_state(Regex, set_interface(hd(MbScopes), Anno))),
  Expr0 = paterl_syntax:set_anno(
    erl_syntax:receive_expr(Analysis0#analysis.result), Anno0
  ),
  Analysis0#analysis{result = Expr0};
annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Annotated self expression with invalid annotation (i.e., not ?as). Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:receive_expr([]), Anno),
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(ErrNode)
  ]),
  Analysis0 = ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis),

  % Annotate rest of clauses to uncover further possible errors.
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1#analysis{result = Expr};

annotate_expr(_Expr = {'if', Anno, Clauses}, _MbAnno = undefined, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated if expression. Valid.
  ?DEBUG("Annotate '~s'.", [erl_prettypr:format(erl_syntax:if_expr([]))]),
  Analysis0 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),
  Expr0 =
    paterl_syntax:set_anno(erl_syntax:if_expr(Analysis0#analysis.result), Anno),
  Analysis0#analysis{result = Expr0};
annotate_expr(Expr0 = {'if', Anno, _}, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated if expression. Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:if_expr([]), Anno),
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(ErrNode)
  ]),
  ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis#analysis{result = Expr0});

annotate_expr(Expr0 = {match, Anno, Pat, Expr1}, MbAnno, Signature, MbScope, TypeInfo, Analysis)
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

      Analysis0 = annotate_expr(Expr1, MbAnno, Signature, MbScope, TypeInfo, Analysis),
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
      Analysis1 = annotate_expr(Expr1, MbAnno, Signature, MbScope, TypeInfo, Analysis0),
      Analysis1#analysis{result = Expr0}
  end;
annotate_expr(Expr, undefined, _Signature, _MbAnno, _, Analysis) ->
  % Non mailbox-annotated expression.
  ?TRACE("Skip '~s'.", [erl_prettypr:format(Expr)]),
  Analysis#analysis{result = Expr};
annotate_expr(Expr, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Unexpected annotated expression.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Expr)
  ]),
  ?pushError(?E_BAD__ANNO_ON, Expr, Analysis#analysis{result = Expr}).


%%% ----------------------------------------------------------------------------
%%% Annotations.
%%% ----------------------------------------------------------------------------

type(Anno) ->
  get_anno_val(Anno, ?MA_TYPE, undefined).

interface(Anno) ->
  get_anno_val(Anno, ?MA_INTERFACE, undefined).

%%read(Anno) ->
%%  get_anno_val(Anno, ?MA_READ, false).

modality(Anno) ->
  get_anno_val(Anno, ?MA_MODALITY, undefined).

state(Anno) ->
  get_anno_val(Anno, ?MA_STATE, undefined).

set_type(Type, Anno) ->
  set_anno_val(Anno, ?MA_TYPE, Type).

set_interface(Interface, Anno) when is_atom(Interface) ->
  set_anno_val(Anno, ?MA_INTERFACE, Interface).

%%set_read(Read, Anno) when is_boolean(Read) ->
%%  set_anno_val(Anno, ?MA_READ, Read).

set_modality(Modality, Anno) when Modality =:= 'new'; Modality =:= 'use' ->
  set_anno_val(Anno, ?MA_MODALITY, Modality).

set_state(State, Anno) when is_list(State) ->
  set_anno_val(Anno, ?MA_STATE, State).

set_anno_val(Anno, Item, Value) ->
  Anno0 =
    case Anno of
      Line when is_integer(Line) ->
        [{location, Line}];
      {Line, Column} when is_integer(Line), is_integer(Column) ->
        [{location, {Line, Column}}];
      Anno when is_list(Anno) ->
        Anno;
      _ ->
        erlang:error(badarg, [Anno, Item, Value])
    end,
  lists:keystore(Item, 1, Anno0, {Item, Value}).

get_anno_val(Anno, Item, Default) ->
  case Anno of
    Line when is_integer(Line) ->
      undefined;
    {Line, Column} when is_integer(Line), is_integer(Column) ->
      undefined;
    Anno when is_list(Anno) ->
      case lists:keyfind(Item, 1, Anno) of
        false ->
          Default;
        {Item, Value} ->
          Value
      end;
    _ ->
      erlang:error(badarg, [Anno, Item, Default])
  end.


map_anno(Fun, Tree) ->
  map_anno(Fun, Tree, 0).

map_anno(Fun, Tree, Depth) ->
  {Tree0, _} = erl_parse:mapfold_anno(
    fun(Anno, D) when D == Depth ->
      {Fun(Anno), D + 1};
      (Anno, D) -> {Anno, D + 1}
    end,
    0, Tree),
  Tree0.

map_anno_all(Anno, Tree) ->
  map_anno_lt(Anno, Tree, 576460752303423488).

map_anno_lt(Fun, Tree, Depth) ->
  erl_parse:mapfold_anno(
    fun(Anno, D) when D < Depth ->
      {Fun(Anno), D + 1};
      (Anno, D) -> {Anno, D + 1}
    end,
    0, Tree).


get_fun_ref_mfa([M, F, Args]) ->
  case erl_syntax:type(M) of
    atom ->
      case erl_syntax:type(F) of
        atom ->
          case erl_syntax:type(Args) of
            Type when Type =:= nil; Type =:= list ->
              {ok, erl_syntax:atom_value(M), {erl_syntax:atom_value(F),
                erl_syntax:list_length(Args)}};
            _ ->
              {error, Args}
          end;
        _ ->
          {error, F}
      end;
    _ ->
      {error, M}
  end.


get_fun_ref(Call) ->
  maybe
    application ?= erl_syntax:type(Call),
    Operator = erl_syntax:application_operator(Call),

    atom ?= erl_syntax:type(Operator),
    Args = erl_syntax:application_arguments(Call),
    {ok, {erl_syntax:atom_value(Operator), length(Args)}}
  else
    _ ->
      % Not a static call.
      {error, Call};
    module_qualifier ->
      % Remote calls unsupported.
      {error, Call}
  end.


%%  {erl_syntax:atom_value(M), erl_syntax:atom_value(F), erl_syntax:list_length(Args)}.
%%  {erl_syntax:atom_value(F), erl_syntax:list_length(Args)}.

%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
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



is_mb_in_scope(_, undefined) ->
  false;
is_mb_in_scope(MbName, MbScopes) when is_atom(MbName), is_list(MbScopes) ->
  lists:member(MbName, MbScopes).

is_fun_ref_in_scope(FunRef = {_, _}, FunScopes) when is_list(FunScopes) ->
  lists:member(FunRef, FunScopes).





