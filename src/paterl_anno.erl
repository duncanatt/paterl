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
%%% Helpers.
%%% ----------------------------------------------------------------------------

%% @private Returns the list of AST nodes that correspond to the mailbox
%% interface types.
%%
%% Nested message references are recursively expanded in place. Note that
%% currently, mutually-recursive type definitions result in an infinite loop.
%% This is something that can be fixed later.
%%
%% @param TypesCtx: Mailbox types context.
%%
%% @returns List of interface AST nodes.
-spec get_interfaces(paterl_types:type_defs()) -> [erl_syntax:syntaxTree()].
get_interfaces(TypeDefs) when is_map(TypeDefs) ->
  lists:reverse(
    erl_syntax:revert_forms(
      maps:fold(
        fun(Name, {?T_MBOX, Anno, _, Vars = []}, Attrs) ->
          ?TRACE("Create interface for mailbox '~p'.", [Name]),

          MsgTypesAS =
            case expand_msg_type_set(Name, TypeDefs) of
              [Type] ->
                % Mailbox type is a singleton.
                Type;
              Types when is_list(Types) ->
                % Mailbox type is a list that must be recombined as a type union.
                erl_syntax:revert(erl_syntax:set_pos(erl_syntax:type_union(Types), Anno))
            end,

          ?TRACE("Expanded mailbox interface type '~s() :: ~s'.", [Name, erl_prettypr:format(MsgTypesAS)]),

          Interface = erl_syntax:set_pos(
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
    )
  ).

%% @private Expands message types to their full tuple definition form.
-spec expand_msg_type_set(atom(), paterl_types:type_defs()) -> [erl_syntax:syntaxTree()].
expand_msg_type_set(Name, TypeDefs) ->
  % Type variables not supported.
  {?T_MBOX, _, Type, _Vars = []} = maps:get(Name, TypeDefs),
  lists:reverse(expand_msg_type(Type, TypeDefs, [])).

%% @private Expands a message type to its full definition.
-spec expand_msg_type(tuple(), paterl_types:type_defs(), [erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
expand_msg_type(Type = {type, _, pid, []}, TypeDefs, Acc) when is_map(TypeDefs) ->
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
  % User-defined type. Denotes another message type which is a message set
  % alias.
  ?assertEqual(erl_syntax:type(_Type), type_union),
  ?TRACE("Inspect type '~s'.", [erl_prettypr:format(_Type)]),
  lists:foldl(
    fun(Type0, Acc0) ->
      expand_msg_type(Type0, TypeDefs, Acc0)
    end,
    Acc, Union);
expand_msg_type(_Type = {user_type, _, Name, []}, TypeDefs, Acc) when is_map(TypeDefs) ->
  % User-defined type union. Check message set validity. This case is handled
  % using check_msgs_types because MsgTypes is a list.
  ?TRACE("Inspect type '~s'.", [erl_prettypr:format(_Type)]),
  {?T_TYPE, _, UserType, _Vars = _} = maps:get(Name, TypeDefs),
  expand_msg_type(UserType, TypeDefs, Acc).

%% @private Annotates forms.
-spec annotate_forms(erl_syntax:forms(), paterl_types:type_info(), errors:error()) ->
  {erl_syntax:forms(), errors:error()}.
annotate_forms([], _, Analysis) ->
  % Empty forms.
  Analysis#analysis{result = []};

annotate_forms([Form = {attribute, _, module, _} | Forms], TypeInfo = #type_info{type_defs = TypeDefs}, Analysis) ->
  % Module attribute.
  Interfaces = get_interfaces(TypeDefs),
  ?TRACE("Interfaces: ~p", [Interfaces]),

  Analysis0 = annotate_forms(Forms, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Interfaces ++ Analysis0#analysis.result]};
%%  {Forms0, Analysis0} = annotate_forms(Forms, TInfo, Analysis),
%%  {[Mod | Interfaces ++ Forms0], Analysis0};

annotate_forms([Form = {attribute, _, export, _} | Forms], TypeInfo, Analysis) ->
  % Export attribute. Leave in so that Erlang compiler compiles successfully.
  Analysis0 = annotate_forms(Forms, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Analysis0#analysis.result]};
%%  {Forms0, Analysis0} = annotate_forms(Forms, TInfo, Analysis),
%%  {[Export | Forms0], Analysis0};

annotate_forms([Form = {attribute, _, import, _} | Forms], TypeInfo, Analysis) ->
  % Import attribute. Leave in so that Erlang compiler compiles successfully.
  Analysis0 = annotate_forms(Forms, TypeInfo, Analysis),
  Analysis0#analysis{result = [Form | Analysis0#analysis.result]};
%%  {Forms0, Analysis0} = annotate_forms(Forms, TInfo, Analysis),
%%  {[Import | Forms0], Analysis0};

annotate_forms([{attribute, _, _, _} | Forms], TypeInfo, Analysis) ->
  % Eat other attributes.
  annotate_forms(Forms, TypeInfo, Analysis);

annotate_forms([Form = {function, Anno, Name, Arity, Clauses} | Forms], TypeInfo, Analysis) ->
  % Function.
  Analysis0 = annotate_function(Form, TypeInfo, Analysis),
  Analysis1 = annotate_forms(Forms, TypeInfo, Analysis0),
  Analysis1#analysis{
    result = [Analysis0#analysis.result | Analysis1#analysis.result]
  };
%%  {Form0, Analysis0} = annotate_function(Form, TypeInfo, Analysis),
%%  {Forms0, Analysis1} = annotate_forms(Forms, TypeInfo, Analysis0),
%%  {[Form0 | Forms0], Analysis1};

annotate_forms([_ | Forms], TypeInfo, Analysis) ->
  % Eat other forms.
  annotate_forms(Forms, TypeInfo, Analysis).


%% @private Annotates functions with a single clause and without guards.
-spec annotate_function(erl_syntax:syntaxTree(), paterl_types:type_info(), errors:error()) ->
  {erl_syntax:syntaxTree(), errors:error()}.
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

    % Determine if function has mailbox scope.
%%    {Anno0, Analysis0} =
%%    {Clauses0, Anno0, Analysis0} =
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
%%          {Clauses1, Analysis1} = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TypeInfo, Analysis),

%%          {Clauses1, Anno1, Analysis1};
%%        {Anno1, Analysis1};
        ?TRACE("Analysis1#analysis.result = ~p", [Analysis1#analysis.result]),
        % Annotate function clauses.
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
%%          {Clauses1, Analysis1} = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TypeInfo, Analysis),
%%          {Clauses1, Anno, Analysis1}
%%        {Anno, Analysis1}
        Form0 = erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Analysis1#analysis.result)),
        Analysis1#analysis{result = Form0}
    end%,

%%    Form0 = map_anno(fun(_) -> Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Clauses0))),
%%    Form0 = map_anno(fun(_) ->
%%      Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Analysis0#analysis.result))),
%%    {Form0, Analysis0}
%%    Analysis0#analysis{result = Form0}
  else
    undefined_spec ->
      % Non-existent function spec. Should not happen because every fun
      % reference must have a corresponding spec.
      ErrNode = paterl_syntax:fun_reference(FunRef, Anno),

      ?ERROR("Function '~s' has missing spec.", [erl_prettypr:format(ErrNode)]),
      ?pushError(?E_UNDEF__FUN_SPEC, ErrNode, Analysis)
  end.

%%  % Signature must be defined in specs ctx since every function is annotated
%%  % with function specs.
%%  ?assert(maps:is_key(FunRef, SpecDefs)),
%%  {spec, _, Types} = maps:get(FunRef, SpecDefs),
%%
%%  ?INFO("Clauses length ~p", [length(Clauses)]),
%%  ?INFO("Types length ~p", [Types]),
%%
%%  % Only one function clause and spec anno permitted.
%%  ?assert(length(Clauses) == 1 andalso length(Types) == 1),
%%
%%  % Function may or may not be decorated with mailbox annotations.
%%  {Clauses0, Anno0, Error0} =
%%    case maps:get(FunRef, MbFuns, undefined) of
%%      {Modality, _, MbName} ->
%%        % Mailbox annotated function.
%%        ?TRACE("Annotating function '~p/~p' with mailbox scope '~p'", [Name, Arity, MbName]),
%%        {Clauses1, Error1} = annotate_clauses(Clauses, Types, FunRef, MbName, TypeInfo, Error),
%%        Anno1 = set_modality(Modality, set_interface(MbName, Anno)),
%%        {Clauses1, Anno1, Error1};
%%      undefined ->
%%        % Non-annotated function.
%%        ?TRACE("Annotating function '~p/~p'", [Name, Arity]),
%%        {Clauses1, Error1} = annotate_clauses(Clauses, Types, FunRef, undefined, TypeInfo, Error),
%%        {Clauses1, Anno, Error1}
%%    end,

%%  Form0 = map_anno(fun(_) -> Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Clauses0))),
%%  {Form0, Error0}.

%% @private Annotates a function clause list.
-spec annotate_fun_clauses([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()], paterl_types:fun_ref(), atom(), paterl_types:type_info(), errors:error()) ->
  {[erl_syntax:syntaxTree()], errors:error()}.
annotate_fun_clauses([], [], _, _, _, Analysis) ->
%%  {[], Analysis};
  % Empty function clauses.
  ?TRACE("Analysis in empty fun clauses = ~p", [Analysis]),
  Analysis#analysis{result = []};
annotate_fun_clauses([Clause | Clauses], [Type | Types], FunScopes, MbScopes, TInfo, Analysis) ->
  Analysis0 = annotate_fun_clause(Clause, Type, FunScopes, MbScopes, TInfo, Analysis),
  Analysis1 = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TInfo, Analysis0),
  Analysis1#analysis{result =
  [Analysis0#analysis.result | Analysis1#analysis.result]
  }.
%%  {Clause0, Analysis0} = annotate_fun_clause(Clause, Type, FunScopes, MbScopes, TInfo, Analysis),
%%  {Clauses0, Analysis1} = annotate_fun_clauses(Clauses, Types, FunScopes, MbScopes, TInfo, Analysis0),
%%  {[Clause0 | Clauses0], Analysis1}.

%% @private Annotates a function clause.
%%
%% The following spec-annotated clauses are annotated:
%% 1. Function clause without guard
-spec annotate_fun_clause(erl_syntax:syntaxTree(), erl_syntax:syntaxTree(), paterl_types:fun_ref(), atom(), paterl_types:type_info(), errors:error()) ->
  {erl_syntax:syntaxTree(), errors:error()}.
annotate_fun_clause({clause, Anno, PatSeq, GuardSeq = [], Body},
    _ArgType = {type, _, 'fun', [{type, _, product, TypeSeq}, RetType]}, FunScopes, MbScopes, TypeInfo, Analysis)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq),
  is_list(Body) ->
  % Unguarded function clause.
  ?DEBUG("Annotate unguarded function clause '~s :: ~s'.", [
    erl_prettypr:format(erl_syntax:clause(PatSeq, GuardSeq, [erl_syntax:underscore()])),
    erl_prettypr:format(RetType)
  ]),

  % Annotate function pattern sequence and set function return type.
  AnnPatSeq = annotate_pat_seq(PatSeq, TypeSeq, MbScopes, TypeInfo),
  Anno0 = set_type(RetType, Anno),

  % Set mailbox interface if function inside mailbox scope.
  Anno1 =
    if
      MbScopes =:= undefined -> Anno0; true ->
      set_interface(hd(MbScopes), Anno0) % TODO: This hd() would be removed when we tackle multiple mailboxes
    end,

  % Annotate function body.
%%  {Body0, Analysis0} = annotate_expr_seq(Body, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis0 = annotate_expr_seq(Body, FunScopes, MbScopes, TypeInfo, Analysis),
%%  Clause0 = paterl_syntax:set_anno(
%%    erl_syntax:clause(AnnPatSeq, GuardSeq, Body0), Anno1
%%  ),
  ?TRACE("Analysis0#analysis.result = ~p", [Analysis0#analysis.result]),
  Clause0 = paterl_syntax:set_anno(
    erl_syntax:clause(AnnPatSeq, GuardSeq, Analysis0#analysis.result), Anno1
  ),
%%  {Clause0, Analysis0};
  Analysis0#analysis{result = Clause0};
annotate_fun_clause({clause, Anno, PatSeq, GuardSeq, _}, _ArgType, _FunScopes, _MbScopes, _, Analysis) ->
  % Guarded function clause. Unsupported.
  ErrNode = paterl_syntax:set_anno(
    erl_syntax:clause(PatSeq, GuardSeq, [erl_syntax:underscore()]), Anno
  ),
  ?ERROR("Unsupported guarded function clause '~s'.", [
    erl_prettypr:format(ErrNode)
  ]),
  ?TRACE("Analysis#analysis.result = ~p", [Analysis#analysis.result]),
%%  {Clause, ?pushError(?E_BAD__CLAUSE, ErrNode, Analysis)}.
  ?pushError(?E_BAD__CLAUSE, ErrNode, Analysis#analysis{result = ErrNode}).


%% @private Annotates a non-function clause list.
-spec annotate_clauses([erl_syntax:syntaxTree()], paterl_syntax:fun_ref(), atom(), paterl_types:type_info(), errors:error()) ->
  {[erl_syntax:syntaxTree()], errors:error()}.
annotate_clauses([], _, _, _, Analysis) ->
%%  {[], Analysis};
  Analysis#analysis{result = []};
annotate_clauses([Clause | Clauses], Signature, MbScope, TypeInfo, Analysis) ->
  ?TRACE("Annotating clause ~p", [Clause]),
  Analysis0 = annotate_clause(Clause, Signature, MbScope, TypeInfo, Analysis),
  Analysis1 = annotate_clauses(Clauses, Signature, MbScope, TypeInfo, Analysis0),
  Analysis1#analysis{result =
  [Analysis0#analysis.result | Analysis1#analysis.result]
  }.
%%  {Clause0, Analysis0} = annotate_clause(Clause, Signature, MbScope, TInfo, Analysis),
%%  {Clauses0, Analysis1} = annotate_clauses(Clauses, Signature, MbScope, TInfo, Analysis0),
%%  {[Clause0 | Clauses0], Analysis1}.

%% @private Annotates a non-function clause.
%%
%% The following clauses are annotated:
%% 1. If
-spec annotate_clause(erl_syntax:syntaxTree(), paterl_syntax:fun_ref(), atom(), paterl_types:type_info(), errors:error()) ->
  {erl_syntax:syntaxTree(), errors:error()}.
annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    Signature, MbScope, TypeInfo, Analysis)
  when is_list(GuardSeq), is_list(Body) ->
  % If clause.
%%  ?TRACE("If clause: ~p", [Clause]),
  ?TRACE("Annotate clause '~s'.", [erl_syntax:type(Clause)]),

  Analysis0 = annotate_expr_seq(Body, Signature, MbScope, TypeInfo, Analysis),
%%  {Body0, Analysis0} = annotate_expr_seq(Body, Signature, MbScope, TypeInfo, Analysis),
%%  Clause0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:clause(GuardSeq, Body0), Anno)
%%  ),
  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(GuardSeq, Analysis0#analysis.result), Anno)
  ),
%%  {Clause0, Analysis0};
  Analysis0#analysis{result = Clause0};

annotate_clause(Clause = {clause, Anno, PatSeq = [_], GuardSeq = [], Body}, Signature, MbScope, TypeInfo, Analysis) ->
  % Receive or case clause
%%  ?TRACE("Receive clause: ~p", [Clause]),
  ?TRACE("Annotate clause '~s'.", [erl_syntax:type(Clause)]),

  Analysis0 = annotate_expr_seq(Body, Signature, MbScope, TypeInfo, Analysis),
%%  {Body0, Analysis0} = annotate_expr_seq(Body, Signature, MbScope, TypeInfo, Analysis),
%%  Clause0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:clause(PatSeq, GuardSeq, Body0), Anno)
%%  ),
  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(PatSeq, GuardSeq, Analysis0#analysis.result), Anno)
  ),
%%  {Clause0, Analysis0};
  Analysis0#analysis{result = Clause0};

%% The following clauses are not annotated:
%% 1. Case
%% 2. Case with guard
%% 3. Catch
annotate_clause(Clause, _Signature, _MbScope, _, Analysis) ->
  ?TRACE("Skip clause '~s'.", [erl_syntax:type(Clause)]),
%%  {Clause, Analysis}.
  Analysis.

%% @private Annotates a pattern list.
-spec annotate_pat_seq([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()], atom(), paterl_types:type_info()) ->
  [erl_syntax:syntaxTree()].
annotate_pat_seq(PatSeq, TypeSeq, MbScope, TypeInfo)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq) ->
  lists:zipwith(fun(Pat, Type) -> annotate_pat(Pat, Type, MbScope, TypeInfo) end, PatSeq, TypeSeq).

annotate_pat(Pat, Type = {Qualifier, _, _, _}, _MbScope, _)
  when Qualifier == type; Qualifier == user_type ->
  ?TRACE("Annotate function pattern '~s :: ~s'.", [
    erl_prettypr:format(Pat), erl_prettypr:format(Type)
  ]),
  map_anno(fun(Anno) -> set_type(Type, Anno) end, Pat).
%% HERE tyring to get rid of map_anno. I don't think it can be done at this
%% stage.


%% TODO: I have decided that when error messages are present, [] should be returned.

%% @private Annotate an expression list.
%%
%% The function takes into account the mailbox-annotation expressions (which the
%% programmer introduces as macros) that decorate expressions. Error messages
%% are generated according to whether said mailbox-annotation expressions appear
%% in the expected position within an expression list. The following conditions
%% hold in a list of mailbox-annotation and Erlang expressions:
%% 1. A mailbox-annotation expression cannot occur at the end of an expression
%%    list
%% 2. The RHS of a match expression cannot be a mailbox-annotation unless said
%%    match is followed by another expression in the list
%% 3. Two successive mailbox-annotation expressions cannot occur in an
%%    expression list
%%
%% In all other cases, the mailbox-annotation expression is eaten up and the
%% type information contained in the annotation is used to annotate the next
%% expression in the expression list.
%%
%% Match expressions are handled different to other Erlang expressions depending
%% on whether the RHS of a match is a mailbox-annotation or otherwise. In the
%% former case, the mailbox-annotation AST node is replaced with the AST node of
%% the expression that follows the match expression in the expression list.
%% Otherwise, the AST of the match expression remains unmodified and its RHS
%% AST is annotated.
-spec annotate_expr_seq([erl_syntax:syntaxTree()], Signature :: paterl_types:fun_ref(), MbScope :: atom(), paterl_types:type_info(), errors:error()) ->
  {[erl_syntax:syntaxTree()], errors:error()}.
annotate_expr_seq([], _, _, _, Analysis) ->
%%  {[], Analysis};
  % Empty expression sequence.
  Analysis#analysis{result = []};

annotate_expr_seq([_MbAnno = {tuple, Anno, [{atom, _, Name}, {_, _, Value}]}], _, _, _, Analysis) ->
  % Annotation at end of expression list.
  ErrNode = paterl_syntax:mb_anno(Name, [Value], Anno),
  ?ERROR("Expected expression after annotation '~s'.", [
    erl_prettypr:format(ErrNode)
  ]),
%%  {[], ?pushError(?E_EXPECTED_EXPR, Node, Analysis)};
  ?pushError(?E_EXP__EXPR, ErrNode, Analysis#analysis{result = []});

%%annotate_expr_seq(Expr = [{match, _, Pat, _MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]}}], MbScope, TInfo, Error) ->
%%  % Mailbox-annotation as match RHS, which is not followed by another expression.
%%  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
%%  {[], ?pushError(?E_EXPECTED_EXPR, Macro, Error)};

annotate_expr_seq([_MbAnno = {tuple, _, [{atom, _, Name}, {_, _, Value}]}, MbAnno = {tuple, Anno, [{atom, _, _}, {_, _, _}]} | ExprSeq], Signature, MbScope, TInfo, Analysis) ->
  % Two successive annotations.
  ErrNode = paterl_syntax:mb_anno(Name, [Value], Anno),
  ?ERROR("Expected expression after annotation '~s'.", [
    erl_prettypr:format(ErrNode)
  ]),
  Analysis0 = ?pushError(?E_EXP__EXPR, ErrNode, Analysis),
%%  {_, Analysis1} = annotate_expr_seq(ExprSeq, Signature, MbScope, TInfo, Analysis0),
  Analysis1 = annotate_expr_seq(ExprSeq, Signature, MbScope, TInfo, Analysis0),
  Analysis1;
%%  {[], Analysis1};

%%annotate_expr_seq([{match, Anno, Pat, _MbAnno = {tuple, _, [{atom, _, Name}, {_, _, Value}]}}, Expr | ExprSeq], MbScope, TInfo, Error) ->
%%  % RHS of match expression is a mailbox-annotation expression. Use mailbox-
%%  % annotation name and value to annotate the AST node of the next expression in
%%  % the expression list. At this point, the next expression must exist. Replace
%%  % the mailbox-annotation in RHS of match with the AST of the newly annotated
%%  % (next) expression.
%%  Match0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr), Anno)
%%  ),
%%
%%  {Expr1, Error0} = annotate_expr(Match0, {Name, Value}, MbScope, TInfo, Error),
%%  {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
%%  {[Expr1 | ExprSeq1], Error1};

% TODO: Can be safely removed. The general function annotate_expr_seq performs the role of a lookahead.
%%annotate_expr_seq([Match = {match, _, _Pat, _Expr} | ExprSeq], Signature, MbScope, TInfo, Analysis) ->
%%  % RHS of match expression is a non mailbox-annotated expression.
%%  {Expr1, Analysis0} = annotate_expr(Match, undefined, Signature, MbScope, TInfo, Analysis),
%%  {ExprSeq1, Analysis1} = annotate_expr_seq(ExprSeq, Signature, MbScope, TInfo, Analysis0),
%%  {[Expr1 | ExprSeq1], Analysis1};

%%annotate_expr_seq([_MbAnno = {tuple, _Anno, [{atom, _, Name}, {_, _, Value}]}, Expr | ExprSeq], Signature, MbScope, TInfo, Error) ->
%%  % Annotated expression.
%%  ?TRACE("Annotation '~s' on expression '~s'.", [
%%    erl_prettypr:format(paterl_syntax:mb_anno(Name, [Value], _Anno)),
%%    erl_prettypr:format(Expr)
%%  ]),
%%
%%  MbAnno = {Name, Value}, %TODO: Include annotation as well for error reporting purposes.
%%  {Expr1, Error0} = annotate_expr(Expr, MbAnno, Signature, MbScope, TInfo, Error),
%%  {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, Signature, MbScope, TInfo, Error0),
%%  {[Expr1 | ExprSeq1], Error1};

% TODO: Consolidate this with annotations with @
annotate_expr_seq([_MbAnno = {tuple, _Anno, [{atom, _, Name} | Args]}, Expr | ExprSeq], Signature, MbScope, TypeInfo, Analysis) ->
  % Annotation.
  MbAnnoArgs = paterl_syntax:mb_anno_args(_MbAnno),

  ?TRACE("Annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(Name, MbAnnoArgs, _Anno)),
    erl_syntax:type(Expr)
  ]),

  MbAnno0 = list_to_tuple([paterl_syntax:mb_anno_name(_MbAnno) | MbAnnoArgs]), %TODO: Include annotation anno() as well for error reporting purposes.
  ?TRACE("MbAnno0 = ~p", [MbAnno0]),

  Analysis0 = annotate_expr(Expr, MbAnno0, Signature, MbScope, TypeInfo, Analysis),
  Analysis1 = annotate_expr_seq(ExprSeq, Signature, MbScope, TypeInfo, Analysis0),
  Analysis1#analysis{result = [
    Analysis0#analysis.result | Analysis1#analysis.result
  ]};
%%  {Expr1, Analysis0} = annotate_expr(Expr, MbAnno0, Signature, MbScope, TypeInfo, Analysis),
%%  {ExprSeq1, Analysis1} = annotate_expr_seq(ExprSeq, Signature, MbScope, TypeInfo, Analysis0),
%%  {[Expr1 | ExprSeq1], Analysis1};

annotate_expr_seq([Expr | ExprSeq], Signature, MbScope, TypeInfo, Analysis) ->
  % Expression.
  Analysis0 = annotate_expr(Expr, undefined, Signature, MbScope, TypeInfo, Analysis),
  Analysis1 = annotate_expr_seq(ExprSeq, Signature, MbScope, TypeInfo, Analysis0),
  ?TRACE("Analysis0#analysis.result = ~p", [Analysis0#analysis.result]),
  ?TRACE("Analysis1#analysis.result = ~p", [Analysis1#analysis.result]),
  Analysis1#analysis{result = [
    Analysis0#analysis.result | Analysis1#analysis.result
  ]}.
%%  {Expr1, Analysis0} = annotate_expr(Expr, undefined, Signature, MbScope, TypeInfo, Analysis),
%%  {ExprSeq1, Analysis1} = annotate_expr_seq(ExprSeq, Signature, MbScope, TypeInfo, Analysis0),
%%  {[Expr1 | ExprSeq1], Analysis1}.


%% @private Annotates expressions.
%%
%% Error messages are generated according to whether a mailbox-annotation is
%% expected. The following conditions hold:
%% 1. A spawn expression must be mailbox-annotated.
%% 2. A self expression must not be mailbox-annotated. Assuming one mailbox
%%    we can easily infer the name of the mailbox interface.
%% 3. An explicit function call (where the function name is an atom) may or may
%%    not be mailbox-annotated. If it is not, the mailbox-annotation is inferred
%%    from the map of types, otherwise the given annotation must correspond to
%%    the one inferred from the map of types.
%% 4. An implicit function call (where the function name is the result of an
%%    expression) must be mailbox-annotated since it cannot be inferred.
%% 5. A receive expression must be mailbox-annotated.
%% 6. Match expressions may or may not be mailbox-annotated.
%% 7. Any other expression must not be annotated.
%%
%% The following expressions are annotated:
%% 1. Mailbox-annotated spawn call
%% 2. Non mailbox-annotated self call
%% 3. (Non) mailbox-annotated internal function call
%% 4. If expression
%% 5. Match operator
%% 6. Receive without timeout
-spec annotate_expr(Expr, MbAnno, Signature, MbScope, TInfo, Error) ->
  {erl_syntax:syntaxTree(), errors:error()}
  when
  Expr :: erl_syntax:syntaxTree(),
  MbAnno :: {atom(), atom() | string()} | undefined, %TODO: Enumerate the Mb-annotations, state | use | new, etc.
  Signature :: any(), %paterl_syntax:signature(),
  MbScope :: atom(),
  TInfo :: paterl_types:type_info(),
  Error :: errors:error().

annotate_expr(Call = {call, Anno, Operator = {atom, _, spawn}, MFArgs}, undefined, _FunScopes, _MbScopes, TypeInfo, Analysis) ->
  % Unannotated spawn expression. MFArgs can be a static or dynamic function.
  % Static functions in spawn expressions must be unannotated, whereas dynamic
  % functions must be annotated. The latter are unsupported.
  case get_fun_ref_mfa(MFArgs) of
    {ok, _, FunRef = {_, _}} ->
      % MFArgs is a static function.
      ?DEBUG("Annotate '~s'.", [erl_prettypr:format(Call)]),

      maybe
      % Check fun reference is defined.
        {spec, _, _} ?= paterl_types:spec_def(FunRef, TypeInfo),

        % Check mailbox interface is defined.
        {_MbMod, _Anno, MbName} ?= paterl_types:mb_fun(FunRef, TypeInfo),

        % Override modality with ?new for spawn call.
        ?DEBUG("Override '~s' annotation with '~s' on '~s'.", [
          erl_prettypr:format(paterl_syntax:mb_definition(_MbMod, [MbName], _Anno)),
          erl_prettypr:format(paterl_syntax:mb_anno(?MOD_NEW, [MbName], Anno)),
          erl_prettypr:format(Call)
        ]),
        Anno0 = set_modality(?MOD_NEW, set_interface(MbName, Anno)),
        Expr0 = erl_syntax:revert(
          erl_syntax:set_pos(erl_syntax:application(Operator, MFArgs), Anno0)
        ),
%%        {Expr0, Analysis}
        Analysis#analysis{result = Expr0}
      else
        undefined_spec ->
          % Undefined fun reference.
          ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
          ?ERROR("Undefined function '~s'.", [erl_prettypr:format(ErrNode)]),
%%          {Call, ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis)};
          ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis#analysis{result = ErrNode});
        undefined_mb ->
          % Undefined mailbox interface.
          ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
          ?ERROR("Undefined mailbox interface for '~s'.", [
            erl_prettypr:format(ErrNode)
          ]),
%%          {Call, ?pushError(?E_UNDEF__MB, ErrNode, Analysis)}
          ?pushError(?E_UNDEF__MB, ErrNode, Analysis#analysis{result = ErrNode})
      end;
    {error, Term} ->
      % MFArgs is not a static function.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Term), erl_prettypr:format(Call)
      ]),
%%      {Call, ?pushError(?E_BAD__EXPR, Term, Analysis)}
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Term})
  end;
annotate_expr(Call = {call, Anno, {atom, _, OpName = spawn}, _MFArgs}, MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated spawn expression.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(MbAnno)),
    erl_prettypr:format(Call)
  ]),
  ErrNode = paterl_syntax:application(OpName, [], Anno),
%%  {Call, ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis)};
  ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis#analysis{result = ErrNode});

annotate_expr(Call = {call, _, {atom, _, self}, []}, _MbAnno, _FunScopes, _MbScopes = undefined, _, Analysis) ->
  % Annotated self expression outside mailbox scope.
  ?ERROR("'~s' not in mailbox interface scope.", [erl_prettypr:format(Call)]),
%%  {Call, ?pushError(?E_NO__MB_SCOPE, Call, Analysis)};
  ?pushError(?E_NO__MB_SCOPE, Call, Analysis#analysis{result = Call});
annotate_expr(_Call = {call, Anno, Self = {atom, _, self}, []}, _MbAnno = undefined, _FunScopes, MbScopes, _, Analysis) ->
  % Unannotated self expression inside mailbox interface scope. Mailbox
  % interface name can be inferred from the enclosing mailbox scope.
  % TODO: Eventually remove? No. If the mailbox scope is a singleton list, then
  % you can infer it so don't enforce it on the user, but if not, then error out
  % and the user must specify it via the ?as annotation.
  ?TRACE("Annotate '~s' with inferred mailbox interface '~s'.", [
    erl_prettypr:format(_Call), hd(MbScopes)
  ]),
  Anno0 = set_interface(hd(MbScopes), Anno),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Self, []), Anno0)
  ),
%%  {Expr0, Analysis};
  Analysis#analysis{result = Expr0};
annotate_expr(Call = {call, Anno, Self = {atom, _, self}, []}, {?ANNO_AS, MbName}, _FunScopes, MbScopes, _, Analysis) ->
  % Annotated self expression inside mailbox scope. Mailbox interface inferred
  % from the enclosing mailbox scope must match the mailbox interface in the
  % annotation.
  case is_mb_in_scope(MbName, MbScopes) of
    true ->
      % Mailbox interface name in scope.
      ?TRACE("Annotate '~s' with inferred matching mailbox interface '~s'.", [
        erl_prettypr:format(Call), hd(MbScopes)
      ]),
      Anno0 = set_interface(hd(MbScopes), Anno),
      Expr0 = erl_syntax:revert(
        erl_syntax:set_pos(erl_syntax:application(Self, []), Anno0)
      ),
%%      {Expr0, Analysis};
      Analysis#analysis{result = Expr0};
    false ->
      % Mailbox interface name out of scope.
      ?ERROR("Mailbox interface '~s' not in scope.", [MbName]),
%%      {Call, ?pushError(?E_UNDEF__MB_SCOPE, paterl_syntax:name(MbName, Anno), Analysis)}
      ?pushError(?E_UNDEF__MB_SCOPE, paterl_syntax:name(MbName, Anno), Analysis#analysis{result = Call})
  end;
annotate_expr(Call = {call, Anno, {atom, _, self}, []}, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated self expression with invalid annotation (i.e., not ?as).
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Call)
  ]),
%%  {Call, ?pushError(?E_BAD__ANNO_ON, Call, Analysis)};
  ?pushError(?E_BAD__ANNO_ON, Call, Analysis#analysis{result = Call});


annotate_expr(Call = {call, Anno, Operator, Exprs}, _MbAnno = undefined, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated local function call.
%%  ?TRACE("Annotate '~s'.", [erl_prettypr:format(_Call)]),
  case get_fun_ref(Call) of
    {ok, FunRef = {_, _}} ->
      % Static function call.
%%      ?DEBUG("Annotate '~s'.", [erl_prettypr:format(Call)]),

      % Check if fun reference is defined.
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
            % Undefined fun reference.
            ErrNode = paterl_syntax:fun_reference(FunRef, Anno),
            ?ERROR("Undefined function '~s'.", [erl_prettypr:format(ErrNode)]),
%%            {Call, ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis)}
            {Anno, ?pushError(?E_UNDEF__FUN_REF, ErrNode, Analysis#analysis{result = Call})}
        end,

%%      {Exprs0, Analysis1} = annotate_expr_seq(Exprs, FunScopes, MbScopes, TypeInfo, Analysis0),
      Analysis1 = annotate_expr_seq(Exprs, FunScopes, MbScopes, TypeInfo, Analysis0),
%%      Expr0 = erl_syntax:revert(
%%        erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
%%      ),
      Expr0 = erl_syntax:revert(
        erl_syntax:set_pos(erl_syntax:application(Operator, Analysis1#analysis.result), Anno0)
      ),
%%      {Expr0, Analysis1};
      Analysis1#analysis{result = Expr0};
    {error, Term} ->
      % Non static function call.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Term), erl_prettypr:format(Call)
      ]),
%%      {Call, ?pushError(?E_BAD__EXPR, Term, Analysis)}
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Call})
  end;
annotate_expr(Call = {call, Anno, Operator, Exprs}, _MbAnno, FunScopes, MbScopes, _, Analysis) ->
  % Annotated local function call. Invalid.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Call)
  ]),
%%  {Call, ?pushError(?E_BAD__ANNO_ON, Call, Analysis)};
  ?pushError(?E_BAD__ANNO_ON, Call, Analysis#analysis{result = Call});

%%annotate_expr(_Call = {call, Anno, Operator = {atom, _, Name}, Exprs}, MbAnno, FunScopes, MbScopes,
%%    TypeInfo = #type_info{mb_funs = MbFuns}, Error)
%%  when is_tuple(MbAnno), size(MbAnno) == 2;
%%  MbAnno == undefined ->
%%  % Static local function call. Function call may be anno
%%
%%
%%  % Static function call where function name is an atom. Function call may be
%%  % mailbox-annotated or otherwise. If the function is mailbox-annotated, the
%%  % annotation is cross-checked with the inferred mailbox-annotation of the
%%  % called function definition for consistency. An error is raised if in case of
%%  % mismatch. If the function is not mailbox-annotated, the annotation is
%%  % inferred from the called function definition.
%%  FunRef = {Name, length(Exprs)},
%%  ?TRACE("Annotate '~s'.", [erl_prettypr:format(_Call)]),
%%
%%  {Anno0, Error0} =
%%    case maps:get(FunRef, MbFuns, undefined) of
%%      {Modality, _Anno, MbName} ->
%%        % Called function has an associated mailbox interface.
%%        ?TRACE("Check call to '~s' inside mailbox context '~s'.", [
%%          erl_prettypr:format(_Call), MbName
%%        ]),
%%        case MbAnno of
%%          undefined ->
%%            % Omitted mailbox annotation on function call. Infer mailbox
%%            % interface from associated mailbox interface definition.
%%            ?TRACE("Annotate '~s' with inferred mailbox interface '~s'.", [
%%              erl_prettypr:format(_Call),
%%              erl_prettypr:format(paterl_syntax:mb_anno(Modality, [MbName], Anno))
%%            ]),
%%            {set_modality(Modality, set_interface(MbName, Anno)), Error};
%%          {Modality, MbName} ->
%%            % Defined mailbox annotation on function call matching the
%%            % associated mailbox interface definition.
%%            ?TRACE("Annotate '~s' with user-defined '~s'.", [
%%              erl_prettypr:format(_Call),
%%              erl_prettypr:format(paterl_syntax:mb_anno(Modality, [MbName], Anno))
%%            ]),
%%            {set_modality(Modality, set_interface(MbName, Anno)), Error};
%%          {_Modality, _MbName} -> % TODO: Modify here.
%%            % Defined mailbox annotation on function call not matching the
%%            % associated mailbox interface definition.
%%            ?ERROR("Mailbox annotation '~s' conflicts with mailbox interface definition '~s'.", [
%%              erl_prettypr:format(paterl_syntax:mb_anno(_Modality, [_MbName], Anno)),
%%              erl_prettypr:format(erl_syntax:attribute(erl_syntax:atom(new), [erl_syntax:atom(name)]))
%%            ]),
%%            {Anno, ?pushError(?E_MISMATCH_ANNO, {Modality, Anno, MbName}, Error)}
%%        end;
%%      undefined ->
%%        % Called function does not have an associated mailbox interface.
%%        case MbAnno of
%%          undefined ->
%%            % Undefined mailbox annotation on function call.
%%            ?TRACE("Skip call to '~s' outside mailbox context.", [erl_prettypr:format(_Call)]),
%%            {Anno, Error};
%%          {_Modality, _MbName} ->
%%            % Unexpected mailbox annotation on function call.
%%            ?ERROR("Unexpected mailbox annotation '~s' on call to '~s'.", [
%%              erl_prettypr:format(paterl_syntax:mb_anno(_Modality, [_MbName], Anno)),
%%              erl_prettypr:format(_Call)
%%            ]),
%%            {Anno, ?pushError(?E_BAD__ANNO_ON, _Call, Error)}
%%        end
%%    end,
%%  {Exprs0, Error1} = annotate_expr_seq(Exprs, FunScopes, MbScopes, TypeInfo, Error0),
%%  Expr0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
%%  ),
%%  {Expr0, Error1};
%%annotate_expr(_Expr = {call, Anno, Operator, Exprs}, {AnnoName, MbName}, Signature, MbScope, TInfo, Error) ->
%%  % Dynamic function call where the function name an expression. Function call
%%  % must be annotated since it cannot be inferred from the called function
%%  % definition.
%%  {Exprs0, Error0} = annotate_expr_seq(Exprs, Signature, MbScope, TInfo, Error),
%%  Anno0 = set_modality(AnnoName, set_interface(MbName, Anno)),
%%  Expr0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
%%  ),
%%  {Expr0, Error0};

annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno, FunScopes, MbScopes = undefined, TypeInfo, Analysis) ->
  % Receive expression outside mailbox interface scope. Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:receive_expr([]), Anno),
  ?ERROR("'~s' not in mailbox interface scope.", [erl_prettypr:format(ErrNode)]),
  ?TRACE("Analysis = ~p", [Analysis]),
  Analysis0 = ?pushError(?E_NO__MB_SCOPE, ErrNode, Analysis),

  % Find other possible errors in receive clauses.
%%  {_, Analysis1} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
%%  {Expr, Analysis1};
  Analysis1#analysis{result = Expr};
annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno = undefined, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated receive expression. Invalid.
  ErrNode = paterl_syntax:mb_anno(?ANNO_EXPECTS, [], Anno),
  ?ERROR("expected annotation '~s'.", [erl_prettypr:format(ErrNode)]),
  Analysis0 = ?pushError(?E_EXP__ANNO, ErrNode, Analysis),

  % Find other possible errors in receive clauses.
%%  {_, Analysis1} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
%%  {Expr, Analysis1};
  Analysis1#analysis{result = Expr};
annotate_expr(Expr = {'receive', Anno0, Clauses}, _MbAnno = {?ANNO_EXPECTS, MbName, Pattern}, FunScopes, MbScopes, TypeInfo = #type_info{spec_defs = Specs}, Analysis) ->
  % Annotated receive expression inside mailbox interface scope.
  ?DEBUG("Annotate '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),

  case is_mb_in_scope(MbName, MbScopes) of
    true ->
      % Get function return type from spec. Return type of most recent fun
      % reference is used since it necessarily encloses the receive.
      FunRef = hd(FunScopes),
      {spec, _, [FunType]} = paterl_types:spec_def(FunRef, TypeInfo),
      RetType = erl_syntax:function_type_return(FunType),

      % Annotate receive clauses.
%%      {Clauses0, Analysis0} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),
      Analysis0 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),

      % Function type is used to annotate the receive. Type annotation helps
      % the informs the annotation phase when generating 'free' guard patterns
      % in Pat guard expressions. Type is required since state passing
      % translation is a pair whose first component is a value returned by
      % 'free'. Freeing in Erlang does not exist, which is why the type is set
      % such that 'free' returns a dummy value of the same type in Pat.
      Anno2 = set_type(RetType, set_state(Pattern, set_interface(MbName, Anno0))),
%%      Expr0 = erl_syntax:revert(
%%        erl_syntax:set_pos(erl_syntax:receive_expr(Clauses0), Anno2)
%%      ),
      Expr0 = erl_syntax:revert(
        erl_syntax:set_pos(erl_syntax:receive_expr(Analysis0#analysis.result), Anno2)
      ),
%%      {Expr0, Analysis0};
      Analysis0#analysis{result = Expr0};
    false ->
      % Mailbox interface not in scope. Invalid.
      ErrNode = paterl_syntax:name(MbName, Anno0),
      ?ERROR("mailbox interface '~s' not in scope", [erl_prettypr:format(ErrNode)]),
      Analysis0 = ?pushError(?E_UNDEF__MB_SCOPE, ErrNode, Analysis),

      % Find other possible errors in receive clauses.
%%      {_, Analysis1} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
      Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
%%      {Expr, Analysis1}
      Analysis1#analysis{result = Expr}
  end;
annotate_expr({'receive', Anno, Clauses}, _MbAnno = {state, Regex}, FunScopes, MbScopes, TypeInfo = #type_info{spec_defs = Specs}, Analysis) ->
  % Annotated blocking receive expression inside mailbox interface scope. %TODO: This clause should be removed eventually once the annotations are updated to use ?expects
  % Mailbox name can be inferred from mailbox enclosing scope. FOR NOW!
%%  {Clauses0, Analysis0} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis0 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),

  ?TRACE("Annotating receive with return spec in '~s/~b'.", [element(1, hd(FunScopes)), element(2, hd(FunScopes))]),

  % This pattern match must always succeed since every signature has a type spec
  % associated with it.
  {spec, _, [{type, _, 'fun', [_ParamTypes, ReturnType]}]} = maps:get(hd(FunScopes), Specs),

  % The type of the receive is set to the return type of the function in order
  % to enable us to generate the 'free' guard in Pat receive statements.
  Anno0 = set_type(ReturnType, set_state(Regex, set_interface(hd(MbScopes), Anno))),
%%  Expr0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:receive_expr(Clauses0), Anno0)
%%  ),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:receive_expr(Analysis0#analysis.result), Anno0)
  ),
%%  {Expr0, Analysis0};
  Analysis0#analysis{result = Expr0};
annotate_expr(Expr = {'receive', Anno, Clauses}, _MbAnno, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Annotated self expression with invalid annotation (i.e., not ?as). Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:receive_expr([]), Anno),
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(ErrNode)
  ]),
  Analysis0 = ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis),

  % Find other possible errors in receive clauses.
%%  {_, Analysis1} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
  Analysis1 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis0),
%%  {Expr, Analysis1};
  Analysis1#analysis{result = Expr};

annotate_expr(_Expr = {'if', Anno, Clauses}, _MbAnno = undefined, FunScopes, MbScopes, TypeInfo, Analysis) ->
  % Unannotated if expression.
  ?DEBUG("Annotate '~s'.", [erl_prettypr:format(erl_syntax:if_expr([]))]),
%%  {Clauses0, Analysis0} = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),
  Analysis0 = annotate_clauses(Clauses, FunScopes, MbScopes, TypeInfo, Analysis),
%%  Expr0 = erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:if_expr(Clauses0), Anno)
%%  ),
  Expr0 =
    paterl_syntax:set_anno(erl_syntax:if_expr(Analysis0#analysis.result), Anno),

%%  {Expr0, Analysis0};
  Analysis0#analysis{result = Expr0};
annotate_expr(Expr0 = {'if', Anno, _}, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Annotated if expression. Invalid.
  ErrNode = paterl_syntax:set_anno(erl_syntax:if_expr([]), Anno),
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(ErrNode)
  ]),
%%  {Expr0, ?pushError(?E_BAD__ANNO_ON, ErrNode, Analysis)};
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

%%      {Expr2, Analysis0} = annotate_expr(Expr1, MbAnno, Signature, MbScope, TypeInfo, Analysis),
      Analysis0 = annotate_expr(Expr1, MbAnno, Signature, MbScope, TypeInfo, Analysis),
%%      Expr3 = erl_syntax:revert(
%%        erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr2), Anno)
%%      ),
      Expr3 =
        paterl_syntax:set_anno(erl_syntax:match_expr(Pat, Analysis0#analysis.result), Anno),
%%      {Expr3, Analysis0};
      Analysis0#analysis{result = Expr3};
    _ ->
      % Match pattern not a variable. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Pat),
        erl_prettypr:format(erl_syntax:match_expr(Pat, erl_syntax:underscore()))
      ]),
      Analysis0 = ?pushError(?E_BAD__EXPR, Expr0, Analysis),

      % Find other possible errors in receive clauses.
%%      {_, Analysis1} = annotate_expr(Expr1, MbAnno, Signature, MbScope, TypeInfo, Analysis0),
      Analysis1 = annotate_expr(Expr1, MbAnno, Signature, MbScope, TypeInfo, Analysis0),
%%      {Expr0, Analysis1}
      Analysis1#analysis{result = Expr0}
  end;


% TODO: When we have an ERROR in expression, even if we annotate its innards, we always return the original expression by convention.

%% The following expressions are not annotated:
%% 1. Atomic literal
%% 2. Bitstring comprehension
%% 3. Bitstring constructor
%% 4. Block
%% 5. Case
%% 6. Catch
%% 7. Cons skeleton
%% 8. Internal fun
%% 9. External fun
%% 10. Anonymous fun
%% 11. Named fun
%% 12. External function call
%% 13. List comprehension
%% 14. Map comprehension
%% 15. Map creation
%% 16. Map update
%% 17. Conditional match operator
%% 18. Maybe
%% 19. Maybe else
%% 20. Nil
%% 21. Binary operator
%% 22. Unary operator
%% 23. Parenthesized
%% 24. Receive after
%% 25. Record creation
%% 26. Record field access
%% 27. Record field index
%% 28. Record update
%% 29. Tuple skeleton
%% 30. Try catch
%% 31. Try of catch
%% 32. Try after
%% 33. Try of after
%% 34. Try catch after
%% 35. Try of catch after
%% 36. Variable
annotate_expr(Expr, undefined, _Signature, _MbAnno, _, Analysis) ->
  % Non mailbox-annotated expression.
  ?TRACE("Skip '~s'.", [erl_prettypr:format(Expr)]),
%%  {Expr, Analysis};
  ?TRACE("Analysis result before = ~p", [Analysis#analysis.result]),
  Analysis#analysis{result = Expr};
annotate_expr(Expr, _MbAnno, _FunScopes, _MbScopes, _, Analysis) ->
  % Unexpected annotated expression.
  ?ERROR("Unexpected annotation '~s' on '~s'.", [
    erl_prettypr:format(paterl_syntax:mb_anno(_MbAnno)),
    erl_prettypr:format(Expr)
  ]),
%%  {Expr, ?pushError(?E_BAD__ANNO_ON, Expr, Analysis)}.
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
    "expected expression after annotation '~s'",
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





