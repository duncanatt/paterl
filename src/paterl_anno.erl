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
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%% API
-export([annotate/2]).

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Mailbox annotation types.

%% Mailbox annotation primitive type.
-define(MA_TYPE, type).

%% Mailbox annotation interface.
-define(MA_INTERFACE, interface).

%% Mailbox annotation interface usage modality.
-define(MA_MODALITY, modality).

%% Mailbox annotation regular expression state.
-define(MA_STATE, state).

%%% Error types.

%% Expected mailbox-annotated expression.
-define(E_EXPECTED_EXPR, e_expected_expr).

%% Unexpected mailbox annotation.
-define(E_BAD_ANNO, e_bad_anno).

%% Unexpected mailbox annotation before expression.
-define(E_BAD_ANNO_BEFORE, e_bad_anno_before).

%% Inferred mailbox annotation mismatch.
-define(E_MISMATCH_ANNO, e_mismatch_anno).

%% Expected spawn mailbox annotation.
-define(E_SPAWN_ANNO, e_spawn_anno).

%% Expected receive mailbox annotation.
-define(E_RECEIVE_ANNO, e_receive_anno).


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
-spec annotate([erl_syntax:syntaxTree()], paterl_types:t_info()) ->
  {ok, erl_syntax:forms()} | errors:error().
annotate(Forms, TInfo) when is_list(Forms), is_record(TInfo, t_info) ->
  case annotate_forms(Forms, TInfo, #error{}) of
    {Forms0, #error{errors = []}} ->
      {ok, Forms0};
    {_, #error{errors = Errors}} ->
      #error{errors = lists:reverse(Errors)}
  end.


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
-spec get_interfaces(paterl_types:types()) -> [erl_syntax:syntaxTree()].
get_interfaces(TypesCtx) when is_map(TypesCtx) ->
  lists:reverse(
    erl_syntax:revert_forms(
      maps:fold(
        fun(Mailbox, {mbox, Anno, _, Vars = []}, Attrs) ->
          ?TRACE("Creating interface for mailbox '~p'", [Mailbox]),

          MsgTypesAS =
            case expand_msg_types(Mailbox, TypesCtx) of
              [Type] ->
                % Mailbox type is a singleton.
                Type;
              Types when is_list(Types) ->
                % Mailbox type is a list that must be recombined as a type union.
                erl_syntax:revert(erl_syntax:set_pos(erl_syntax:type_union(Types), Anno))
            end,

          ?TRACE("Expanded mailbox type '~p': ~p", [Mailbox, MsgTypesAS]),

          Interface = erl_syntax:set_pos(
            erl_syntax:attribute(
              erl_syntax:atom(interface),
              [erl_syntax:tuple([
                erl_syntax:atom(Mailbox), % Mailbox type name.
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
        [], TypesCtx)
    )
  ).

%% @private Expands message types to their full tuple definition form.
-spec expand_msg_types(atom(), paterl_types:types()) -> [erl_syntax:syntaxTree()].
expand_msg_types(Name, TypesCtx) ->
  {mbox, _, Type, _TypeVars} = maps:get(Name, TypesCtx),

  % Type variables not supported.
  ?assertMatch(_TypeVars, []),
  lists:reverse(expand_msg_type(Type, TypesCtx, [])).

%% @private Expands a message type to its full definition.
-spec expand_msg_type(tuple(), paterl_types:types(), [erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
expand_msg_type(Type = {type, _, pid, []}, TypesCtx, Acc) when is_map(TypesCtx) ->
  ?assertEqual(erl_syntax:type(Type), type_application),
  ?TRACE("Found pid() type"),
  [Type | Acc];
expand_msg_type(Type = {type, _, tuple, _}, TypesCtx, Acc) when is_map(TypesCtx) ->
  ?assertEqual(erl_syntax:type(Type), tuple_type),
  ?TRACE("Found tuple() type ~p", [Type]),
  [Type | Acc];
expand_msg_type(Type = {type, _, union, Union}, TypesCtx, Acc) when is_map(TypesCtx) ->
  ?assertEqual(erl_syntax:type(Type), type_union),
  ?TRACE("Found union() type ~p", [Type]),
  lists:foldl(
    fun(Type0, Acc0) ->
      expand_msg_type(Type0, TypesCtx, Acc0)
    end,
    Acc, Union);
expand_msg_type(Type = {user_type, _, Name, []}, TypesCtx, Acc) when is_map(TypesCtx) ->
  ?TRACE("Found user_type() type ~p", [Type]),
  {type, _, UserType, _Vars = _} = maps:get(Name, TypesCtx),
  ?TRACE("Expanding user_type() type ~p", [UserType]),
  Typ = expand_msg_type(UserType, TypesCtx, Acc),
  ?TRACE("Expanded user_type() type ~p", [Typ]),
  Typ.


%% @private Annotates forms.
-spec annotate_forms(erl_syntax:forms(), paterl_types:t_info(), errors:error()) ->
  {erl_syntax:forms(), errors:error()}.
annotate_forms([], _, Error) ->
  {[], Error};

annotate_forms([Mod = {attribute, _, module, _} | Forms], TInfo = #t_info{types = TypesCtx}, Error) ->
  Interfaces = get_interfaces(TypesCtx),
  ?TRACE("Interfaces: ~p", [Interfaces]),
  {Forms0, Error0} = annotate_forms(Forms, TInfo, Error),
  {[Mod | Interfaces ++ Forms0], Error0};

annotate_forms([Export = {attribute, _, export, _} | Forms], TInfo, Error) ->
  {Forms0, Error0} = annotate_forms(Forms, TInfo, Error),
  {[Export | Forms0], Error0};

annotate_forms([Import = {attribute, _, import, _} | Forms], TInfo, Error) ->
  {Forms0, Error0} = annotate_forms(Forms, TInfo, Error),
  {[Import | Forms0], Error0};

annotate_forms([{attribute, _, _, _} | Forms], TInfo, Error) ->
  % Eat other attributes.
  annotate_forms(Forms, TInfo, Error);

annotate_forms([Form = {function, Anno, Name, Arity, Clauses} | Forms], TInfo, Error) ->
  % Function.
  {Form0, Error0} = annotate_function(Form, TInfo, Error),
  {Forms0, Error1} = annotate_forms(Forms, TInfo, Error0),
  {[Form0 | Forms0], Error1};

annotate_forms([_ | Forms], TInfo, Error) ->
  annotate_forms(Forms, TInfo, Error).


%% @private Annotates functions with a single clause and without guards.
-spec annotate_function(erl_syntax:syntaxTree(), paterl_types:t_info(), errors:error()) ->
  {erl_syntax:syntaxTree(), errors:error()}.
annotate_function({function, Anno, Name, Arity, Clauses},
    TInfo = #t_info{specs = SpecsCtx, mb_defs = MbDefsCtx}, Error) ->

  % Recover function signature.
  Sig = {Name, Arity},

  % Signature must be defined in specs ctx since every function is annotated
  % with function specs.
  ?assert(maps:is_key(Sig, SpecsCtx)),
  {spec, _, Types} = maps:get(Sig, SpecsCtx),

  % Only one function clause and spec anno permitted.
  ?assert(length(Clauses) == 1 andalso length(Types) == 1),

  % Function may or may not be decorated with mailbox annotations.
  {Clauses0, Anno0, Error0} =
    case maps:get(Sig, MbDefsCtx, undefined) of
      {Modality, _, MbName} ->
        % Mailbox annotated function.
        ?TRACE("Annotating function '~p/~p' with mailbox scope '~p'", [Name, Arity, MbName]),
        {Clauses1, Error1} = annotate_clauses(Clauses, Types, MbName, TInfo, Error),
        Anno1 = set_modality(Modality, set_interface(MbName, Anno)),
        {Clauses1, Anno1, Error1};
      undefined ->
        % Non-annotated function.
        {Clauses, Anno, Error}
    end,

  Form0 = map_anno(fun(_) -> Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Clauses0))),
  {Form0, Error0}.

%% @private Annotates a function clause list.
-spec annotate_clauses([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()], atom(), paterl_types:t_info(), errors:error()) ->
  {[erl_syntax:syntaxTree()], errors:error()}.
annotate_clauses([], [], _, _, Error) ->
  {[], Error};
annotate_clauses([Clause | Clauses], [Type | Types], MbScope, TInfo, Error) ->
  {Clause0, Error0} = annotate_clause(Clause, Type, MbScope, TInfo, Error),
  {Clauses0, Error1} = annotate_clauses(Clauses, Types, MbScope, TInfo, Error0),
  {[Clause0 | Clauses0], Error1}.

%% @private Annotates a function clause.
%%
%% The following spec-annotated clauses are annotated:
%% 1. Function clause without guard
-spec annotate_clause(erl_syntax:syntaxTree(), erl_syntax:syntaxTree(), atom(), paterl_types:t_info(), errors:error()) ->
  {erl_syntax:syntaxTree(), errors:error()}.
annotate_clause(Clause = {clause, Anno, PatSeq, GuardSeq = [], Body},
    _ArgType = {type, _, 'fun', [{type, _, product, TypeSeq}, _RetType = {type, _, RetType, []}]}, MbScope, TInfo, Error)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq),
  is_list(Body) ->
  ?TRACE("Unguarded function clause: ~p", [Clause]),

  ?TRACE("Arg types: ~p", [TypeSeq]),
%%  ?TRACE("Ret type: ~p", [RetType]),

  ?TRACE("Function pats: ~p", [PatSeq]),
  ?TRACE("Function body: ~p", [Body]),

  AnnPatSeq = annotate_pat_seq(PatSeq, TypeSeq, MbScope, TInfo),

  ?TRACE(">> Ann pats: ~p", [AnnPatSeq]),


%%  Body0 = annotate_expr_seq(Body, undefined, MbScope, TInfo),
  {Body0, Error0} = annotate_expr_seq(Body, MbScope, TInfo, Error),

  ?TRACE(">>>>>>>>> Errors: ~p", [Error0]),
  Anno0 = set_interface(MbScope, set_type(RetType, Anno)),

  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(AnnPatSeq, GuardSeq, Body0), Anno0)
  ),
  {Clause0, Error0};

%% The following spec-annotated clauses are not annotated:
%% 1. Function clause with guard
annotate_clause(Clause, _ArgType, _MbScope, _TInfo, Error) ->
  ?TRACE("Guarded function clause: ~p", [Clause]),
  ?TRACE("_ArgType: ~p", [_ArgType]),
  {Clause, Error}.

%% @private Annotates a non-function clause list.
-spec annotate_clauses([erl_syntax:syntaxTree()], atom(), paterl_types:t_info(), errors:error()) ->
  {[erl_syntax:syntaxTree()], errors:error()}.
annotate_clauses([], _, _, Error) ->
  {[], Error};
annotate_clauses([Clause | Clauses], MbScope, TInfo, Error) ->
  ?TRACE("Annotating clause ~p", [Clause]),
  {Clause0, Error0} = annotate_clause(Clause, MbScope, TInfo, Error),
  {Clauses0, Error1} = annotate_clauses(Clauses, MbScope, TInfo, Error0),
  {[Clause0 | Clauses0], Error1}.

%% @private Annotates a non-function clause.
%%
%% The following clauses are annotated:
%% 1. If
-spec annotate_clause(erl_syntax:syntaxTree(), atom(), paterl_types:t_info(), errors:error()) ->
  {erl_syntax:syntaxTree(), errors:error()}.
annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    MbScope, TInfo, Error)
  when is_list(GuardSeq), is_list(Body) ->
  ?TRACE("If clause: ~p", [Clause]),

  {Body0, Error0} = annotate_expr_seq(Body, MbScope, TInfo, Error),
  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(GuardSeq, Body0), Anno)
  ),
  {Clause0, Error0};

%% 2. Receive/case
annotate_clause(Clause = {clause, Anno, PatSeq = [_], GuardSeq = [], Body}, MbScope, TInfo, Error) ->
  ?TRACE("Receive clause: ~p", [Clause]),

  {Body0, Error0} = annotate_expr_seq(Body, MbScope, TInfo, Error),
  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(PatSeq, GuardSeq, Body0), Anno)
  ),
  {Clause0, Error0};

%% The following clauses are not annotated:
%% 1. Case
%% 2. Case with guard
%% 3. Catch
annotate_clause(Clause, _, _, Error) ->
  ?TRACE("Other clause: ~p", [Clause]),
  {Clause, Error}.


%% @private Annotates a pattern list.
-spec annotate_pat_seq([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()], atom(), paterl_types:t_info()) ->
  [erl_syntax:syntaxTree()].
annotate_pat_seq(PatSeq, TypeSeq, MbScope, TInfo)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq) ->
  lists:zipwith(fun(Pat, Type) -> annotate_pat(Pat, Type, MbScope, TInfo) end, PatSeq, TypeSeq).

annotate_pat(Pat, {Qualifier, _, Type, _}, _MbScope, _TInfo)
  when Qualifier == type; Qualifier == user_type ->
%%  ?TRACE("Annotating function pattern '~p' with type '~p'", [Pat, Type]),
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
-spec annotate_expr_seq([erl_syntax:syntaxTree()], atom(), paterl_types:t_info(), errors:error()) ->
  {[erl_syntax:syntaxTree()], errors:error()}.
annotate_expr_seq([], _, _, Error = #error{}) ->
  ?TRACE("Empty expression sequence."),
  {[], Error};

annotate_expr_seq([_MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]}], _, _, Error) ->
  % Mailbox-annotation at the end of expression list.
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  {[], ?pushError(?E_EXPECTED_EXPR, Macro, Error)};

annotate_expr_seq(Expr = [{match, _, Pat, _MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]}}], MbScope, TInfo, Error) ->
  % Mailbox-annotation as match RHS, which is not followed by another expression.
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  {[], ?pushError(?E_EXPECTED_EXPR, Macro, Error)};

annotate_expr_seq([_MbAnno = {tuple, _, [{atom, _, _}, {_, _, _}]}, MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]} | ExprSeq], MbScope, TInfo, Error) ->
  % Two successive mailbox annotations.
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  Error0 = ?pushError(?E_BAD_ANNO, Macro, Error),
  {_, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
  {[], Error1};

annotate_expr_seq([{match, Anno, Pat, _MbAnno = {tuple, _, [{atom, _, Name}, {_, _, Value}]}}, Expr | ExprSeq], MbScope, TInfo, Error) ->
  % RHS of match expression is a mailbox-annotation expression. Use mailbox-
  % annotation name and value to annotate the AST node of the next expression in
  % the expression list. At this point, the next expression must exist. Replace
  % the mailbox-annotation in RHS of match with the AST of the newly annotated
  % (next) expression.
  Match0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr), Anno)
  ),

  {Expr1, Error0} = annotate_expr(Match0, {Name, Value}, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1};

annotate_expr_seq([Match = {match, _, _Pat, _Expr} | ExprSeq], MbScope, TInfo, Error) ->
  % RHS of match expression is a non mailbox-annotated expression.
  {Expr1, Error0} = annotate_expr(Match, undefined, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1};

annotate_expr_seq([_MbAnno = {tuple, _, [{atom, _, Name}, {_, _, Value}]}, Expr | ExprSeq], MbScope, TInfo, Error) ->
  % Mailbox-annotated expression.
  ?TRACE("--------------------> Annotated Mailbox expression = ~p", [_MbAnno]),
  {Expr1, Error0} = annotate_expr(Expr, {Name, Value}, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1};

annotate_expr_seq([Expr | ExprSeq], MbScope, TInfo, Error) ->
  % Non mailbox-annotated expression.
  ?TRACE("--------------------> Non Annotated Mailbox expression = ~p", [Expr]),

  % The commented below are tests to see whether it is more clean to return
  % "{ok, [Expr]} and {error, [], []" depending on whether the annotation is
  % successful or not, OR whether to return "{[Expr], {error, [], []}}" at all
  % times to keep the code short and then decide at the top-level annotate
  % function above.

  % Full errors.
%%  case annotate_expr(Expr, undefined, MbScope, TInfo, Error) of
%%    Error0 = #error{} ->
%%      annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error0); % This will surely return an error since Error0 already has an error in it.
%%    Expr0 ->
%%      case annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error) of
%%        Error1 = #error{} ->
%%          Error1;
%%        ExprSeq0 ->
%%          [Expr0 | ExprSeq0]
%%      end
%%  end,

  % Stop at first error. Same verbosity as above.
%%  case annotate_expr(Expr, undefined, MbScope, TInfo, Error) of
%%    Error0 = #error{} ->
%%      Error0;
%%    Expr0 ->
%%      case annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error) of
%%        Error1 = #error{} ->
%%          Error1;
%%        ExprSeq0 ->
%%          [Expr0 | ExprSeq0]
%%      end
%%  end,

  % TODO: Maybe something we can use to cleanly separate returning errors from
  % TODO: returning data.
%%  maybe
%%    {ok} ?= {ok, nahh},
%%    ?TRACE("baloo~n")
%%  end,

  ?TRACE("Annotating expression ~p", [Expr]),
  {Expr1, Error0} = annotate_expr(Expr, undefined, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1}.


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
-spec annotate_expr(Expr, MbAnno, MbScope, TInfo, Error) ->
  {erl_syntax:syntaxTree(), errors:error()}
  when
  Expr :: erl_syntax:syntaxTree(),
  MbAnno :: {atom(), atom() | string()} | undefined, %TODO: Enumerate the Mb-annotations, state | use | new, etc.
  MbScope :: atom(),
  TInfo :: paterl_types:t_info(),
  Error :: errors:error().
annotate_expr({call, Anno, Spawn = {atom, _, spawn}, MFArgs}, {AnnoName, MbName}, MbScope, TInfo, Error)
  when is_list(MFArgs), AnnoName == new ->
  % Mailbox-annotated spawn expression.
  Anno0 = set_modality(AnnoName, set_interface(MbName, Anno)),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Spawn, MFArgs), Anno0)
  ),
  {Expr0, Error};

annotate_expr(Expr = {call, Anno, Operator = {atom, _, spawn}, MFArgs}, _, MbScope, TInfo, Error)
  when is_list(MFArgs) ->
  % Mailbox-annotated spawn expression annotated with anything other than the new modality or a
  % non mailbox-annotated spawn expression.
  Spawn = erl_syntax:set_pos(erl_syntax:application(Operator, []), Anno),
  {Expr, ?pushError(?E_SPAWN_ANNO, Spawn, Error)};

annotate_expr({call, Anno, Self = {atom, _, self}, []}, undefined, MbScope, TInfo, Error) ->
  % Non mailbox-annotated self expression. Mailbox interface name can be
  % inferred from the enclosing mailbox scope.
  Anno0 = set_interface(MbScope, Anno),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Self, []), Anno0)
  ),
  {Expr0, Error};

annotate_expr({call, Anno, Operator = {atom, _, Name}, Exprs}, MbTypeAnno, MbScope,
    TInfo = #t_info{mb_defs = MbDefsCtx}, Error)
  when is_tuple(MbTypeAnno), size(MbTypeAnno) == 2;
  MbTypeAnno == undefined ->
  % Explicit function call where function name is an atom. Function call may be
  % mailbox-annotated or otherwise. If the function is mailbox-annotated, the
  % annotation is cross-checked with the inferred mailbox-annotation of the
  % called function definition for consistency. An error is raised if in case of
  % mismatch. If the function is not mailbox-annotated, the annotation is
  % inferred from the called function definition.
  Sig = {Name, length(Exprs)},

  {Anno0, Error0} =
    case maps:get(Sig, MbDefsCtx, undefined) of
      X = {Modality, _, MbName} ->
        ?TRACE("++++++++++++++ Found usage annotation = ~p", [X]),
        % Mailbox-annotated function call.
        case MbTypeAnno of
          undefined ->
            % Inferred mailbox-annotations.
            {set_modality(Modality, set_interface(MbName, Anno)), Error};
          {Modality, MbName} ->
            % Explicit mailbox-annotations correspond to inferred annotations.
            {set_modality(Modality, set_interface(MbName, Anno)), Error};
          {_, _} ->
            % Explicit mailbox-annotations conflict with inferred annotations.
            {Anno, ?pushError(?E_MISMATCH_ANNO, {Modality, Anno, MbName}, Error)} % e_mismatch_anno
        end;
      undefined ->
        % Non mailbox-annotated function call.
        {Anno, Error}
    end,
  {Exprs0, Error1} = annotate_expr_seq(Exprs, MbScope, TInfo, Error0),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
  ),
  {Expr0, Error1};

annotate_expr(_Expr = {call, Anno, Operator, Exprs}, {AnnoName, MbName}, MbScope, TInfo, Error) ->
  % Implicit function call where the function name an expression. Function call
  % must be annotated since it cannot be inferred from the called function
  % definition.
  {Exprs0, Error0} = annotate_expr_seq(Exprs, MbScope, TInfo, Error),
  Anno0 = set_modality(AnnoName, set_interface(MbName, Anno)),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
  ),
  {Expr0, Error0};

annotate_expr(_Expr = {'if', Anno, Clauses}, undefined, MbScope, TInfo, Error) ->
  % Non-annotated if expression.
  {Clauses0, Error0} = annotate_clauses(Clauses, MbScope, TInfo, Error),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:if_expr(Clauses0), Anno)
  ),
  {Expr0, Error0};

annotate_expr(Expr0 = {'if', Anno, _}, _, _, _, Error) ->
  % Annotated if expression.
  Expr = erl_syntax:set_pos(erl_syntax:if_expr([]), Anno),
  {Expr0, ?pushError(?E_BAD_ANNO_BEFORE, Expr, Error)};

annotate_expr(_Expr = {match, Anno, Pat, Expr}, MbTypeAnno, MbScope, TInfo, Error)
  when is_tuple(MbTypeAnno), size(MbTypeAnno) == 2;
  MbTypeAnno == undefined ->

  % Annotated match RHS expression or normal non-annotated RHS expression.
  {Expr0, Error0} = annotate_expr(Expr, MbTypeAnno, MbScope, TInfo, Error),
  Expr1 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr0), Anno)
  ),
  {Expr1, Error0};

annotate_expr({'receive', Anno, Clauses}, {state, Regex}, MbScope, TInfo, Error) ->
  % Mailbox-annotated blocking receive expression. Mailbox name can be inferred
  % from enclosing mailbox scope.
  {Clauses0, Error0} = annotate_clauses(Clauses, MbScope, TInfo, Error),
  Anno0 = set_state(Regex, set_interface(MbScope, Anno)),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:receive_expr(Clauses0), Anno0)
  ),
  {Expr0, Error0};

annotate_expr(Expr = {'receive', Anno, Clauses}, undefined, MbScope, TInfo, Error) ->
  % Non-annotated blocking receive expression.
  Receive = erl_syntax:set_pos(erl_syntax:receive_expr([]), Anno),
  Error0 = ?pushError(?E_RECEIVE_ANNO, Receive, Error),
  {_, Error1} = annotate_clauses(Clauses, MbScope, TInfo, Error0),

  {Expr, Error1};

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
annotate_expr(Expr, undefined, _, _, Error) ->
  % Non mailbox-annotated expression.
  {Expr, Error};
annotate_expr(Expr, _, _, _, Error) ->
  % Unexpected mailbox-annotated expression.
  {Expr, ?pushError(?E_BAD_ANNO_BEFORE, Expr, Error)}.


%%% ----------------------------------------------------------------------------
%%% Annotations.
%%% ----------------------------------------------------------------------------

type(Anno) ->
  get_anno_val(Anno, ?MA_TYPE, undefined).

interface(Anno) ->
  get_anno_val(Anno, ?MA_INTERFACE, undefined).

modality(Anno) ->
  get_anno_val(Anno, ?MA_MODALITY, undefined).

state(Anno) ->
  get_anno_val(Anno, ?MA_STATE, undefined).

set_type(Type, Anno) ->
  set_anno_val(Anno, ?MA_TYPE, Type).

set_interface(Interface, Anno) when is_atom(Interface) ->
  set_anno_val(Anno, ?MA_INTERFACE, Interface).

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


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
format_error({?E_EXPECTED_EXPR, Node}) ->
  io_lib:format(
    "expected expression after annotation '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD_ANNO, Node}) ->
  io_lib:format(
    "unexpected mailbox annotation '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD_ANNO_BEFORE, Node}) ->
  io_lib:format(
    "unexpected mailbox annotation before '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MISMATCH_ANNO, {Modality, _, MbName}}) ->
  io_lib:format(
    "incorrect mailbox annotation on function call; expected '?~s(~s)'",
    [Modality, MbName]
  );
format_error({?E_SPAWN_ANNO, Node}) ->
  io_lib:format(
    "expected ?new annotation before '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_RECEIVE_ANNO, Node}) ->
  io_lib:format(
    "expected ?assert annotation before '~s'",
    [erl_prettypr:format(Node)]
  ).


