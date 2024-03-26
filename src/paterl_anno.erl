%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang AST annotator.
%%% @end
%%% Created : 29. Jan 2024 15:20
%%%-------------------------------------------------------------------
-module(paterl_anno).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").


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


% Assumes that TInfo is uses the erlang parse tree spec. But if we want to use
% in erl_syntax, then we must lift it using merl:term!

annotate(Forms, TInfo) when is_list(Forms), is_record(TInfo, t_info) ->
  case annotate_forms(Forms, TInfo, #error{}) of
    {Forms0, #error{errors = []}} ->
      ?TRACE("-----> No errors found!"),
      {ok, Forms0};
    {_, #error{errors = Errors}} ->
      #error{errors = lists:reverse(Errors)}
  end.

get_interfaces(TypesCtx) ->
  lists:reverse(
    erl_syntax:revert_forms(
      maps:fold(
        fun(Mailbox, {mbox, ANNO, _, Vars = []}, Attrs) ->
          ?TRACE("Creating interface for mailbox '~p'", [Mailbox]),

          MsgTypesAS =
            case expand_msg_types(Mailbox, TypesCtx) of
              [Type] ->
                % Mailbox type is a singleton.
                Type;
              Types when is_list(Types) ->
                % Mailbox type is a list that must be recombined as a type union.
                erl_syntax:revert(erl_syntax:set_pos(erl_syntax:type_union(Types), ANNO))
            end,

          ?TRACE("Expanded mailbox type '~p': ~p", [Mailbox, MsgTypesAS]),

          Interface = erl_syntax:set_pos(
            erl_syntax:attribute(
              erl_syntax:atom(interface),
%%              erl_syntax:atom(type),
              [erl_syntax:tuple([
                erl_syntax:atom(Mailbox), % Mailbox type name.
%%                merl:term(MsgTypesAS), % Lifted message types AS.
%%                merl:term(Vars)] % Lifted variable types AS.
                erl_syntax:abstract(MsgTypesAS), % Lifted message types AS.
                erl_syntax:abstract(Vars)] % Lifted variable types AS.
              )]
            ),
            ANNO),

          [Interface | Attrs];
          (_, _, Attrs) ->
            % Eat other attributes.
            Attrs
        end,
        [], TypesCtx)
    )
  ).

expand_msg_types(Name, TypesCtx) ->
  {mbox, _, Type, _TypeVars} = maps:get(Name, TypesCtx),

  % Type variables not supported.
  ?assertMatch(_TypeVars, []),

  lists:reverse(expand_msg_type(Type, TypesCtx, [])).

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





annotate_forms([], TInfo, Error) ->
  {[], Error};

annotate_forms([Mod = {attribute, _, module, _} | Forms], TInfo = #t_info{types = TypesCtx}, Error) ->
  Interfaces = get_interfaces(TypesCtx),
  ?TRACE("Interfaces: ~p", [Interfaces]),
  {Forms0, Error0} = annotate_forms(Forms, TInfo, Error),
  {[Mod | Interfaces ++ Forms0], Error0};
%%  [Mod | annotate_forms(Forms, TInfo)];

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
        {Clauses1, Error1} = annotate_clauses2(Clauses, Types, MbName, TInfo, Error),
        Anno1 = set_modality(Modality, set_interface(MbName, Anno)),
        {Clauses1, Anno1, Error1};
      undefined ->
        % Non-annotated function.
        {Clauses, Anno, Error}
    end,

  Form0 = map_anno(fun(_) -> Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Clauses0))),
  {Form0, Error0}.



annotate_pat_seq(PatSeq, TypeSeq, MbScope, TInfo)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq) ->
  lists:zipwith(fun(Pat, Type) -> annotate_pat(Pat, Type, MbScope, TInfo) end, PatSeq, TypeSeq).

annotate_pat(Pat, {Qualifier, _, Type, _}, MbScope, TInfo)
  when Qualifier == type; Qualifier == user_type ->
  ?TRACE("Annotating function pattern '~p' with type '~p'", [Pat, Type]),
  map_anno(fun(Anno) -> set_type(Type, Anno) end, Pat).
%%  HERE tyring to get rid of map_anno

%%annotate_pat_seq(PatSeq, MbScope, TInfo)
%%  when is_list(PatSeq) ->
%%  lists:map(fun(Pat) -> annotate_pat(Pat, MbScope, TInfo) end, PatSeq).

%%annotate_pat(Pat, MbScope, TInfo) ->
%%  ?TRACE("Annotating pattern '~p' with type '~p'", [Pat]),


%%annotate_expr_seq([], _, _, _) ->
%%  [];

%% Type annotations.
%%annotate_expr_seq([{tuple, _, [{atom, _, AnnoName}, {atom, _, MbName}]} | ExprSeq], _, MbScope, TInfo) ->
%%  annotate_expr_seq(ExprSeq, {anno, AnnoName, MbName}, MbScope, TInfo);
%%annotate_expr_seq([{tuple, _, [{atom, _, AnnoName}, {string, _, Regex}]} | ExprSeq], _, MbScope, TInfo) ->
%%  annotate_expr_seq(ExprSeq, {anno, AnnoName, Regex}, MbScope, TInfo);

%% Annotated expressions:
%% 1. Internal spawn, self, or generic function call.
%% 2. If
%% 3. Match operator
%% 4. Receive
%%annotate_expr_seq([_Expr = {call, Anno, {atom, _, spawn}, Exprs = [M, F, Args]} | ExprSeq], {anno, '@new', MbName}, MbScope, TInfo) ->
%%  % Spawn always expects a @new annotation; @use is never permitted.
%%  ?TRACE("Spawn expr: ~p", [_Expr]),
%%
%%  % Annotate spawn.
%%  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

%%annotate_expr_seq([Expr = {call, _, {atom, _, self}, []} | ExprSeq], undefined, MbScope, TInfo) ->
%%  % Self expects no annotation.
%%  ?TRACE("Self expr: ~p", [Expr]),
%%  [Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

%%annotate_expr_seq([_Expr = {call, _, {atom, _, Name}, Exprs} | ExprSeq], {anno, AnnoName, MbName}, MbScope, TInfo)
%%  when is_atom(Name), is_list(Exprs), AnnoName == '@new'; is_list(Exprs), AnnoName == '@use' ->
%%  ?TRACE("[Auto inference] Call expr ~p/~p: ~p", [Name, length(Exprs), _Expr]),
%%
%%  % If sig exists in MbDefs, annotate with interface and modality, otherwise its not a mailbox used function.
%%%%  annotate_expr_seq(Exprs, TypeAnno, MbScope, TInfo),
%%  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

%%annotate_expr_seq([_Expr = {'if', Anno, Clauses} | ExprSeq], undefined, MbScope, TInfo) when is_list(Clauses) ->
%%  ?TRACE("If expr: ~p", [_Expr]),
%%  annotate_clauses(Clauses, lists:duplicate(length(Clauses), undefined), MbScope, TInfo),
%%  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

%%annotate_expr_seq([Expr0 = {match, Anno, Pat, Expr} | ExprSeq], undefined, MbScope, TInfo) ->
%%  ?TRACE("Match expr: ~p", [Expr0]),
%%
%%  AnnExpr =
%%    case annotate_expr_seq([Expr], undefined, MbScope, TInfo) of
%%      [] ->
%%        % This was an annotation.
%%        % Annotate the next expression sequence to stitch it.
%%        ?TRACE("This was an annotated match"),
%%        {tuple, _, [{atom, _, AnnoName}, {atom, _, MbName}]} = Expr,
%%        [XX] = annotate_expr_seq([hd(ExprSeq)], {anno, AnnoName, MbName}, MbScope, TInfo),
%%        ?TRACE("Annotated the next only ~p", [XX]),
%%        ?TRACE("Annotating the rest of the expressions ~p", [tl(ExprSeq)]),
%%        [{match, Anno, Pat, XX} | annotate_expr_seq(tl(ExprSeq), undefined, MbScope, TInfo)];
%%      _ ->
%%        % This was a normal match.
%%        ?TRACE("This was a normal match"),
%%        [Expr0 | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)]
%%    end;


%%annotate_expr_seq([_Expr = {'receive', Anno, Clauses} | ExprSeq], {anno, '@state', Regex}, MbScope, TInfo) when is_list(Clauses) ->
%%  ?TRACE("Receive expr: ~p", [_Expr]),
%%
%%  % Extract receive type.
%%  annotate_clauses(Clauses, lists:duplicate(length(Clauses), undefined), MbScope, TInfo),
%%  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];


%% The following expressions are not annotated:
%% 1. Atomic literal
%% 2. Bitstring comprehension
%% 3. Bitstring constructor
%% 4. Block
%% 5. Case
%% 6. Catch
%% 7. Cons skeleton
%% 8. Fun
%% 9. External function call
%% 10. List comprehension
%% 11. Map comprehension
%% 12. Map creation
%% 13. Map update
%% 14. Conditional match operator
%% 15. Maybe
%% 16. Nil
%% 17. Operator
%% 18. Record creation
%% 19. Record field access
%% 20. Record field index
%% 21. Record update
%% 22. Tuple skeleton
%% 23. Try
%% 24. Variable
%%annotate_expr_seq([Expr | ExprSeq], TypeAnno, MbScope, TInfo) ->
%%  ?TRACE("Other expr: ~p", [Expr]),
%%  [Expr | annotate_expr_seq(ExprSeq, TypeAnno, MbScope, TInfo)].

%% Spec-annotated function clauses.
%%annotate_clauses(Clauses, Types, MbScope, TInfo, Error)
%%  when is_list(Clauses), is_list(Types), is_record(TInfo, t_info) ->
%%
%%  % Each function clause has a corresponding function spec.
%%  ?assert(length(Clauses) == length(Types)),
%%
%%
%%
%%  HERE = lists:zipwith(fun(Clause, Type) -> annotate_clause(Clause, Type, MbScope, TInfo) end, Clauses, Types),
%%
%%
%%  {HERE, Error}. %TODO: Fix this

annotate_clauses2([], [], _, _, Error) ->
  {[], Error};
annotate_clauses2([Clause | Clauses], [Type | Types], MbScope, TInfo, Error) ->
  {Clause0, Error0} = annotate_clause(Clause, Type, MbScope, TInfo, Error),
  {Clauses0, Error1} = annotate_clauses2(Clauses, Types, MbScope, TInfo, Error0),
  {[Clause0 | Clauses0], Error1}.

%% The following spec-annotated clauses are annotated:
%% 1. Function clause without guard
annotate_clause(Clause = {clause, Anno, PatSeq, GuardSeq = [], Body},
    Type = {type, _, 'fun', [{type, _, product, TypeSeq}, RetType]}, MbScope, TInfo, Error)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq),
  is_list(Body) ->
  ?TRACE("Unguarded function clause: ~p", [Clause]),

  ?TRACE("Arg types: ~p", [TypeSeq]),
  ?TRACE("Ret type: ~p", [RetType]),

  ?TRACE("Function pats: ~p", [PatSeq]),
  ?TRACE("Function body: ~p", [Body]),

  AnnPatSeq = annotate_pat_seq(PatSeq, TypeSeq, MbScope, TInfo),

  ?TRACE(">> Ann pats: ~p", [AnnPatSeq]),


%%  Body0 = annotate_expr_seq(Body, undefined, MbScope, TInfo),
  {Body0, Error0} = annotate_expr_seq2(Body, MbScope, TInfo, Error),

  ?TRACE(">>>>>>>>> Errors: ~p", [Error0]),
  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(AnnPatSeq, GuardSeq, Body0), Anno)
  ),
  {Clause0, Error0};

%% The following spec-annotated clauses are not annotated:
%% 1. Function clause with guard
annotate_clause(Clause, _, _, _, Error) ->
  ?TRACE("Guarded function clause: ~p", [Clause]),
  {Clause, Error}.

%% Non spec-annotated if, case, and catch clauses.
%%annotate_clauses(Clauses, MbScope, TInfo)
%%  when is_list(Clauses), is_record(TInfo, t_info) ->
%%  lists:map(fun(Clause) -> annotate_clause(Clause, MbScope, TInfo) end, Clauses).

annotate_clauses2([], _, _, Error) ->
  {[], Error};
annotate_clauses2([Clause | Clauses], MbScope, TInfo, Error) ->
  ?TRACE("Annotating clause ~p", [Clause]),
  {Clause0, Error0} = annotate_clause(Clause, MbScope, TInfo, Error),
  {Clauses0, Error1} = annotate_clauses2(Clauses, MbScope, TInfo, Error0),
  {[Clause0 | Clauses0], Error1}.

%% The following clauses are annotated:
%% 1. If
%% 2. Receive/case
annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    MbScope, TInfo, Error)
  when is_list(GuardSeq), is_list(Body) ->
  ?TRACE("If clause: ~p", [Clause]),

%%  Body0 = annotate_expr_seq(Body, undefined, MbScope, TInfo),
  {Body0, Error0} = annotate_expr_seq2(Body, MbScope, TInfo, Error),
  Clause0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(GuardSeq, Body0), Anno)
  ),
  {Clause0, Error0};

annotate_clause(Clause = {clause, Anno, PatSeq = [_], GuardSeq = [], Body}, MbScope, TInfo, Error) ->
  ?TRACE("Receive clause: ~p", [Clause]),

  {Body0, Error0} = annotate_expr_seq2(Body, MbScope, TInfo, Error),
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


%% TODO: Alternate implementation ----------------------------------------------

%% @private Body.
%%
%% Match expressions are handled depending on whether the RHS is a mailbox
%% annotation or otherwise. In the former case, the mailbox-annotation AST node
%% is replaced with the AST node of the expression that follows the match
%% expression in the sequence of expressions. Otherwise, the AST of the match
%% expression is not modified and its RHS is annotated.
%%annotate_expr_seq([], _, _, Error) ->
%%  {[], Error};
%%annotate_expr_seq([Match = {match, Anno, Pat, Expr} | ExprSeq], MbScope, TInfo, Error) ->
%%  % Check whether current expression is a mailbox annotation and extract its
%%  % components.
%%  case is_anno_expr(Expr) of
%%    {true, AnnoName, Value} ->
%%      % RHS of match expression is a mailbox annotation. Use mailbox annotation
%%      % name and value to annotate the AST node of the next expression in the
%%      % sequence following the match expression. The next expression must exist,
%%      % otherwise it is an error. Replace the RHS mailbox-annotation AST node of
%%      % the match expression with the AST of the newly-annotation expression.
%%%%      case hd(ExprSeq) of
%%
%%      case ExprSeq of
%%        [] -> %TODO: I don't think this can ever occur in a match
%%          % Mailbox-annotation must be followed by an expression.
%%          {[], ?pushError(?E_EXPECTED_EXPR, {AnnoName, 66, Value}, Error)};
%%        [Expr0 | ExprSeq0] ->
%%          % Replace AST node of mailbox-annotation with newly-annotated
%%          % expression and annotate remaining expressions in the sequence.
%%%%          [Expr0 | ExprSeq0] = ExprSeq,
%%
%%          Match0 = erl_syntax:revert(
%%            erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr0), Anno)
%%          ),
%%
%%          {Expr1, Error0} = annotate_expr(Match0, {AnnoName, Value}, MbScope, TInfo, Error),
%%          {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq0, MbScope, TInfo, Error0),
%%          {[Expr1 | ExprSeq1], Error1}
%%%%          [annotate_expr(Match0, {AnnoName, Value}, MbScope, TInfo) | annotate_expr_seq(ExprSeq0, MbScope, TInfo)]
%%      end;
%%    false ->
%%      % RHS of match expression is a non mailbox-annotated expression.
%%      {Expr1, Error0} = annotate_expr(Match, undefined, MbScope, TInfo, Error),
%%      {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
%%      {[Expr1 | ExprSeq1], Error1}
%%
%%
%%%%      [annotate_expr(Match, undefined, MbScope, TInfo) | annotate_expr_seq(ExprSeq, MbScope, TInfo)]
%%  end;
%%
%%annotate_expr_seq([Expr | ExprSeq], MbScope, TInfo, Error) ->
%%  % Check whether current expression is a mailbox annotation and extract its
%%  % components.
%%  case is_anno_expr(Expr) of
%%    {true, AnnoName, Value} ->
%%      % Expression is a mailbox-annotation. Use mailbox annotation name and
%%      % value to annotate the AST node of the next expression in the sequence.
%%      % The next expression must exist, otherwise it is an error. Eat the
%%      % mailbox annotation expression.
%%%%      HERE: ExprSeq can be [one elem], which is good, or [], which is bad when Expr is an expression.
%%%%      case hd(ExprSeq) of
%%      case ExprSeq of
%%        [] ->
%%          % Annotation expression must always be followed by an annotated
%%          % expression.
%%%%          {[], ?pushError(?E_EXPECTED_EXPR, {AnnoName, erl_syntax:get_pos(Expr), Value}, Error)};
%%          {[], ?pushError(?E_EXPECTED_EXPR, {AnnoName, 10000, Value}, Error)};
%%        [Expr0 | ExprSeq0] ->
%%          % Annotate next expression and remaining expressions in the sequence.
%%%%          [Expr0 | ExprSeq0] = ExprSeq,
%%
%%          {Expr1, Error0} = annotate_expr(Expr0, {AnnoName, Value}, MbScope, TInfo, Error),
%%          {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq0, MbScope, TInfo, Error0),
%%          {[Expr1 | ExprSeq1], Error1}
%%
%%%%          [annotate_expr(Expr0, {AnnoName, Value}, MbScope, TInfo) | annotate_expr_seq(ExprSeq0, MbScope, TInfo)]
%%      end;
%%    false ->
%%      % Expression is a normal (non mailbox annotation) expression.
%%      {Expr1, Error0} = annotate_expr(Expr, undefined, MbScope, TInfo, Error),
%%      {ExprSeq1, Error1} = annotate_expr_seq(ExprSeq, MbScope, TInfo, Error0),
%%      {[Expr1 | ExprSeq1], Error1}
%%
%%%%      [annotate_expr(Expr, undefined, MbScope, TInfo) | annotate_expr_seq(ExprSeq, MbScope, TInfo)]
%%  end.


annotate_expr_seq2([], _, _, Error = #error{errors = Errors}) ->
  ?TRACE("::: Empty expression seq"),
  {[], Error};
annotate_expr_seq2(Expr = [_MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]}], MbScope, TInfo, Error) ->
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  {[], ?pushError(?E_EXPECTED_EXPR, Macro, Error)};
annotate_expr_seq2(Expr = [{match, _, Pat, _MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]}}], MbScope, TInfo, Error) ->
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  {[], ?pushError(?E_EXPECTED_EXPR, Macro, Error)};

annotate_expr_seq2([_MbAnno = {tuple, _, [{atom, _, _}, {_, _, _}]}, MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]} | ExprSeq], MbScope, TInfo, Error) ->
  % Two successive mailbox annotations.
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  Error0 = ?pushError(?E_BAD_ANNO, Macro, Error),
  {_, Error1} = annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error0),
  {[], Error1};

annotate_expr_seq2([_MbAnno = {tuple, Anno, [Name = {atom, _, _}, Arg = {_, _, _}]}, Match = {match, _, _, _} | ExprSeq], MbScope, TInfo, Error) ->
  % A mailbox annotation followed by a match.
  Macro = erl_syntax:set_pos(erl_syntax:macro(Name, [Arg]), Anno),
  Error0 = ?pushError(?E_BAD_ANNO, Macro, Error),
  % Check the rest of the expressions INCLUDING the match.
  {_, Error1} = annotate_expr_seq2([Match | ExprSeq], MbScope, TInfo, Error0),
  {[], Error1};


annotate_expr_seq2([{match, Anno, Pat, _MbAnno = {tuple, _, [{atom, _, Name}, {_, _, Value}]}}, Expr | ExprSeq], MbScope, TInfo, Error) ->

  ?TRACE("----------> Match + Anno = ~p", [_MbAnno]),
  ?TRACE("----------> Next EXPR = ~p", [Expr]),
  Match0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr), Anno)
  ),

  {Expr1, Error0} = annotate_expr(Match0, {Name, Value}, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1};

annotate_expr_seq2([Match = {match, _, Pat, Expr} | ExprSeq], MbScope, TInfo, Error) ->
  {Expr1, Error0} = annotate_expr(Match, undefined, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1};



annotate_expr_seq2([_MbAnno = {tuple, _, [{atom, _, Name}, {_, _, Value}]}, Expr | ExprSeq], MbScope, TInfo, Error) ->

  ?TRACE("=====> Anno + Expr = ~p", [Expr]),

  {Expr1, Error0} = annotate_expr(Expr, {Name, Value}, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1};

annotate_expr_seq2([Expr | ExprSeq], MbScope, TInfo, Error) ->
  ?TRACE("Annotating expression ~p", [Expr]),
  {Expr1, Error0} = annotate_expr(Expr, undefined, MbScope, TInfo, Error),
  {ExprSeq1, Error1} = annotate_expr_seq2(ExprSeq, MbScope, TInfo, Error0),
  {[Expr1 | ExprSeq1], Error1}.


%%- HERE FILL COMMENTS
%%- USE MACROS NAMES FOR SAVING ANNOTATIONS AS PART OF THE AST
%%- HANDLE ERRORS AND RETURN VALUES
%% @private Expressions.
%%
%% The following expressions are annotated:
%% 1. Mailbox-annotated spawn call
%% 2. Non mailbox-annotated self call
%% 3. (Non) mailbox-annotated internal function call
%% 4. If expression
%% 5. Match operator
%% 6. Receive
annotate_expr({call, Anno, Spawn = {atom, _, spawn}, MFArgs}, {AnnoName, MbName}, MbScope, TInfo, Error)
  when is_list(MFArgs), AnnoName == new ->
  % Mailbox-annotated spawn expression.
  Anno0 = set_modality(AnnoName, set_interface(MbName, Anno)),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Spawn, MFArgs), Anno0)
  ),
  {Expr0, Error};

annotate_expr(Expr = {call, Anno, Spawn = {atom, _, spawn}, MFArgs}, {AnnoName, MbName}, MbScope, TInfo, Error)
  when is_list(MFArgs)->
  % Mailbox-annotated spawn expression annotated with anything other than the new modality.
  {Expr, ?pushError(?E_SPAWN_ANNO, Expr, Error)};

annotate_expr(Expr = {call, Anno, {atom, _, spawn}, Exprs = [M, F, Args]}, undefined, _, TInfo, Error) ->
  % Non mailbox-annotated spawn expression.
  {Expr, ?pushError(?E_SPAWN_ANNO, Expr, Error)};

annotate_expr({call, Anno, Self = {atom, _, self}, []}, undefined, MbScope, TInfo, Error) ->
  % Non mailbox-annotated self expression. Mailbox interface name can be
  % inferred from the enclosing mailbox scope.
  Anno0 = set_interface(MbScope, Anno),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Self, []), Anno0)
  ),
  {Expr0, Error};

annotate_expr(Expr = {call, Anno, Operator = {atom, _, Name}, Exprs}, MbTypeAnno, MbScope,
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

  ?TRACE("++++++++++++++ About to look for mb usage annotation"),
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
  {Exprs0, Error1} = annotate_expr_seq2(Exprs, MbScope, TInfo, Error0),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
  ),
  {Expr0, Error1};

annotate_expr(_Expr = {call, Anno, Operator, Exprs}, {AnnoName, MbName}, MbScope, TInfo, Error) ->
  % Implicit function call where the function name an expression. Function call
  % must be annotated since it cannot be inferred from the called function
  % definition.
  {Exprs0, Error0} = annotate_expr_seq2(Exprs, MbScope, TInfo, Error),
  Anno0 = set_modality(AnnoName, set_interface(MbName, Anno)),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:application(Operator, Exprs0), Anno0)
  ),
  {Expr0, Error0};

annotate_expr(_Expr = {'if', Anno, Clauses}, undefined, MbScope, TInfo, Error) ->
  % Non-annotated if expression.
  {Clauses0, Error0} = annotate_clauses2(Clauses, MbScope, TInfo, Error),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:if_expr(Clauses0), Anno)
  ),
  {Expr0, Error0};

annotate_expr(Expr0 = {'if', Anno, Clauses}, _, MbScope, TInfo, Error) ->
  % Annotated if expression.

  If = erl_syntax:set_pos(erl_syntax:if_expr([]), Anno),
  {Expr0, ?pushError(?E_BAD_ANNO_BEFORE, If, Error)};

annotate_expr(_Expr = {match, Anno, Pat, Expr}, MbTypeAnno, MbScope, TInfo, Error)
  when is_tuple(MbTypeAnno), size(MbTypeAnno) == 2;
  MbTypeAnno == undefined ->

  ?TRACE("== Match ~p with ~p", [_Expr, MbTypeAnno]),
  % Annotated match RHS expression or normal non-annotated RHS expression.
  {Expr0, Error0} = annotate_expr(Expr, MbTypeAnno, MbScope, TInfo, Error),
  Expr1 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:match_expr(Pat, Expr0), Anno)
  ),
  {Expr1, Error0};

% Receive without timeout.
annotate_expr({'receive', Anno, Clauses}, X = {AnnoName, Regex}, MbScope, TInfo, Error) ->
  % Mailbox-annotated receive expression. Mailbox name can be inferred from
  % enclosing mailbox scope.
%%  ?TRACE("----------------------------------> ~p", [X]),
%%  ?TRACE("----------------------------------> ~p", [MbScope]),
  ?TRACE("About to annotate receive clauses"),
  {Clauses0, Error0} = annotate_clauses2(Clauses, MbScope, TInfo, Error),
  Anno0 = set_state(Regex, set_interface(MbScope, Anno)),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:receive_expr(Clauses0), Anno0)
  ),
  {Expr0, Error0};

annotate_expr(Expr = {'receive', Anno, Clauses}, undefined, MbScope, TInfo, Error) ->
  % Non-annotated receive expression.
  % ERR: Expected mailbox assertion annotation.
  ?TRACE("About to annotate receive clauses WITHOUT type annotation!!"),

  Error0 = ?pushError(?E_RECEIVE_ANNO, Expr, Error),
  {_, Error1} = annotate_clauses2(Clauses, MbScope, TInfo, Error0),

  %TODO: Still cont
  {Expr, Error1}; % e_receive_anno

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
  Anno = element(2, Expr),
  {Expr, ?pushError(?E_BAD_ANNO_BEFORE, Expr, Error)}. % e_bad_anno



is_anno_expr({tuple, _, [{atom, _, AnnoName}, {atom, _, MbName}]}) ->
  {true, AnnoName, MbName};
is_anno_expr({tuple, _, [{atom, _, AnnoName}, {string, _, Regex}]}) ->
  {true, AnnoName, Regex};
is_anno_expr(_) ->
  false.

%%any() ->
%%  erl_syntax:revert(erl_syntax:type_application(erl_syntax:atom(any), [])).
%%
%%none() ->
%%  erl_syntax:revert(erl_syntax:type_application(erl_syntax:atom(none), [])).


%%make_type(Name, Args) when is_atom(Name), is_list(Args) ->
%%  erl_syntax:revert(erl_syntax:type_application(erl_syntax:atom(Name), Args)).
%%
%%make_type(Name) when is_atom(Name) ->
%%  make_type(Name, []).

%%merl() ->
%%  Tuple = ?Q("{foo, 42}"),
%%  Tuple1 = erl_syntax:tuple([erl_syntax:atom(foo), erl_syntax:integer(42)]),
%%  ?TRACE("Tuple: ~p", [Tuple]),
%%  ?TRACE("Tuple1: ~p", [Tuple1]),
%%  ?Q("{foo, _@Number}") = Tuple1,
%%  ?TRACE("Number? ~p", [Number]),
%%  ?TRACE("Pattern: ~p", [?Q("{foo, _@Number}")]),
%%  Call = ?Q("foo:bar(_@Number)"),
%%  ?TRACE("Call: ~p", [Call]),
%%  merl:print(Call).
%%
%%merl_test(AST) ->
%%  case AST of
%%    ?Q("{foo, _@Foo}") -> io:format("Foo: ~p", [Foo]);
%%    ?Q("{bar, _@Bar}") when erl_syntax:is_integer(Bar) -> io:format("Bar: ~p", [Bar]);
%%    _ -> ok
%%  end.

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

set_type(Type, Anno) when is_atom(Type) ->
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
%%format_error({?E_EXPECTED_EXPR, {Modality, _, MbName}}) ->
%%  io_lib:format(
%%%%    "!!expected expression after annotation '~s'",
%%    "!!expected expression after annotation '?~s(~s)'",
%%%%    [erl_prettypr:format(Node)]
%%    [Modality, MbName]
%%  );
format_error({?E_EXPECTED_EXPR, Node}) ->
  io_lib:format(
    "!!expected expression after annotation '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD_ANNO, Node}) ->
  io_lib:format(
    "!!unexpected mailbox annotation '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD_ANNO_BEFORE, Node}) ->
  io_lib:format(
    "!!unexpected mailbox annotation before '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MISMATCH_ANNO, {Modality, _, MbName}}) ->
  io_lib:format(
    "!!incorrect mailbox annotation on function call; expected '?~s(~s)'",
    [Modality, MbName]
  );
format_error({?E_SPAWN_ANNO, Node}) ->
  io_lib:format(
    "!!expected ?new annotation before '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_RECEIVE_ANNO, Node}) ->
  io_lib:format(
    "!!expected ?assert annotation before '~s'",
    [erl_prettypr:format(Node)]
  ).


