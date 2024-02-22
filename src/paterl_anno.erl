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






% Assumes that TInfo is uses the erlang parse tree spec. But if we want to use
% in erl_syntax, then we must lift it using merl:term!

annotate(Forms, TInfo) when is_list(Forms), is_record(TInfo, t_info) ->
  annotate_forms(Forms, TInfo).


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
%%              erl_syntax:atom(interface),
              erl_syntax:atom(type),
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





annotate_forms([], TInfo) ->
  [];
annotate_forms([Mod = {attribute, _, module, _} | Forms], TInfo = #t_info{types = TypesCtx}) ->
  Interfaces = get_interfaces(TypesCtx),
  ?TRACE("Interfaces: ~p", [Interfaces]),
  [Mod | Interfaces ++ annotate_forms(Forms, TInfo)];
%%  [Mod | annotate_forms(Forms, TInfo)];
annotate_forms([Export = {attribute, _, export, _} | Forms], TInfo) ->
  [Export | annotate_forms(Forms, TInfo)];
annotate_forms([Import = {attribute, _, import, _} | Forms], TInfo) ->
  [Import | annotate_forms(Forms, TInfo)];
annotate_forms([{attribute, _, _, _} | Forms], TInfo) ->
  % Remove any other attribute.
  annotate_forms(Forms, TInfo);

%%annotate_forms([Form = {function, ANNO, Name, Arity, Clauses} | Forms], TInfo = #t_info{specs = SpecsCtx, mb_defs = MbDefsCtx}) ->
annotate_forms([Form = {function, ANNO, Name, Arity, Clauses} | Forms], TInfo) ->
  ?TRACE("Annotating '~s/~b'", [Name, Arity]),

%%  TODO: Change all the annotation functions to use TInfo directly since I can
%%  probably infer a lot of information for function calls and things.

  Form0 = annotate_fun(Form, TInfo),

  [Form0 | annotate_forms(Forms, TInfo)];

annotate_forms([_ | Forms], TInfo) ->
  annotate_forms(Forms, TInfo).


annotate_fun({function, Anno, Name, Arity, Clauses},
    TInfo = #t_info{specs = SpecsCtx, mb_defs = MbDefsCtx}) ->
  Sig = {Name, Arity},

  % Signature defined in specs and mb defs ctx.
  ?assert(maps:is_key(Sig, SpecsCtx)),

  {spec, _, Types} = maps:get(Sig, SpecsCtx),

  % Only one function clause and spec anno permitted.
  ?assert(length(Clauses) == 1 andalso length(Types) == 1),

  Anno0 =
    case maps:get(Sig, MbDefsCtx, undefined) of
      {Modality, _, Interface} ->
        % Annotated function.
        set_modality(Modality, set_interface(Interface, Anno));
      undefined ->
        % Non-annotated function.
        Anno
    end,

  ?DEBUG("~n~n----------------- Annotating function ~p/~p", [Name, Arity]),

  Clauses0 = annotate_clauses(Clauses, Types, none, TInfo),
  map_anno(fun(_) -> Anno0 end, erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Clauses0))).


%%(Node, Sig or Type, Scope, TInfo)
%% Clauses, pats, expressions?

%%annotate_fun_clauses(Clauses, Specs)
%%annotate_fun_clauses(Clauses, Types, Scope, TInfo) ->
%%  when is_list(Clauses), is_list(Specs), length(Clauses) == length(Specs) ->


% Only one function clause and one corresponding spec permitted.
%%  ?assert(length(Clauses) == 1),
%%  ?assert(length(Types) == 1),
%%  lists:map(
%%    fun({Clause, Type}) -> annotate_clause(Clause, Type, Scope, TInfo) end,
%%    lists:zip(Clauses, Types)
%%  ).
%%  lists:zipwith(fun(Clause, Type) -> annotate_clause(Clause, Type, Scope, TInfo) end, Clauses, Types).

%%annotate_clause({clause, Anno, Pats, Guard, Body},
%%    Type = {type, _, 'fun', [{type, _, product, ArgTypes}, RetType]}, Scope, TInfo) ->
%%  % Clause guard not permitted.
%%  ?assertMatch(Guard, []),
%%
%%  ?TRACE("Arg types: ~p", [ArgTypes]),
%%  ?TRACE("Ret type: ~p", [RetType]),
%%
%%  ?TRACE("Function pats: ~p", [Pats]),
%%  ?TRACE("Function body: ~p", [Body]),
%%
%%  % length(Pats) == length(ArgTypes)
%%  AnnPats = annotate_pats(Pats, ArgTypes, Scope, TInfo),
%%
%%  ?TRACE(">> Ann pats: ~p", [AnnPats]),
%%
%%
%%  Body0 = annotate_exprs(Body, Scope, TInfo),
%%
%%  erl_syntax:revert(
%%    erl_syntax:set_pos(erl_syntax:clause(AnnPats, Guard, Body0), Anno)
%%  ).

annotate_pat_seq(PatSeq, TypeSeq, MbScope, TInfo)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq) ->
  lists:zipwith(fun(Pat, Type) -> annotate_pat(Pat, Type, MbScope, TInfo) end, PatSeq, TypeSeq).

annotate_pat(Pat, {Qualifier, _, Type, _}, MbScope, TInfo)
  when Qualifier == type; Qualifier == user_type ->
  ?TRACE("Annotating '~p' with type '~p'", [Pat, Type]),
  map_anno(fun(Anno) -> set_type(Type, Anno) end, Pat).


% TODO: If I use a foldl and two accumulators {ExprAcc, PairOfExprAcc}, I should be able to do it.
% TODO: I think it will also fit nicely for when annotations are not needed.
%%annotate_expr_seq(Exprs, MbScope, TInfo) when is_list(Exprs) ->
%%  lists:map(
%%    fun(Expr = {tuple, Anno, [{atom, _, '@new'}, {atom, _, MbName}]}) ->
%%        annotate_ann_expr(Expr, MbScope, TInfo); % Not enough because I need to extract the next expression underneath.
%%      (Expr) ->
%%        annotate_expr(Expr, MbScope, TInfo)
%%    end,
%%    Exprs).

%%annotate_expr_seq(ExprSeq, MbScope, TInfo) when is_list(ExprSeq) ->
%%  ?TRACE("---------------- Annotating an expression sequence"),
%%  {AnnExprs, []} =
%%  lists:foldl(
%%    fun(Expr = {tuple, Anno, [{atom, _, '@new'}, {atom, _, MbName}]}, {AnnExprSeq, ExprBuf}) ->
%%      % Add annotation expression to buffer.
%%      ?TRACE("Buf @new annotation"),
%%      {AnnExprSeq, [Expr | ExprBuf]};
%%      (Expr = {tuple, Anno, [{atom, _, '@use'}, {atom, _, MbName}]}, {AnnExprSeq, ExprBuf}) ->
%%        ?TRACE("Buf @use annotation"),
%%        % Add annotation expression to buffer.
%%        {AnnExprSeq, [Expr | ExprBuf]};
%%      (Expr = {tuple, Anno, [{atom, _, '@state'}, {atom, _, MbName}, {string, _, Regex}]}, {AnnExprSeq, ExprBuf}) ->
%%        % Add annotation expression to buffer.
%%        ?TRACE("Buf @state annotation"),
%%        {AnnExprSeq, [Expr | ExprBuf]};
%%      (Expr, {AnnExprSeq, ExprBuf}) ->
%%        {[Expr | AnnExprSeq], []}
%%    end,
%%    {[], []}, ExprSeq),
%%    AnnExprs.

%%annotate_expr_seq(ExprSeq, MbScope, TypeAnno, TInfo) when is_list(ExprSeq) ->
%%%%  lists:map(fun(Expr) -> annotate_expr(Expr, MbScope, undefined, TInfo) end, ExprSeq).
%%  {AnnExprSeq, undefined} =
%%    lists:foldl(
%%      fun(Expr, {AnnExprSeq, TypeAnno}) ->
%%        case annotate_expr(Expr, TypeAnno, MbScope, TInfo) of
%%          Anno = {anno, _Type, MbName} when is_atom(MbName) ->
%%            ?TRACE("++ Found anno: ?~s(~s)", [_Type, MbName]),
%%            {AnnExprSeq, Anno};
%%          Anno = {anno, _Type, Regex} when is_list(Regex) ->
%%            ?TRACE("++ Found anno: ?~s(\"~s\")", [_Type, Regex]),
%%            {AnnExprSeq, Anno};
%%          AnnExpr ->
%%            ?TRACE("Found normal expression ~p", [AnnExpr]),
%%            {[AnnExpr | AnnExprSeq], undefined}
%%        end
%%      end,
%%      {[], undefined},
%%      ExprSeq),
%%  lists:reverse(AnnExprSeq).

annotate_expr_seq([], _, _, _) ->
  [];

%% Type annotations.
annotate_expr_seq([{tuple, _, [{atom, _, AnnoName}, {atom, _, MbName}]} | ExprSeq], _, MbScope, TInfo) ->
  annotate_expr_seq(ExprSeq, {anno, AnnoName, MbName}, MbScope, TInfo);
annotate_expr_seq([{tuple, _, [{atom, _, AnnoName}, {string, _, Regex}]} | ExprSeq], _, MbScope, TInfo) ->
  annotate_expr_seq(ExprSeq, {anno, AnnoName, Regex}, MbScope, TInfo);

%% Annotated expressions:
%% 1. Internal spawn, self, or generic function call.
%% 2. If
%% 3. Match operator
%% 4. Receive
annotate_expr_seq([_Expr = {call, Anno, {atom, _, spawn}, Exprs = [M, F, Args]} | ExprSeq], {anno, '@new', MbName}, MbScope, TInfo) ->
  % Spawn always expects a @new annotation; @use is never permitted.
  ?TRACE("Spawn expr: ~p", [_Expr]),

  % Annotate spawn.
  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

annotate_expr_seq([Expr = {call, _, {atom, _, self}, []} | ExprSeq], undefined, MbScope, TInfo) ->
  % Self expects no annotation.
  ?TRACE("Self expr: ~p", [Expr]),
  [Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

annotate_expr_seq([_Expr = {call, _, {atom, _, Name}, Exprs} | ExprSeq], {anno, AnnoName, MbName}, MbScope, TInfo)
  when is_atom(Name), is_list(Exprs), AnnoName == '@new'; is_list(Exprs), AnnoName == '@use' ->
  ?TRACE("[Auto inference] Call expr ~p/~p: ~p", [Name, length(Exprs), _Expr]),

  % If sig exists in MbDefs, annotate with interface and modality, otherwise its not a mailbox used function.
%%  annotate_expr_seq(Exprs, TypeAnno, MbScope, TInfo),
  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

annotate_expr_seq([_Expr = {'if', Anno, Clauses} | ExprSeq], undefined, MbScope, TInfo) when is_list(Clauses) ->
  ?TRACE("If expr: ~p", [_Expr]),
  annotate_clauses(Clauses, lists:duplicate(length(Clauses), undefined), MbScope, TInfo),
  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];

annotate_expr_seq([Expr0 = {match, Anno, Pat, Expr} | ExprSeq], undefined, MbScope, TInfo) ->
  ?TRACE("Match expr: ~p", [Expr0]),

  AnnExpr =
    case annotate_expr_seq([Expr], undefined, MbScope, TInfo) of
      [] ->
        % This was an annotation.
        % Annotate the next expression sequence to stitch it.
        ?TRACE("This was an annotated match"),
        {tuple, _, [{atom, _, AnnoName}, {atom, _, MbName}]} = Expr,
        [XX] = annotate_expr_seq([hd(ExprSeq)], {anno, AnnoName, MbName}, MbScope, TInfo),
        ?TRACE("Annotated the next only ~p", [XX]),
        ?TRACE("Annotating the rest of the expressions ~p", [tl(ExprSeq)]),
        [{match, Anno, Pat, XX} | annotate_expr_seq(tl(ExprSeq), undefined, MbScope, TInfo)];
      _ ->
        % This was a normal match.
        ?TRACE("This was a normal match"),
        [Expr0 | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)]
    end;



annotate_expr_seq([_Expr = {'receive', Anno, Clauses} | ExprSeq], {anno, '@state', Regex}, MbScope, TInfo) when is_list(Clauses) ->
  ?TRACE("Receive expr: ~p", [_Expr]),

  % Extract receive type.
  annotate_clauses(Clauses, lists:duplicate(length(Clauses), undefined), MbScope, TInfo),
  [_Expr | annotate_expr_seq(ExprSeq, undefined, MbScope, TInfo)];


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
annotate_expr_seq([Expr | ExprSeq], TypeAnno, MbScope, TInfo) ->
  ?TRACE("Other expr: ~p", [Expr]),
  [Expr | annotate_expr_seq(ExprSeq, TypeAnno, MbScope, TInfo)].


annotate_clauses(Clauses, Types, MbScope, TInfo) when is_list(Clauses) ->
  lists:zipwith(fun(Clause, Type) -> annotate_clause(Clause, Type, MbScope, TInfo) end, Clauses, Types).

%% The following clauses are annotated:
%% 1. Function clause with no guard
%% 2. If
annotate_clause(Clause = {clause, Anno, PatSeq, GuardSeq = [], Body},
    Type = {type, _, 'fun', [{type, _, product, TypeSeq}, RetType]}, MbScope, TInfo)
  when is_list(PatSeq), is_list(TypeSeq), length(PatSeq) == length(TypeSeq),
  is_list(Body) ->
  ?TRACE("Fun clause: ~p", [Clause]),

  ?TRACE("Arg types: ~p", [TypeSeq]),
  ?TRACE("Ret type: ~p", [RetType]),

  ?TRACE("Function pats: ~p", [PatSeq]),
  ?TRACE("Function body: ~p", [Body]),

  AnnPatSeq = annotate_pat_seq(PatSeq, TypeSeq, MbScope, TInfo),

  ?TRACE(">> Ann pats: ~p", [AnnPatSeq]),


  Body0 = annotate_expr_seq(Body, undefined, MbScope, TInfo),

  erl_syntax:revert(
    erl_syntax:set_pos(erl_syntax:clause(AnnPatSeq, GuardSeq, Body0), Anno)
  );


%%annotate_clause(Clause = {clause, Anno, PatSeq, GuardSeq, Body})
%%  when is_list(PatSeq), is_list(GuardSeq), is_list(Body) ->
%%  ?TRACE("Fun clause with guard: ~p", [Clause]),
%%  Clause;
annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    _, MbScope, TInfo)
  when is_list(GuardSeq), is_list(Body) ->
  ?TRACE("If clause: ~p", [Clause]),

  Body0 = annotate_expr_seq(Body, undefined, MbScope, TInfo),


  erl_syntax:clause(GuardSeq, Body0);

%%  Clause;


%% The following clauses are not annotated:
%% 1. Case
%% 2. Case with guard
%% 3. Catch
%% 4. Function clause with guard
annotate_clause(Clause, _, _, _) ->
  ?TRACE("Other clause: ~p", [Clause]),
  Clause.




any() ->
  erl_syntax:revert(erl_syntax:type_application(erl_syntax:atom(any), [])).

none() ->
  erl_syntax:revert(erl_syntax:type_application(erl_syntax:atom(none), [])).


make_type(Name, Args) when is_atom(Name), is_list(Args) ->
  erl_syntax:revert(erl_syntax:type_application(erl_syntax:atom(Name), Args)).

make_type(Name) when is_atom(Name) ->
  make_type(Name, []).

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
  get_anno_val(Anno, type, undefined).

interface(Anno) ->
  get_anno_val(Anno, interface, undefined).

modality(Anno) ->
  get_anno_val(Anno, modality, undefined).

state(Anno) ->
  get_anno_val(Anno, state, undefined).

set_type(Type, Anno) when is_atom(Type) ->
  set_anno_val(Anno, type, Type).

set_interface(Interface, Anno) when is_atom(Interface) ->
  set_anno_val(Anno, interface, Interface).

set_modality(Modality, Anno) when Modality =:= new; Modality =:= use ->
  set_anno_val(Anno, modality, Modality).

set_state(State, Anno) when is_list(State) ->
  set_anno_val(Anno, state, State).



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

% set 0
% set all
% set at


%%map_anno(Anno, Tree) ->
%%  map_anno(Anno, Tree, 0).

%%set_anno(Anno, Tree) ->
%%  set_anno(Anno, Tree, 0).

%%set_anno(Anno, Tree, Depth) ->
%%  Map = fun(_, D) when D == Depth -> {Anno, D + 1}; (A, D) -> {A, D + 1} end,
%%  erl_parse:mapfold_anno(Map, 0, Tree).

%%set_anno_lt(Anno, Tree, Depth) ->
%%  Map = fun(_, D) when D < Depth -> {Anno, D + 1}; (A, D) -> {A, D + 1} end,
%%  erl_parse:mapfold_anno(Map, 0, Tree).

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


