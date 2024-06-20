%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans_3).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%% API
-export([module/1]).
%%-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(MB_IDX_START, 0).

-define(MB_VAR_NAME, "mb").

-spec module(erl_syntax:forms()) -> list().
module(Forms) ->
  forms(Forms ++ [main_fun_def()]).

forms(Forms) when is_list(Forms) ->
  [form(Form) || Form <- Forms].


%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

form({attribute, _, module, Name}) ->
  io_lib:format("# Translated from ~s.erl~n", [Name]);
form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  % Interface with message signature types.
  ?TRACE("Translating interface ~s.", [Name]),
  case type(Type) of
    undefined ->
      pat_syntax:interface_def(Name);
    Type0 ->
      pat_syntax:interface_def(Name, Type0)
  end;
form({function, _, Name, Arity, Clauses = [_]}) ->
  % Function with one clause.
  ?TRACE("Translating function ~s/~b.", [Name, Arity]),
  pat_syntax:fun_def(Name, fun_clauses(Clauses));
form(_) ->
  % Other Erlang forms without Pat equivalent.
  [].

type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. This handles the case where the a mailbox
  % interface type is just a PID.
  undefined;
type({type, _, Type, _Vars = []})
  when
  Type =:= boolean;
  Type =:= integer;
  Type =:= float;
  Type =:= string;
  Type =:= atom ->
  % Literal types.
  pat_syntax:lit_type(Type);
type({type, _, Name, _Vars = []})
  when Name =:= no_return; Name =:= any; Name =:= none ->
  % Unit type.
  pat_syntax:lit_type(unit);
type({atom, _, ok}) ->
  % Atom ok translated as unit type.
  pat_syntax:lit_type(unit);
type({user_type, _, Name, _Vars = []}) ->
  % Mailbox type. Mailbox types default to the write capability.
  pat_syntax:mb_type(Name, write);
type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Message signature type.
  pat_syntax:msg_type(Name, type_seq(TypeSeq));
type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Union of message types.
  pat_syntax:union_type(type_seq(TypeSeq)).

type_seq([]) ->
  [];
type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  % Erlang PID types are not translated and eaten.
  type_seq(TypeSeq);
type_seq([Type | TypeSeq]) ->
  [type(Type) | type_seq(TypeSeq)].


%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------

%% @private Translates receive/case and if clauses.
clauses(Fun, Clauses, MbCtx)
  when
  is_function(Fun, 2), is_list(Clauses) ->
  {Clauses0, MbCtx0} =
    lists:foldl(
      fun(Clause, {Clauses0, MbCtx0}) ->
        {Clause1, MbCtx1} = Fun(Clause, MbCtx0),
        {[Clause1 | Clauses0], MbCtx1}
      end,
      {[], MbCtx}, Clauses
    ),
  {lists:reverse(Clauses0), MbCtx0}.

case_clauses(Clauses, MbCtx) ->
  clauses(fun case_clause/2, Clauses, MbCtx).

%%if_clauses(Clauses, MbCtx) ->
%%  clauses(fun if_clause/2, Clauses, MbCtx).

%% @private Translates a receive/case and if clause.
case_clause(_Clause = {clause, _, PatSeq = [_], _GuardSeq = [], Body}, Mb) ->
  % Unconstrained receive or case clause.
  Mb0 = new_mb(Mb),
  {Expr, Mb1} = expr_seq(Body, Mb0),
  {pat_syntax:receive_expr(pat_seq(PatSeq), pat_syntax:var(make_mb(Mb0)), Expr), Mb1}.

if_clause(_Clause = {clause, _, _PatSeq = [], [[GuardTest]], ExprSeq}, Mb) ->
  % Constrained if clause with exactly one guard and one guard test.
  {Expr0, Mb0} = expr_seq(ExprSeq, Mb),
  {{guard_test(GuardTest), Expr0}, Mb0}.

expr_seq([], Mb) ->
  {[], Mb};
expr_seq([Expr | ExprSeq], Mb) ->
  {Expr0, Rest, Mb0} = expr(Expr, ExprSeq, Mb),
  {ExprSeq0, Mb1} = expr_seq(Rest, Mb0),
  {[Expr0 | ExprSeq0], Mb1}.

%% @private Translates values and expressions.
expr({call, _, {atom, _, self}, _MFArgs = []}, ExprSeq, Mb) ->
  % Self expression.
  MbVar = pat_syntax:var(make_mb(Mb)),
  {pat_syntax:tuple([MbVar, MbVar]), ExprSeq, Mb};

expr(Expr = {call, Anno, {atom, _, Name}, Args}, ExprSeq, Mb) ->
  % Explicit internal function call, and explicit internal mailbox-annotated
  % function call.
  Call0 =
    case paterl_anno:interface(Anno) of
      undefined ->
        % Call to closed function call outside mailbox context.
        {Call, []} = expr(Expr, []),
        pat_syntax:tuple([Call, pat_syntax:var(make_mb(Mb))]);

      _Interface ->
        % Call to an open function call inside mailbox context.
        case paterl_anno:modality(Anno) of
          new ->
            % Inject new mailbox.
            {Call, []} = expr(Expr, []),
            pat_syntax:tuple([Call, pat_syntax:var(make_mb(Mb))]);

          use ->
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(make_mb(Mb))) | Args],
            pat_syntax:call_expr(Name, expr_seq(Args0))
        end
    end,
  {Call0, ExprSeq, Mb};

expr({match, _, Pat, Expr}, ExprSeq, Mb) ->
  % Match expression.
  {Expr0, [], Mb0} = expr(Expr, [], Mb),
  Mb1 = new_mb(Mb0),
  Binders = pat_syntax:tuple([pat(Pat), pat_syntax:var(make_mb(Mb1))]),

  % Rest of Erlang expression sequence is translated because let expressions
  % induce a hierarchy of nested expression contexts that does not align with
  % Erlang match expressions.
  {Body, Mb2} =
    case ExprSeq of
      [] ->
        % Empty let body. Use binders to complete let body.
        {Binders, Mb1};
      ExprSeq ->
        % Non-empty let body.
        expr_seq(ExprSeq, Mb1)
    end,
  {pat_syntax:let_expr(Binders, Expr0, Body), [], Mb2};

expr({'if', _, [Clause0, Clause1]}, ExprSeq, Mb) ->
  % If expression with one user-defined constraint and one catch-all constraint.
  % The two constraints emulate the if and else in Pat branching expressions.
  {{ExprC, ExprT}, Mb0} = if_clause(Clause0, Mb), % If.
  {{"true", ExprF}, Mb1} = if_clause(Clause1, Mb), % Else.

  Mb2 = new_mb(max(Mb0, Mb1)),
  {pat_syntax:if_expr(ExprC, ExprT, ExprF), ExprSeq, Mb2};

expr({'receive', Anno, Clauses}, ExprSeq, Mb) ->
  % Unconstrained receive expression that is translated to Pat guard expression.
  State = paterl_anno:state(Anno),
  {ReceiveClauses, Mb0} = case_clauses(Clauses, Mb),

  % Check whether an empty expression is required.
  ReceiveClauses0 =
    case pat_regex:is_mb_empty(State) of
      true ->
        % Regex may be empty. Add empty expression.
        Mb1 = new_mb(Mb),
        MbVar = pat_syntax:var(make_mb(Mb1)),
        EmptyExpr = pat_syntax:empty_expr(
          MbVar, pat_syntax:tuple([pat_syntax:unit(), MbVar])
        ),
        [EmptyExpr | ReceiveClauses];
      false ->
        ReceiveClauses
    end,

  Guard = pat_syntax:guard_expr(
    pat_syntax:var(make_mb(Mb)), State, ReceiveClauses0
  ),
  {Guard, ExprSeq, Mb0};

expr(Expr, ExprSeq, Mb) ->
  % Literal and variable.
  % Binary and unary operators.
  % Spawn expression.
  {Expr0, []} = expr(Expr, []),
  {pat_syntax:tuple([Expr0, pat_syntax:var(make_mb(Mb))]), ExprSeq, Mb}.


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------

%% @private Translates closed functions and if clauses.
fun_clauses(Clauses) ->
  clauses(fun fun_clause/1, Clauses).

%%if_clauses(Clauses) ->
%%  clauses(fun if_clause/1, Clauses).

clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  [Fun(Clause) || Clause <- Clauses].

%% @private Translates closed function and if clauses.
fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Mailbox-annotated or non mailbox-annotated unconstrained function clause.

  % Translate function return type.
  RetType = type(paterl_anno:type(Anno)),

  % Determine whether function is mailbox-annotated or non mailbox-annotated.
  case paterl_anno:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating NON mailbox-annotated function clause */~b.",
        [length(PatSeq)]
      ),

      % Translate function parameters and body.
      Params = params(PatSeq),
      Expr = expr_seq(Body),

      pat_syntax:fun_clause(Params, Expr, RetType);
    _Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating mailbox-annotated function clause */~b.",
        [length(PatSeq)]
      ),

      % Create mailbox to inject as first parameter of the mailbox-annotated
      % function clause.
      Mb = new_mb(),
      MbType = pat_syntax:mb_type(paterl_anno:interface(Anno), read),
      Params = [
        pat_syntax:param(pat_syntax:var(make_mb(Mb)), MbType) |
        params(PatSeq)
      ],

      {Expr, _} = expr_seq(Body, Mb),
      pat_syntax:fun_clause(Params, Expr, pat_syntax:product_type([RetType, MbType]))
  end.

if_clause({clause, _, _PatSeq = [], [[GuardTest]], Body}) ->
  % Constrained if clause with exactly one guard and one guard test.
  {guard_test(GuardTest), expr_seq(Body)}.

%% @private Translates value and expression sequences.
expr_seq([]) ->
  [];
expr_seq([Expr | ExprSeq]) ->
  {Expr0, Rest} = expr(Expr, ExprSeq),
  [Expr0 | expr_seq(Rest)].

%% @private Translates values and expressions.
expr(Lit, ExprSeq)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  {pat_syntax:lit(element(3, Lit)), ExprSeq};
expr({var, _, Name}, ExprSeq) ->
  % Variable.
  {pat_syntax:var(Name), ExprSeq};
expr({tuple, _, [_Tag = {atom, _, Name} | Args]}, ExprSeq) ->
  % Message.
  {pat_syntax:msg_expr(Name, expr_seq(Args)), ExprSeq};
expr(Expr = {call, _, {atom, _, spawn}, _MFArgs = [_, _Fun, _Args]}, ExprSeq) ->
  % Spawn expression.
  {spawn_expr(Expr), ExprSeq};
expr({call, _, {atom, _, format}, _}, ExprSeq) ->
  % Format call expressions.
  {pat_syntax:unit(), ExprSeq};
expr(Expr = {call, Anno, _Fun = {atom, _, Name}, Args}, ExprSeq) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call. Function calls with use mailbox annotations not permitted.
  case paterl_anno:modality(Anno) of
    undefined ->
      % Call to closed function call outside mailbox context.
      {pat_syntax:call_expr(Name, expr_seq(Args)), ExprSeq};
    new ->
      % New interface modality. Use modality not permitted.
      {new_call_expr(Expr), ExprSeq}
  end;
expr({match, _, Pat, Expr}, ExprSeq) ->
  % Match expression.
  {Expr0, []} = expr(Expr, []),

  % Rest of Erlang expression sequence is translated because let expressions
  % induce a hierarchy of nested expression contexts that does not align with
  % Erlang match expressions.
  Binders = pat(Pat),
  Body =
    case ExprSeq of
      [] ->
        % Empty let body. Use binders to complete let body.
        Binders;
      ExprSeq ->
        % Non-empty let body.
        expr_seq(ExprSeq)
    end,
  {pat_syntax:let_expr(Binders, Expr0, Body), []};
expr({op, _, Op, Expr0, Expr1}, ExprSeq) ->
  % Binary operator expression.
  {ExprL, []} = expr(Expr0, []),
  {ExprR, []} = expr(Expr1, []),
  {pat_syntax:op_expr(Op, ExprL, ExprR), ExprSeq};
expr({op, _, Op, Expr}, ExprSeq) ->
  % Unary operator expression.
  {Expr0, []} = expr(Expr, []),
  {pat_syntax:op_expr(Op, Expr0), ExprSeq};
expr({'if', _, [Clause0, Clause1]}, ExprSeq) ->
  % If expression with one user-defined constraint and one catch-all constraint.
  % The two constraints emulate the if and else in Pat branching expressions.
  {ExprC, ExprT} = if_clause(Clause0), % If
  {"true", ExprF} = if_clause(Clause1), % Else
  {pat_syntax:if_expr(ExprC, ExprT, ExprF), ExprSeq};
expr(Other, ExprSeq) ->
  PatExpr = io_lib:format("<Cannot translate ~p>", [Other]),
  {PatExpr, ExprSeq}.


%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------


%% @private Translates a guard sequence.
guard_seq(GuardSeq) ->
  [guard(Guard) || Guard <- GuardSeq].

%% @private Translates a guard, which is a sequence of guard tests.
guard(GuardTests) ->
  [guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates a guard test.
guard_test(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  pat_syntax:lit(element(3, Lit));
guard_test({var, _, Name}) ->
  % Variable.
  pat_syntax:var(Name);
guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Decidable function call.
  pat_syntax:call_expr(Name, guard(GuardTests));
guard_test({op, _, Op, GuardTest0, GuardTest1}) ->
  % Binary operator.
  pat_syntax:op_expr(Op, guard_test(GuardTest0), guard_test(GuardTest1));
guard_test({op, _, Op, GuardTest}) ->
  % Unary operator.
  pat_syntax:op_expr(Op, guard_test(GuardTest)).

params(PatSeq) ->
  Translate =
    fun(Pat) ->
      Type = paterl_anno:type(_Anno = element(2, Pat)),
      pat_syntax:param(pat(Pat), type(Type))
    end,
  [Translate(Pat) || Pat <- PatSeq].

%% @private Translates pattern sequences.
pat_seq(PatSeq) ->
  [pat(Pat) || Pat <- PatSeq].

%% @private Translates patterns.
pat(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  pat_syntax:lit(element(3, Lit));
pat({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
pat({tuple, _, [_Tag = {atom, _, Name} | Args]}) ->
  % Message.
  pat_syntax:msg_expr(Name, pat_seq(Args)).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

new_mb() ->
  ?MB_IDX_START.

new_mb(Id) ->
  Id + 1.

make_mb(Id) ->
  list_to_atom(?MB_VAR_NAME ++ integer_to_list(Id)).

new_call_expr({call, Anno, Fun = {atom, _, _}, Args}) ->
  % Only function calls with new mailbox-annotation modality are permitted.
  ?assertEqual(new, paterl_anno:modality(Anno)),

  % Create Erlang syntax of function to be called. The created function must
  % be annotated with use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
  ),

  % Create local mailbox IDs.
  MbNew = new_mb(),
  MbCall = new_mb(MbNew),

  % Translate function call with .
  {Call, [], _} = expr(Expr, [], MbNew),

  % Variables used to construct call expression.
  MbVarNew = pat_syntax:var(make_mb(MbNew)),
  MbVarCall = pat_syntax:var(make_mb(MbCall)),
  RetCall = pat_syntax:var(x),

  % Let with free expression.
  LetFree = pat_syntax:let_expr(
    pat_syntax:var(y), pat_syntax:free_expr(MbVarCall), RetCall
  ),

  % Let with call expression.
  LetCall = pat_syntax:let_expr(
    pat_syntax:tuple([RetCall, MbVarCall]), Call, LetFree
  ),

  % Let with new mailbox creation.
  Interface = paterl_anno:interface(Anno),
  pat_syntax:let_expr(
    MbVarNew, pat_syntax:new_expr(pat_syntax:mb_type(Interface)), LetCall
  ).

spawn_expr({call, Anno, {atom, _, spawn}, _MFArgs = [_, Fun, Args]}) ->
  % Create Erlang syntax of function to be spawned. The created function must
  % be annotated with use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, erl_syntax:list_elements(Args)),
      paterl_anno:set_modality(use, Anno))
  ),

  % Create local mailbox IDs.
  MbNew = new_mb(),
  MbCall = new_mb(MbNew),

  % Translate function call.
  {Call, [], MbNew} = expr(Expr, [], MbNew),

  % Variables used to construct spawn expression.
  MbVarNew = pat_syntax:var(make_mb(MbNew)),
  MbVarCall = pat_syntax:var(make_mb(MbCall)),
  RetCall = pat_syntax:var(x),

  % Let with call expression to be spawned.
  LetCall = pat_syntax:let_expr(
    pat_syntax:tuple([RetCall, MbVarCall]),
    Call,
    pat_syntax:free_expr(MbVarCall)
  ),

  % Let with spawn expression.
  LetSpawn = pat_syntax:let_expr(
    pat_syntax:var(y), pat_syntax:spawn_expr(LetCall), MbVarNew
  ),

  % Let with new mailbox creation.
  pat_syntax:let_expr(
    MbVarNew,
    pat_syntax:new_expr(pat_syntax:mb_type(paterl_anno:interface(Anno))),
    LetSpawn
  ).

%% @private Creates the auxiliary main function that is added to the Pat file
%% to complete it.
main_fun_def() ->
  Type = erl_syntax:revert(
    erl_syntax:type_application(erl_syntax:atom(any), [])
  ),

  Call =
    erl_syntax:set_pos(
      erl_syntax:application(erl_syntax:atom(main), []),
      paterl_anno:set_modality(new, paterl_anno:set_interface(main_mb, erl_anno:new(0)))
    ),

  Clause =
    erl_syntax:revert(
      erl_syntax:set_pos(
        erl_syntax:clause([], [Call]),
        paterl_anno:set_type(Type, erl_anno:new(0))

      )),

  erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:function(erl_syntax:atom('main\''), [Clause]),
      paterl_anno:set_type(Type, erl_anno:new(0))
    )).

