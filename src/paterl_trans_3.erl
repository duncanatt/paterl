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
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%% API
-export([]).
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(MB_IDX_START, 0).

-spec translate(erl_syntax:forms()) -> list().
translate(Forms) ->

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

  Fun = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:function(erl_syntax:atom('main\''), [Clause]),
      paterl_anno:set_type(Type, erl_anno:new(0))
    )),

  translate_forms(Forms ++ [Fun]).


translate_forms(Forms) ->
  [translate_form(Form) || Form <- Forms].

%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

translate_form({attribute, _, module, Name}) ->
  io_lib:format("# Translated from ~s.erl~n", [Name]);
translate_form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  % Interface with message signature types.
  ?TRACE("Translating interface ~s.", [Name]),
  case translate_type(Type) of
    undefined ->
      pat_syntax:iface_def(Name);
    Type0 ->
      pat_syntax:iface_def(Name, Type0)
  end;
translate_form({function, _, Name, Arity, Clauses = [_]}) ->
  % Function with one clause.
  ?TRACE("Translating function ~s/~b.", [Name, Arity]),
  pat_syntax:fun_def(Name, fun_clauses(Clauses));
translate_form(_) ->
  % Other Erlang module attributes without Pat equivalent.
  [].

translate_type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. This handles the case where the a mailbox
  % interface type is just a PID.
  undefined;
translate_type({type, _, integer, _Vars = []}) ->
  % Integer type.
  pat_syntax:lit_type(integer);
translate_type({type, _, float, _Vars = []}) ->
  % Float type.
  pat_syntax:lit_type(float);
translate_type({type, _, string, _Vars = []}) ->
  % String type.
  pat_syntax:lit_type(string);
translate_type({type, _, Name, _Vars = []})
  when Name =:= no_return; Name =:= any; Name =:= none ->
  % Unit type.
  pat_syntax:lit_type(unit);
translate_type({atom, _, ok}) ->
  % Atom ok translated as unit type.
  pat_syntax:lit_type(unit);
translate_type({user_type, _, Name, _Vars = []}) ->
  % Mailbox type. Mailbox types default to the write capability.
  pat_syntax:mb_type(Name, write);
translate_type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Message signature type.
  pat_syntax:msg_type(Name, translate_type_seq(TypeSeq));
translate_type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Union of message types.
  pat_syntax:union_type(translate_type_seq(TypeSeq)).

translate_type_seq([]) ->
  [];
translate_type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  % Erlang PID types are not translated and eaten.
  translate_type_seq(TypeSeq);
translate_type_seq([Type | TypeSeq]) ->
  [translate_type(Type) | translate_type_seq(TypeSeq)].

make_type_name(Type) when is_atom(Type) ->
  string:titlecase(atom_to_list(Type)).

make_fun_name(Fun) when is_atom(Fun) ->
  atom_to_list(Fun).


%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------

%% @private Translates receive/case and if clauses.
translate_open_clauses(Fun, Clauses, MbCtx)
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

translate_case_clauses(Clauses, MbCtx) ->
  translate_open_clauses(fun translate_case_clause/2, Clauses, MbCtx).

translate_if_clauses(Clauses, MbCtx) ->
  translate_open_clauses(fun translate_if_clause/2, Clauses, MbCtx).

%% @private Translates a receive/case and if clause.
translate_case_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, MbCtx) ->
  % Unconstrained receive or case clause.
  MbCtx0 = new_mb(MbCtx),
  {Body0, MbCtx1} = translate_expr_seq(Body, MbCtx0),
  Receive = pat_syntax:receive_expr(translate_pat_seq(PatSeq), MbCtx0, Body0),
  {Receive, MbCtx1}.

translate_if_clause(_Clause = {clause, _, _PatSeq = [], GuardSeq = [_], Body}, MbCtx) ->
  % Constrained if clause.
  {Expr, MbCtx0} = translate_expr_seq(Body, MbCtx),

  Expr1 =
    case translate_guard_seq(GuardSeq) of
      [["true"]] ->
        % Default Erlang if guard equates to else branch in Pat.
        Expr;
      GuardSeq0 ->
        % If branch.
        {GuardSeq0, Expr}
    end,
  {Expr1, MbCtx0}.


translate_expr_seq([], MbCtx) ->
  {[], MbCtx};
translate_expr_seq([Expr | ExprSeq], MbCtx) ->
  {Expr0, Rest, MbCtx0} = translate_expr(Expr, ExprSeq, MbCtx),
  {Exprs0, MbCtx1} = translate_expr_seq(Rest, MbCtx0),
  {[Expr0 | Exprs0], MbCtx1}.

%% @private Translates values and expressions.
translate_expr({call, _, {atom, _, self}, _MFArgs = []}, ExprSeq, MbCtx) ->
  % Self expression.
  Self = pat_syntax:tuple([MbCtx, MbCtx]),
  {Self, ExprSeq, MbCtx};

translate_expr(Expr = {call, Anno, {atom, _, Name}, Args}, ExprSeq, MbCtx) ->
  % Explicit internal function call, and explicit internal mailbox-annotated
  % function call.
  Call0 =
    case paterl_anno:interface(Anno) of
      undefined ->
        % Call to closed function call outside mailbox context.
        {Call, []} = closed_expr(Expr, []),
        pat_syntax:tuple([Call, pat_syntax:var(list_to_atom(MbCtx))]);

      _ ->
        % Call to an open function call inside mailbox context.
        case paterl_anno:modality(Anno) of
          new ->
            % Inject new mailbox.
            {Call, []} = closed_expr(Expr, []),
            pat_syntax:tuple([Call, pat_syntax:var(list_to_atom(MbCtx))]);

          use ->
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(MbCtx)) | Args],
            pat_syntax:call_expr(Name, closed_expr_seq(Args0))
        end
    end,
  {Call0, ExprSeq, MbCtx};



translate_expr({match, _, Pat, Expr}, ExprSeq, MbCtx) ->
  % Match expression.
  {Expr0, [], MbCtx0} = translate_expr(Expr, [], MbCtx),
  MbCtx1 = new_mb(MbCtx0),
  Binders = pat_syntax:tuple([translate_pat(Pat), MbCtx1]),

  % Rest of Erlang expression sequence is translated because let expressions
  % induce a hierarchy of nested expression contexts that does not align with
  % Erlang match expressions.
  {Body, MbCtx3} =
    case translate_expr_seq(ExprSeq, MbCtx1) of
      {[], MbCtx1} ->
        % Empty let body. Use binders to complete let body.
        {Binders, MbCtx1};
      {Expr1, MbCtx2} ->
        % Non-empty let body.
        {Expr1, MbCtx2}
    end,
  {pat_syntax:let_expr(Binders, Expr0, Body), [], MbCtx3};

translate_expr({'if', _, [Clause0, Clause1]}, ExprSeq, MbCtx) ->
  % If expression with one user-defined constraint and one catch-all constraint.
  % The two constraints emulate the if and else in Pat branching expressions.
  {{ExprC, ExprT}, MbCtx} = translate_if_clause(Clause0, MbCtx), % If.
  {ExprF, MbCtx1} = translate_if_clause(Clause1, MbCtx), % Else.

  IfExpr = pat_syntax:if_expr(ExprC, ExprT, ExprF),
  MbCtx2 = new_mb(MbCtx1),
  {IfExpr, ExprSeq, MbCtx2};

translate_expr({'receive', Anno, Clauses}, ExprSeq, MbCtx) ->
  % Unconstrained receive expression that is translated to Pat guard expression.
  State = paterl_anno:state(Anno),
  {ReceiveExprSeq, MbCtx0} = translate_case_clauses(Clauses, MbCtx),

  % Check whether an empty expression is required.
  ReceiveExprSeq0 =
    case pat_regex:is_mb_empty(State) of
      true ->
        % Regex may be empty. Add empty expression.
        MbCtx1 = new_mb(MbCtx),
        MbVar = pat_syntax:var(list_to_atom(MbCtx1)),
        EmptyExpr = pat_syntax:empty_expr(
          MbVar, pat_syntax:tuple([pat_syntax:unit(), MbVar])
        ),
        [EmptyExpr | ReceiveExprSeq];
      false ->
        ReceiveExprSeq
    end,

  Guard = pat_syntax:guard_expr(
    pat_syntax:var(list_to_atom(MbCtx)), State, ReceiveExprSeq0
  ),
  {Guard, ExprSeq, MbCtx0};

translate_expr(Expr, ExprSeq, MbCtx) ->
  % Literal and variable.
  % Binary and unary operators.
  % Spawn expression.
  {Expr0, []} = closed_expr(Expr, []),
  Tuple = pat_syntax:tuple([Expr0, pat_syntax:var(list_to_atom(MbCtx))]),
  {Tuple, ExprSeq, MbCtx}.


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------

%% @private Translates closed functions and if clauses.
fun_clauses(Clauses) ->
  closed_clauses(Clauses, fun fun_clause/1).

if_clauses(Clauses) ->
  closed_clauses(Clauses, fun translate_if_clause/1).

closed_clauses(Clauses, Fun) when is_list(Clauses), is_function(Fun, 1) ->
  [Fun(Clause) || Clause <- Clauses].

%% @private Translates closed function and if clauses.
fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Mailbox-annotated or non mailbox-annotated unconstrained function clause.

  % Translate function return type.
  RetType = translate_type(paterl_anno:type(Anno)),

  % Determine whether function is mailbox-annotated or non mailbox-annotated.
  case paterl_anno:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating NON mailbox-annotated function clause */~b.",
        [length(PatSeq)]
      ),

      % Translate function parameters and body.
      Params = translate_params(PatSeq),
      Expr = closed_expr_seq(Body),

      pat_syntax:fun_clause(Params, Expr, RetType);
    Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating mailbox-annotated function clause */~b.",
        [length(PatSeq)]
      ),

      % Create mailbox to inject as first parameter of the mailbox-annotated
      % function clause.
      MbCtx = new_mb(),
      MbType = pat_syntax:mb_type(paterl_anno:interface(Anno), read),
      Params = [
        pat_syntax:param(pat_syntax:var(list_to_atom(MbCtx)), MbType) |
        translate_params(PatSeq)
      ],

      {Expr, _} = translate_expr_seq(Body, MbCtx),
      pat_syntax:fun_clause(Params, Expr, pat_syntax:prod_type([RetType, MbType]))
  end.

translate_if_clause({clause, _, _PatSeq = [], GuardSeq = [_], Body}) ->
  % Constrained if clause.
  Expr = closed_expr_seq(Body),

  % Determine whether clause is the default catch-all true clause.
  case translate_guard_seq(GuardSeq) of
    [["true"]] ->
      % Default Erlang if guard equates to else branch in Pat.
      Expr;
    GuardSeq0 ->
      % If branch.
      {GuardSeq0, Expr}
  end.


%% @private Translates value and expression sequences.
closed_expr_seq([]) ->
  [];
closed_expr_seq([Expr | ExprSeq]) ->
  {Expr0, Rest} = closed_expr(Expr, ExprSeq),
  [Expr0 | closed_expr_seq(Rest)].

%% @private Translates values and expressions.
closed_expr(Lit, ExprSeq)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  {pat_syntax:lit(element(3, Lit)), ExprSeq};
closed_expr({var, _, Name}, ExprSeq) ->
  % Variable.
  {pat_syntax:var(Name), ExprSeq};
closed_expr({tuple, _, [_Tag = {atom, _, Name} | Args]}, ExprSeq) ->
  % Message.
  {pat_syntax:msg_expr(Name, closed_expr_seq(Args)), ExprSeq};
closed_expr({call, Anno, {atom, _, spawn}, _MFArgs = [_, Fun, Args]}, ExprSeq) ->
  % Spawn expression.
  Interface = paterl_anno:interface(Anno),

  MbCtx0 = new_mb(),
  MbCtx1 = new_mb(MbCtx0),

  % Create Erlang syntax of function to be spawned.
  Args0 = erl_syntax:list_elements(Args),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args0), paterl_anno:set_modality(use, Anno))
  ),

  % Translate function call.
  {Call, [], MbCtx0} = translate_expr(Expr0, [], MbCtx0),

  %% TODO: START Refactor this to own call.
  % Variables used to construct spawn expression.
  MbVarNew = pat_syntax:var(list_to_atom(MbCtx0)),
  MbVarCall = pat_syntax:var(list_to_atom(MbCtx1)),
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
  LetNewMb = pat_syntax:let_expr(
    MbVarNew, pat_syntax:new_expr(pat_syntax:mb_type(Interface)), LetSpawn
  ),

  %% TODO: END Refactor this to own call.
  {LetNewMb, ExprSeq};

closed_expr({call, _, {atom, _, format}, _}, ExprSeq) ->
  % Format call expressions.
  {pat_syntax:unit(), ExprSeq};
closed_expr({call, Anno, Fun = {atom, _, Name}, Args}, ExprSeq) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call. Function calls with use mailbox annotations not permitted.
  Expr =
    case paterl_anno:modality(Anno) of
      undefined ->
        % Call to closed function call outside mailbox context.
        pat_syntax:call_expr(Name, closed_expr_seq(Args));
      new ->
        % New interface modality. Use modality not permitted.
        Interface = make_type_name(paterl_anno:interface(Anno)),
        Interface0 = paterl_anno:interface(Anno),

        MbCtx0 = new_mb(),
        MbCtx1 = new_mb(MbCtx0),

        % Create Erlang syntax of function to be called.
        Expr0 = erl_syntax:revert(
          erl_syntax:set_pos(
            erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
        ),

        % Translate function call.
        {Call, [], _} = translate_expr(Expr0, [], MbCtx0),

        %% TODO: START Refactor this to own call.
        % Variables used to construct call expression.
        MbVarNew = pat_syntax:var(list_to_atom(MbCtx0)),
        MbVarCall = pat_syntax:var(list_to_atom(MbCtx1)),
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
        pat_syntax:let_expr(
          MbVarNew, pat_syntax:new_expr(pat_syntax:mb_type(Interface0)), LetCall
        )
        %% TODO: END Refactor this to own call.
    end,
  {Expr, ExprSeq};
closed_expr({match, _, Pat, Expr}, ExprSeq) ->
  % Match expression.
  {Expr0, []} = closed_expr(Expr, []),

  % Rest of Erlang expression sequence is translated because let expressions
  % induce a hierarchy of nested expression contexts that does not align with
  % Erlang match expressions.
  Binders = translate_pat(Pat),
  Body =
    case closed_expr_seq(ExprSeq) of
      [] ->
        % Empty let body. Use binders to complete let body.
        Binders;
      Expr1 ->
        % Non-empty let body.
        Expr1
    end,
  {pat_syntax:let_expr(Binders, Expr0, Body), []};
closed_expr({op, _, Op, Expr0, Expr1}, ExprSeq) ->
  % Binary operator expression.
  {ExprL, []} = closed_expr(Expr0, []),
  {ExprR, []} = closed_expr(Expr1, []),
  {pat_syntax:op_expr(Op, ExprL, ExprR), ExprSeq};
closed_expr({op, _, Op, Expr}, ExprSeq) ->
  % Unary operator expression.
  {Expr0, []} = closed_expr(Expr, []),
  {pat_syntax:op_expr(Op, Expr0), ExprSeq};
closed_expr({'if', _, [Clause0, Clause1]}, ExprSeq) ->
  % If expression with one user-defined constraint and one catch-all constraint.
  % The two constraints emulate the if and else in Pat branching expressions.
  {ExprC, ExprT} = translate_if_clause(Clause0), % If
  ExprF = translate_if_clause(Clause1), % Else
  {pat_syntax:if_expr(ExprC, ExprT, ExprF), ExprSeq};
closed_expr(Other, ExprSeq) ->
  PatExpr = io_lib:format("<Cannot translate ~p>", [Other]),
  {PatExpr, ExprSeq}.


%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------

%% @private Translates a guard sequence.
translate_guard_seq(GuardSeq) ->
  [translate_guard(Guard) || Guard <- GuardSeq].

%% @private Translates a guard, which is a sequence of guard tests.
translate_guard(GuardTests) ->
  [translate_guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates a guard test.
translate_guard_test(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  pat_syntax:lit(element(3, Lit));
translate_guard_test({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Function call.
  % TODO: I think the arguments need to be translated as expressions and not guard tests.
  GuardTests0 = string:join(translate_guard(GuardTests), ","),
  [atom_to_list(Name), "(", GuardTests0, ")"];
translate_guard_test({op, _, Op, GuardTest0, GuardTest1}) ->
  % Binary operator.
  GuardTest2 = translate_guard_test(GuardTest0),
  GuardTest3 = translate_guard_test(GuardTest1),
  [GuardTest2, " ", atom_to_list(Op), " ", GuardTest3];
translate_guard_test({op, _, Op, GuardTest}) ->
  % Unary operator.
  GuardTest0 = translate_guard_test(GuardTest),
  [atom_to_list(Op), GuardTest0].

translate_params(PatSeq) ->
  Translate =
    fun(Pat) ->
      Type = paterl_anno:type(_Anno = element(2, Pat)),
      pat_syntax:param(translate_pat(Pat), translate_type(Type))
    end,
  [Translate(Pat) || Pat <- PatSeq].

%% @private Translates pattern sequences.
translate_pat_seq(PatSeq) ->
  [translate_pat(Pat) || Pat <- PatSeq].

%% @private Translates patterns.
translate_pat(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  pat_syntax:lit(element(3, Lit));
translate_pat({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_pat({tuple, _, [_Tag = {atom, _, Name} | Args]}) ->
  % Message.
  pat_syntax:msg_expr(Name, translate_pat_seq(Args)).



%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%%make_mb(MbId) ->
%%  "mb" + integer_to_list(MbId).

%%new_mb1(MbId) ->
%%  {"mb" ++ integer_to_list(MbId), MbId + 1}.

new_mb() ->
  "mb" ++ integer_to_list(?MB_IDX_START).

new_mb([$m, $b | IdStr]) ->
  "mb" ++ integer_to_list(list_to_integer(IdStr) + 1).

%%reset_mb() ->
%%  put(mb, 0).
%%
%%new_mb() ->
%%  "mb" ++ integer_to_list(put(mb, get(mb) + 1)).

%%new_mb() ->
%%  "mb" ++
%%  integer_to_list(
%%    case get(mb) of
%%      undefined ->
%%        put(mb, 1),
%%        0;
%%      Mb ->
%%        put(mb, Mb + 1),
%%        Mb
%%    end
%%  ).



