%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans_4).
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

%% Mailbox variable name.
-define(MB_VAR_NAME, mb).

-spec module(erl_syntax:forms()) -> list().
%% @doc Transforms an Erlang abstract syntax representation to its equivalent
%% Pat abstract syntax representation.
module(Forms) ->
  forms(Forms ++ [main_fun_def()]).


%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

%% @private Translates Erlang forms.
forms(Forms) when is_list(Forms) ->
%%  [case form(Form) of undefined -> end || Form <- Forms].
  [Form0 || Form <- Forms, (Form0 = form(Form)) =/= undefined].

%% @private Translates Erlang forms.
form({attribute, _, module, Name}) ->
%%  io_lib:format("# Translated from ~s.erl~n", [Name]);
  pat_syntax:comment("Translated from " ++ atom_to_list(Name) ++ ".erl");
form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  % Erlang interface with message signature types.
  ?TRACE("Translating interface ~s.", [Name]),
  case type(Type) of
    undefined ->
      % Empty interface.
      pat_syntax:interface_def(Name);
    Type0 ->
      % Non-empty interface.
      pat_syntax:interface_def(Name, Type0)
  end;
form({function, _, Name, Arity, Clauses = [_]}) ->
  % Erlang function with one clause.
  ?TRACE("Translating function ~s/~b.", [Name, Arity]),
  pat_syntax:fun_def(Name, fun_clauses(Clauses));
form(_) ->
  % Skip other Erlang forms without Pat equivalent.
  undefined.

%% @private Translates Erlang type definitions.
type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. Clause handles the case where a mailbox
  % interface type is just a PID. Mailbox interfaces whose type is just a PID
  % are considered empty Pat mailbox interface definitions.
  undefined;
type({type, _, Type, _Vars = []})
  when
  Type =:= boolean;
  Type =:= integer;
  Type =:= float;
  Type =:= string;
  Type =:= atom ->
  % Erlang literal types.
  pat_syntax:lit_type(Type);
type({type, _, Name, _Vars = []})
  when Name =:= no_return; Name =:= any; Name =:= none ->
  % Erlang special type translated as Pat unit type.
  pat_syntax:lit_type(unit);
type({atom, _, ok}) ->
  % Erlang atom 'ok' translated as Pat unit type.
  pat_syntax:lit_type(unit);
type({user_type, _, Name, _Vars = []}) ->
  % Erlang mailbox type. Mailbox types default to the write capability.
  pat_syntax:mb_type(Name, write);
type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Erlang message signature type.
  pat_syntax:msg_type(Name, type_seq(TypeSeq));
type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Erlang union of message types.
  pat_syntax:union_type(type_seq(TypeSeq)).

%% @private Translates Erlang type definition sequences.
type_seq([]) ->
  [];
type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  % Erlang PID types are not translated and dropped.
  type_seq(TypeSeq);
type_seq([Type | TypeSeq]) ->
  [type(Type) | type_seq(TypeSeq)].


%%% ----------------------------------------------------------------------------
%%% Translation on terms in mailbox context.
%%% ----------------------------------------------------------------------------

%% @private Generic function to translate Erlang clauses in mailbox context.
clauses(Fun, Clauses, Mb) when is_function(Fun, 2), is_list(Clauses) ->
  [Fun(Clause, Mb) || Clause <- Clauses].

%% @private Translates Erlang case and receive clauses.
case_clauses(Clauses, Mb) ->
  ?TRACE("(~s) Translating case/receive clauses.", [Mb]),
  clauses(fun case_clause/2, Clauses, Mb).

%% @private Translates Erlang if clauses.
if_clauses(Clauses, Mb) ->
  ?TRACE("(~s) Translating if clauses.", [Mb]),
  clauses(fun if_clause/2, Clauses, Mb).

%% @private Translates Erlang case, receive, and if clauses.
case_clause(_Clause = {clause, _, PatSeq = [_], _GuardSeq = [], Body}, _Mb) ->
  % Erlang unconstrained case and receive clause.
  ?TRACE("(~s) Translating case/receive clause.", [_Mb]),
  Mb = fresh_mb(),
  [Expr] = expr_seq(Body, Mb),
  [PatSeq0] = pat_seq(PatSeq),
  pat_syntax:receive_expr(PatSeq0, pat_syntax:var(Mb), Expr).
if_clause(_Clause = {clause, _, _PatSeq = [], [[GuardTest]], ExprSeq}, Mb) ->
  % Erlang constrained if clause with exactly one guard and one guard test.
  ?TRACE("(~s) Translating if clause.", [Mb]),
  [Expr0] = expr_seq(ExprSeq, Mb),
  {guard_test(GuardTest), Expr0}.

%% @private Translates Erlang expression sequences.
expr_seq([], _) ->
  [];
expr_seq([Expr | ExprSeq], Mb) ->
  {Expr0, Rest} = expr(Expr, ExprSeq, Mb),
  ExprSeq0 = expr_seq(Rest, Mb),
  [Expr0 | ExprSeq0].

%% @private Translates Erlang values and expressions.
expr({call, _, {atom, _, self}, _MFArgs = []}, ExprSeq, Mb) ->
  % Erlang self function call expression.
  ?TRACE("(~s) Translating self expression.", [Mb]),
  MbVar = pat_syntax:var(Mb),
  {pat_syntax:tuple([MbVar, MbVar]), ExprSeq};
expr(Expr = {call, _, {atom, _, spawn}, _Args}, ExprSeq, Mb) ->
  ?TRACE("(~s) Translating spawn expression.", [Mb]),
  {Expr0, []} = expr(Expr, []),
  {pat_syntax:tuple([Expr0, pat_syntax:var(Mb)]), ExprSeq};
expr(Expr = {call, Anno, {atom, _, Name}, Args}, ExprSeq, Mb) ->
  % Erlang explicit internal function call and explicit internal
  % mailbox-annotated function call.
  % Erlang implicit function calls (i.e. where the function name
  % is derived from an expression) not supported. External function calls are
  % not supported either.
  Call0 =
    case paterl_anno:interface(Anno) of
      undefined ->
        % Call to function call outside mailbox context.
        ?TRACE("(~s) Translating call to ~s/~b.", [Mb, Name, length(Args)]),
        {Call, []} = expr(Expr, []),
        pat_syntax:tuple([Call, pat_syntax:var(Mb)]);

      _Interface ->
        % Call to function call inside mailbox context.
        Modality = paterl_anno:modality(Anno),
        ?TRACE("(~s) Translating call to ~s/~b [~s, ~s].", [
          Mb, Name, length(Args), _Interface, Modality
        ]),
        case Modality of
          new ->
            % Inject new mailbox.
            {Call, []} = expr(Expr, []),
            pat_syntax:tuple([Call, pat_syntax:var(Mb)]);

          use ->
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(Mb)) | Args],
            pat_syntax:call_expr(Name, expr_seq(Args0))
        end
    end,
  {Call0, ExprSeq};
expr({match, _, Pat, Expr}, ExprSeq, Mb) ->
  % Erlang match expression.
  ?TRACE("(~s) Translating match expression.", [Mb]),
  {Expr0, []} = expr(Expr, [], Mb),
  Mb0 = fresh_mb(),
  Binders = pat_syntax:tuple([pat(Pat), pat_syntax:var(Mb0)]),

  % Rest of Erlang expression sequence is translated because let expressions
  % induce a hierarchy of nested Pat expression contexts that does not align
  % with Erlang match expression sequences. This means that an Erlang expression
  % sequence is always reduced to a singleton list, which consists either of
  % the translated Erlang expression forming the root context of the let-binding
  % or the singleton list consisting of the expression in the pattern, when the
  % expression is the list expression in the sequence.
  [Body] =
    case ExprSeq of
      [] ->
        % Empty let body. Use binders to complete let body.
        [Binders];
      ExprSeq ->
        % Non-empty let body.
        expr_seq(ExprSeq, Mb0)
    end,
  ?INFO("@@@@ [Inside MB context] About to create let expression."),
  {pat_syntax:let_expr(Binders, Expr0, Body), []};
expr({'if', _, [Clause0, Clause1]}, ExprSeq, Mb) ->
  % Erlang if expression with exactly one used-defined constraint and one catch-
  % all constraint. These pair of constraints emulate if and else Pat branches.
  ?TRACE("(~s) Translating if-else expression.", [Mb]),
  {ExprC, ExprT} = if_clause(Clause0, Mb), % If.

  ?TRACE("Translated else clause is: ~p~n~n", [if_clause(Clause1, Mb)]),

%%  {"true", ExprF} = if_clause(Clause1, Mb), % Else.
  {{boolean, _, true}, ExprF} = if_clause(Clause1, Mb), % Else.
  {pat_syntax:if_expr(ExprC, ExprT, ExprF), ExprSeq};
expr({'receive', Anno, Clauses}, ExprSeq, Mb) ->
  % Erlang unconstrained receive expression. Translated to Pat guard expression.
  State = paterl_anno:state(Anno),
  ?TRACE("(~s) Translating receive expression guarding on '~s'.", [Mb, State]),
  ReceiveClauses = case_clauses(Clauses, Mb),

  % Check whether a Pat empty expression is required.
  ReceiveClauses0 =
    case pat_regex:is_mb_empty(State) of
      true ->
        % Regex may be empty. Add Pat empty expression.
        MbVar = pat_syntax:var(fresh_mb()),
        EmptyExpr = pat_syntax:empty_expr(
          MbVar, pat_syntax:tuple([pat_syntax:unit(), MbVar])
        ),
        [EmptyExpr | ReceiveClauses];
      false ->
        % Regex not empty.
        ReceiveClauses
    end,

  ?TRACE("(~s) Generated guard on '~s' with ~b clause(s).", [
    Mb, State, length(ReceiveClauses0)
  ]),
  Guard = pat_syntax:guard_expr(
    pat_syntax:var(Mb), State, ReceiveClauses0
  ),
  {Guard, ExprSeq};
expr(Expr, ExprSeq, Mb) ->
  % Erlang literals and variables.
  % Erlang binary and unary operators.
  % Erlang spawn expression.
  {Expr0, []} = expr(Expr, []),
  {pat_syntax:tuple([Expr0, pat_syntax:var(Mb)]), ExprSeq}.


%%% ----------------------------------------------------------------------------
%%% Translation on terms outside mailbox context.
%%% ----------------------------------------------------------------------------

%% @private Generic function to translate Erlang clauses outside mailbox context.
clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  [Fun(Clause) || Clause <- Clauses].

%% @private Translates Erlang function clauses.
fun_clauses(Clauses) ->
  clauses(fun fun_clause/1, Clauses).

%% @private Translates Erlang if clauses.
if_clauses(Clauses) ->
  clauses(fun if_clause/1, Clauses).

%% @private Translates Erlang function and if clauses.
fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Erlang mailbox-annotated or non mailbox-annotated unconstrained function
  % clause.

  % Translate function return type.
  RetType = type(paterl_anno:type(Anno)),

  % Determine whether function is mailbox-annotated or non mailbox-annotated.
  case paterl_anno:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating NON mailbox-annotated function clause */~b.", [
        length(PatSeq)
      ]),

      % Translate function parameters and body.
      Params = params(PatSeq),
%%      [Expr] = expr_seq(Body),
      [Expr] = expr_seq(Body),
      ?TRACE("------------------------------------>> Expr: ~p~n", [Expr]),
      pat_syntax:fun_clause(Params, Expr, RetType);
    _Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating mailbox-annotated function clause */~b.", [
        length(PatSeq)
      ]),

      % Create mailbox to inject as first parameter of the mailbox-annotated
      % function clause.
      Mb = fresh_mb(),
      MbType = pat_syntax:mb_type(paterl_anno:interface(Anno), read),
      Params = [
        pat_syntax:param(pat_syntax:var(Mb), MbType) |
        params(PatSeq)
      ],

      [Expr] = expr_seq(Body, Mb),
      pat_syntax:fun_clause(Params, Expr, pat_syntax:product_type([RetType, MbType]))
  end.
if_clause({clause, _, _PatSeq = [], [[GuardTest]], Body}) ->
  % Erlang constrained if clause with exactly one guard and one guard test.
  ?TRACE("Translating if clause."),
  {guard_test(GuardTest), expr_seq(Body)}.


%% @private Translates Erlang expression sequences.
expr_seq([]) ->
  [];
expr_seq([Expr | ExprSeq]) ->
  ?TRACE("Translating the expression ~p in sequence.", [Expr]),
  Tmp = expr(Expr, ExprSeq),
  ?TRACE("Translated expr ~p (~p)", [Tmp, tuple_size(Tmp)]),
  {Expr0, Rest} = Tmp,
  [Expr0 | expr_seq(Rest)].

% TODO: I think the cleanest way to write this function, since we need to
% TODO: convert a list of Erlang expressions into a single Pat let expression,
% TODO: is to incorporate the expr_seq above with the expr function such that
% TODO: the signature is: expr_seq([Expr | ExprSeq]) with clauses for each case.
% TODO: The advantage of this is two-fold. One, we eliminate the extra logic
% TODO: with users of the function which need to case-split on empty and
% TODO: singleton list expressions, although we need to case spilt on the length
% TODO: of the remaining list. Two, the function takes two parameters, one of
% TODO: which is the list of expressions, which maybe makes sense to consolidate
% TODO: it into one in order to make it cleaner.
%% @private Translates values and expressions.
expr(Lit, ExprSeq)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Erlang literal expressions.
  ?TRACE("Translating literal ~s ~p.", [element(1, Lit), element(3, Lit)]),
  {pat_syntax:lit(element(3, Lit)), ExprSeq};
expr({var, _, Name}, ExprSeq) ->
  % Erlang variable.
  ?TRACE("Translating variable '~s'.", [Name]),
  {pat_syntax:var(Name), ExprSeq};
expr({tuple, _, [_Tag = {atom, _, Name} | Args]}, ExprSeq) ->
  % Erlang Pat message expression.
  ?TRACE("Translating message with tag '~s'.", [Name]),
  {pat_syntax:msg_expr(Name, expr_seq(Args)), ExprSeq};
expr(Expr = {call, _, {atom, _, spawn}, _MFArgs = [_, _Fun, _Args]}, ExprSeq) ->
  % Erlang spawn function call expression.
  ?TRACE("Translating call to spawn ~s/~b.", [element(3, _Fun), erl_syntax:list_length(_Args)]),
  {spawn_expr(Expr), ExprSeq};
expr({call, _, {atom, _, format}, _}, ExprSeq) ->
  % Erlang format function call expression.
  ?TRACE("Translating call to format."),
  {pat_syntax:unit(), ExprSeq};
expr(Expr = {call, Anno, _Fun = {atom, _, Name}, Args}, ExprSeq) ->
  % Erlang explicit internal function call and explicit internal
  % mailbox-annotated function call.
  % Internal or external function calls with use mailbox annotations are not
  % permitted.
%%  case paterl_anno:modality(Anno) of
%%    undefined ->
%%      % Call to function outside mailbox context.
%%      {pat_syntax:call_expr(Name, expr_seq(Args)), ExprSeq};
%%    new ->
%%      % Call to function inside mailbox context with new modality.
%%      {new_call_expr(Expr), ExprSeq}
%%  end;
  case paterl_anno:interface(Anno) of
    undefined ->
      % Call to function outside mailbox context.
      {pat_syntax:call_expr(Name, expr_seq(Args)), ExprSeq};
    _Interface ->
      % Call to function inside mailbox context. Only the new modality is
      % permitted at this point.
      Modality = paterl_anno:modality(Anno),
      ?assertEqual(Modality, new),

      ?TRACE("Translating call to ~s/~b [~s, ~s].", [
        Name, length(Args), _Interface, Modality
      ]),
      {new_call_expr(Expr), ExprSeq}
  end;
expr({match, _, Pat, Expr}, ExprSeq) ->
  % Erlang match expression.
  ?TRACE("Translating match expression."),
  {Expr0, []} = expr(Expr, []),

  % Rest of Erlang expression sequence is translated because let expressions
  % induce a hierarchy of nested Pat expression contexts that does not align
  % with Erlang match expression sequences.
  Binders = pat(Pat),
  [Body] =
    case ExprSeq of
      [] ->
        % Empty let body. Use binders to complete let body.
        [Binders];
      ExprSeq ->
        % Non-empty let body.
        expr_seq(ExprSeq)
    end,
  ?INFO("@@@@ [Outside MB context] About to create let expression."),
  {pat_syntax:let_expr(Binders, Expr0, Body), []};
expr({op, _, Op, Expr0, Expr1}, ExprSeq) ->
  % Erlang binary operator expression.
  ?TRACE("Translating binary operator expression ~s.", [Op]),
  {ExprL, []} = expr(Expr0, []),
  {ExprR, []} = expr(Expr1, []),
  {pat_syntax:op_expr(to_pat_op(Op), ExprL, ExprR), ExprSeq};
expr({op, _, Op, Expr}, ExprSeq) ->
  % Erlang unary operator expression.
  ?TRACE("Translating unary operator expression ~s.", [Op]),
  {Expr0, []} = expr(Expr, []),
  {pat_syntax:op_expr(to_pat_op(Op), Expr0), ExprSeq};
expr({'if', _, [Clause0, Clause1]}, ExprSeq) ->
  % Erlang if expression with exactly one used-defined constraint and one catch-
  % all constraint. These pair of constraints emulate if and else Pat branches.
  ?TRACE("Translating if-else expression."),
  {ExprC, ExprT} = if_clause(Clause0), % If
%%  {"true", ExprF} = if_clause(Clause1), % Else
  {{boolean, _, true}, ExprF} = if_clause(Clause1), % Else
  {pat_syntax:if_expr(ExprC, ExprT, ExprF), ExprSeq};
expr(Other, ExprSeq) ->
  % Erlang unsupported expressions.
  PatExpr = io_lib:format("<Cannot translate ~p>", [Other]),
  {PatExpr, ExprSeq}.


%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------

%% @private Translates Erlang guard sequences.
guard_seq(GuardSeq) ->
  [guard(Guard) || Guard <- GuardSeq].

%% @private Translates an Erlang guard, which is a sequence of guard tests.
guard(GuardTests) ->
  [guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates an Erlang guard test.
guard_test(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Erlang literal guard tests.
  pat_syntax:lit(element(3, Lit));
guard_test({var, _, Name}) ->
  % Erlang variable guard test.
  pat_syntax:var(Name);
guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Erlang decidable function call guard test.
  pat_syntax:call_expr(Name, guard(GuardTests));
guard_test({op, _, Op, GuardTest0, GuardTest1}) ->
  % Erlang binary operator guard test.
  pat_syntax:op_expr(to_pat_op(Op), guard_test(GuardTest0), guard_test(GuardTest1));
guard_test({op, _, Op, GuardTest}) ->
  % Erlang unary operator guard test.
  pat_syntax:op_expr(to_pat_op(Op), guard_test(GuardTest)).

%% @private Translates a list of Erlang parameters.
params(PatSeq) ->
  Translate =
    fun(Pat) ->
      Type = paterl_anno:type(_Anno = element(2, Pat)),
      pat_syntax:param(pat(Pat), type(Type))
    end,
  [Translate(Pat) || Pat <- PatSeq].

%% @private Translates Erlang pattern sequences.
pat_seq(PatSeq) ->
  [pat(Pat) || Pat <- PatSeq].

%% @private Translates Erlang patterns.
pat(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Erlang literal patterns.
  pat_syntax:lit(element(3, Lit));
pat({var, _, Name}) ->
  % Erlang variable pattern.
%%  string:lowercase(atom_to_list(Name));
  pat_syntax:var(Name);
pat({tuple, _, [_Tag = {atom, _, Name} | Args]}) ->
  % Erlang message pattern.
  pat_syntax:msg_pat(Name, pat_seq(Args)).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%% @private Creates a call to a Pat function, injects a new mailbox, and frees
%% the mailbox once the function goes out of scope.
new_call_expr({call, Anno, Fun = {atom, _, _}, Args}) ->
  % Only function calls with new mailbox-annotation modality are permitted.
  ?assertEqual(new, paterl_anno:modality(Anno)),

  % Create Erlang syntax of function to be called. The created function must be
  % annotated with the use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
  ),

  % Create local mailbox IDs.
  MbNew = fresh_mb(),
  MbCall = fresh_mb(),

  % Translate function call.
  {Call, []} = expr(Expr, [], MbNew),

  % Variables used to construct call expression.
  MbVarNew = pat_syntax:var(MbNew),
  MbVarCall = pat_syntax:var(MbCall),
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

%% @private Creates a spawn of a Pat function, injects a new mailbox, and frees
%% the mailbox once the function goes out of scope.
spawn_expr({call, Anno, {atom, _, spawn}, _MFArgs = [_, Fun, Args]}) ->
  % Only function calls with new mailbox-annotation modality are permitted.
  ?assertEqual(new, paterl_anno:modality(Anno)),

  % Create Erlang syntax of function to be spawned. The created function must be
  % annotated with the use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, erl_syntax:list_elements(Args)),
      paterl_anno:set_modality(use, Anno))
  ),

  % Create local mailbox IDs.
  MbNew = fresh_mb(),
  MbCall = fresh_mb(),

  % Translate function call.
  {Call, []} = expr(Expr, [], MbNew),

  % Variables used to construct spawn expression.
  MbVarNew = pat_syntax:var(MbNew),
  MbVarCall = pat_syntax:var(MbCall),
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

%% @private Returns a fresh mailbox name.
fresh_mb() ->
  paterl_tools:fresh_var(?MB_VAR_NAME).

to_pat_op(Op) when
  Op =:= '+';
  Op =:= '-';
  Op =:= '*';
  Op =:= '/';
  Op =:= '==';
  Op =:= '<';
  Op =:= '>';
  Op =:= '>=';
  Op =:= '!' ->
  ?TRACE("Passing through pat op"),
  Op;
to_pat_op(Op) when Op =:= '=<' ->
  ?TRACE("Converting pat op"),
  '<='.