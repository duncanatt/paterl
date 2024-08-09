%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Erlang to Pat source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans_5).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% API.
-export([module/1]).
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Mailbox variable name.
-define(MB_VAR_NAME, mb).

%% Checks whether the term is a type.
-define(IS_TYPE(Type), element(1, Type) =:= type).

%% Checks whether the term is a literal type.
-define(IS_LIT_TYPE(Type), (?IS_TYPE(Type)
  andalso (element(3, Type) =:= boolean
    orelse element(3, Type) =:= integer
    orelse element(3, Type) =:= float
    orelse element(3, Type) =:= string
    orelse element(3, Type) =:= atom)
)).

%% Checks whether the term is a literal.
-define(IS_LIT(Term), (element(1, Term) =:= boolean
  orelse element(1, Term) =:= integer
  orelse element(1, Term) =:= float
  orelse element(1, Term) =:= string
  orelse element(1, Term) =:= atom
)).

%% Checks whether the Erlang type is a Pat unit equivalent.
-define(IS_UNIT_EQ_TYPE(Type), (?IS_TYPE(Type) andalso
  (element(3, Type) =:= no_return
    orelse element(3, Type) =:= any
    orelse element(3, Type) =:= none)
)).

%% Extracts the literal element from the Erlang term.
-define(GET_LIT(Term), element(3, Term)).

%% Map of externally-defined opaque Erlang functions that can be substituted for
%% the concrete data unit value that has the same type as that returned by the
%% corresponding function. This scheme makes it possible to type check files
%% that call external functions without modifying the Pat implementation.
-define(OPAQUE_FUNS, #{
  format => unit,
  uniform => integer,
  system_time => integer,
  sleep => unit
}).

%%test_type(Type) when ?IS_UNIT_EQ_TYPE(Type) ->
%%  yes;
%%test_type(_) ->
%%  no.

%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------
%% TODO: Add typespecs.

-spec module(erl_syntax:forms()) -> list().
%% @doc Translates the specified Erlang abstract syntax representation to its
%% equivalent Pat abstract syntax representation.
module(Forms) ->
%%  forms(Forms ++ [main_fun_def()]).
  forms(Forms).


%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

%% @private Translates the specified list of Erlang forms.
forms(Forms) when is_list(Forms) ->
  % Skip untranslatable forms.
  [Form0 || Form <- Forms, (Form0 = form(Form)) =/= undefined].

%% @private Translates the specified Erlang form.
form({attribute, _, module, Name}) ->
  % Erlang module attribute.
  pat_syntax:comment("Translated from " ++ atom_to_list(Name) ++ ".erl");
form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  % Pat interface module attribute with message signatures.
  ?TRACE("Translating interface '~s'.", [Name]),
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
  ?TRACE("Translating function '~s/~b'.", [Name, Arity]),
  pat_syntax:fun_def(Name, fun_clauses(Clauses));
form(_Form) ->
  % Erlang forms without Pat equivalent.
  ?TRACE("Skipping form '~s'.", [element(3, _Form)]),
  undefined.

%% @private Translates the specified Erlang type definition.
type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. The clause handles the case where a
  % mailbox interface type is just a PID. Mailbox interfaces whose type is just
  % a PID are treated as empty Pat mailbox interface definitions.
  undefined;
type(Type = {type, _, Name, _Vars = []}) when ?IS_LIT_TYPE(Type) ->
  % Erlang literal types.
  pat_syntax:lit_type(Name);
type(Type = {type, _, Name, _Vars = []}) when ?IS_UNIT_EQ_TYPE(Type) ->
  % Erlang special type translated as Pat unit type.
  pat_syntax:lit_type(unit);
type({atom, _, ok}) ->
  % Erlang atom 'ok' translated as Pat unit type.
  % TODO: Will be changed removed once Pat supports atoms.
  pat_syntax:lit_type(unit);
type({user_type, _, Name, _Vars = []}) ->
  % Erlang mailbox type. Mailbox types default to the write capability.
  pat_syntax:mb_type(Name, write);
type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Erlang message signature type.
  pat_syntax:msg_type(Name, type_seq(TypeSeq));
type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Erlang union type.
  pat_syntax:union_type(type_seq(TypeSeq)).

%% @private Translates the specified Erlang type definition sequence.
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

%% @private Generic function that translates the specified Erlang clauses in
%% a mailbox context.
clauses(Fun, Clauses, Mb) when is_function(Fun, 2), is_list(Clauses) ->
  [Fun(Clause, Mb) || Clause <- Clauses].

%% @private Translates the specified Erlang case or receive clauses.
case_clauses(Clauses, Mb) ->
  ?TRACE("(~s) Translating case/receive clauses.", [Mb]),
  clauses(fun case_clause/2, Clauses, Mb).

%% @private Translates the specified Erlang if clauses.
if_clauses(Clauses, Mb) ->
  ?TRACE("(~s) Translating if clauses.", [Mb]),
  clauses(fun if_clause/2, Clauses, Mb).

%% @private Translates the specified Erlang case or receive clause.
case_clause(_Clause = {clause, _, PatSeq = [_], _GuardSeq = [], Body}, _Mb) ->
  % Erlang unconstrained case and receive clause.
  ?TRACE("(~s) Translating case/receive clause.", [_Mb]),
  Mb = fresh_mb(),
  Expr = expr(Body, Mb),
  [MsgPat] = pat_seq(PatSeq),
  pat_syntax:receive_expr(MsgPat, pat_syntax:var(Mb), Expr).

%% @private Translates the specified Erlang if clause.
if_clause(_Clause = {clause, _, _PatSeq = [], [[GuardTest]], ExprSeq}, Mb) ->
  % Erlang constrained if clause with exactly one guard and one guard test.
  ?TRACE("(~s) Translating if clause.", [Mb]),
  {guard_test(GuardTest), expr(ExprSeq, Mb)}.

%% @private Translates the specified Erlang expression sequence into its
%% equivalent single nested Pat let expression.
expr(ExprSeq, Mb) ->
  [Expr] = expr_seq(ExprSeq, Mb),
  Expr.

%% @private Translates the specified Erlang expression sequence into its
%% equivalent Pat expression sequence.
%%
%% The returned sequence is the singleton list whose one element consists of
%% the equivalent single nested Pat expression.
expr_seq([], _) ->
  [];
expr_seq([{call, _, {atom, _, self}, _MFArgs = []} | ExprSeq], Mb) ->
  % Erlang self function call expression.
  ?TRACE("(~s) Translating self expression.", [Mb]),
  MbVar = pat_syntax:var(Mb),
  [pat_syntax:tuple([MbVar, MbVar]) | expr_seq(ExprSeq, Mb)];
expr_seq([Expr = {call, Anno, {atom, _, Name}, Args} | ExprSeq], Mb) when Name =/= spawn ->
  % Erlang implicitly-qualified local function call (i.e. function name is an
  % atom) and implicitly-qualified local mailbox-annotated function call.
  %
  % Erlang explicit function calls (i.e. function name is an expression) are
  % unsupported, as are remote function calls.
  %
  % The guard 'Name =/= spawn' is added to force passing calls to 'spawn' to the
  % catch-all clause, which in turn, translates it outside the mailbox context.
  % This makes it correspond to our translation on paper, where 'spawn' is a
  % distinguished operator in the pseudo-Erlang syntax. By contrast, Erlang
  % treats 'spawn' as a regular function call. Such a distinction would not be
  % needed in practice and removing 'Name =/= spawn' yields the same translated
  % output.
  Call0 =
    case paterl_anno_3:interface(Anno) of
      undefined ->
        % Call to function call outside mailbox context.
        ?TRACE("(~s) Translating call to ~s/~b.", [Mb, Name, length(Args)]),
        Call = expr([Expr]),
        pat_syntax:tuple([Call, pat_syntax:var(Mb)]);

      _Interface ->
        % Call to function call inside mailbox context.
        Modality = paterl_anno_3:modality(Anno),
        ?TRACE("(~s) Translating call to ~s/~b [~s, ~s].", [
          Mb, Name, length(Args), _Interface, Modality
        ]),
        case Modality of
          new ->
            % Inject new mailbox.
            Call = expr([Expr]),
            pat_syntax:tuple([Call, pat_syntax:var(Mb)]);

          use ->
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(Mb)) | Args],
            pat_syntax:call_expr(Name, args(Args0))
        end
    end,
  [Call0 | expr_seq(ExprSeq, Mb)];
expr_seq([{match, _, Pat, Expr} | ExprSeq], Mb) ->
  % Erlang match expression.
  ?TRACE("(~s) Translating match expression.", [Mb]),
  Expr0 = expr([Expr], Mb),
  Mb0 = fresh_mb(),
  Binders = pat_syntax:tuple([pat(Pat), pat_syntax:var(Mb0)]),

  % Rest of Erlang expression sequence is translated because Pat let expressions
  % induce nested evaluation context rooted at this top-level let expression.
  % This means that an Erlang expression sequence is always reduced to a
  % singleton list of Pat expressions consisting of one let.
  Body =
    case ExprSeq of
      [] ->
        % Empty expression sequence. Use binders to complete let body.
        Binders;
      ExprSeq ->
        % Non-empty expression sequence.
        expr(ExprSeq, Mb0)
    end,
  [pat_syntax:let_expr(Binders, Expr0, Body)];
expr_seq([{'if', _, [Clause0, Clause1]} | ExprSeq], Mb) ->
  % Erlang expression sequence with exactly two clauses. This constraint pair
  % corresponds to the 'if' and 'else' branches in Pat.
  ?TRACE("(~s) Translating if-else expression.", [Mb]),
  {ExprC, ExprT} = if_clause(Clause0, Mb), % If.
  {{boolean, _, true}, ExprF} = if_clause(Clause1, Mb), % Else.
  [pat_syntax:if_expr(ExprC, ExprT, ExprF) | expr_seq(ExprSeq, Mb)];
expr_seq([{'receive', Anno, Clauses} | ExprSeq], Mb) ->
  % Erlang unconstrained receive expression. Corresponds to a Pat guard
  % expression.
  State = paterl_anno_3:state(Anno),
  ?TRACE("(~s) Translating receive expression guarding on '~s'.", [Mb, State]),
  ReceiveClauses = case_clauses(Clauses, Mb),

  % Check mailbox regular expression for emptiness to determine if a Pat empty
  % expression is required.
  ReceiveClauses0 =
    case pat_regex:is_mb_empty(State) of
      true ->
        % Mailbox may be empty. Add Pat empty expression.
        MbVar = pat_syntax:var(fresh_mb()),

        % BEGIN HACK: Determine unit datum to return based on the return type of the
        % enclosing function.
        {type, _, Type, _} = paterl_anno_3:type(Anno),
        Unit = get_unit_value(Type),
        ?TRACE("(~s) HACK: Generating unit value ~p for type ~p", [Mb, Unit, Type]),

        EmptyExpr = pat_syntax:empty_expr(
          MbVar, pat_syntax:tuple([Unit, MbVar]) %TODO: This cannot be unit but must be the datatype of the return type of the function.
        ),

        % END HACK.
        [EmptyExpr | ReceiveClauses];
      false ->
        % Mailbox cannot be empty.
        ReceiveClauses
    end,

  ?TRACE("(~s) Generated guard on '~s' with ~b clause(s).", [
    Mb, State, length(ReceiveClauses0)
  ]),
  Guard = pat_syntax:guard_expr(pat_syntax:var(Mb), State, ReceiveClauses0),
  [Guard | expr_seq(ExprSeq, Mb)];
%%expr_seq([{atom, _, ok} | ExprSeq], Mb) ->
%%  [pat_syntax:tuple([pat_syntax:unit(), pat_syntax:var(Mb)]) | expr_seq(ExprSeq, Mb)];
expr_seq([Expr | ExprSeq], Mb) ->
  % Pass thru to out-of-mailbox context translation:
  %
  % 1. Erlang literals and variables.
  % 2. Erlang binary and unary operators.
  % 3. Erlang spawn expression.
%%  _T = element(3, Expr),
%%  ?TRACE("(~s) Passing thru ~p expression.", [Mb,
%%    if (is_tuple(_T)) -> element(tuple_size(_T), _T); true -> _T end
%%  ]),
  ?TRACE("(~s) Passing thru ~p expression.", [Mb, Expr]),
  Expr0 = expr([Expr]),
  [pat_syntax:tuple([Expr0, pat_syntax:var(Mb)]) | expr_seq(ExprSeq, Mb)].


%%% ----------------------------------------------------------------------------
%%% Translation on terms outside mailbox context.
%%% ----------------------------------------------------------------------------

%% @private Generic function that translates the specified Erlang clauses
%% outside a mailbox context.
clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  [Fun(Clause) || Clause <- Clauses].

%% @private Translates the specified Erlang function clauses.
fun_clauses(Clauses) ->
  clauses(fun fun_clause/1, Clauses).

%% @private Translates the specified Erlang if clauses.
if_clauses(Clauses) ->
  clauses(fun if_clause/1, Clauses).

%% @private Translates the specified Erlang function clause.
fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Erlang unconstrained function clause or unconstrained mailbox-annotated
  % function clause.

  % Translate function return type.
  RetType = type(paterl_anno_3:type(Anno)),

  % Determine whether function is mailbox-annotated.
  case paterl_anno_3:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating NON mailbox-annotated function clause */~b.", [
        length(PatSeq)
      ]),

      % Translate function parameters and body.
      pat_syntax:fun_clause(params(PatSeq), expr(Body), RetType);
    _Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating mailbox-annotated function clause */~b.", [
        length(PatSeq)
      ]),

      % Create mailbox to be injected as first parameter of the
      % mailbox-annotated function clause.
      Mb = fresh_mb(),
      MbType = pat_syntax:mb_type(paterl_anno_3:interface(Anno), read),
      Params = [
        pat_syntax:param(pat_syntax:var(Mb), MbType) |
        params(PatSeq)
      ],

      Expr = expr(Body, Mb),
      pat_syntax:fun_clause(
        Params, Expr, pat_syntax:product_type([RetType, MbType])
      )
  end.

%% @private Translates the specified Erlang if clause.
if_clause({clause, _, _PatSeq = [], [[GuardTest]], ExprSeq}) ->
  % Erlang constrained if clause with exactly one guard and one guard test.
  ?TRACE("Translating if clause."),
  {guard_test(GuardTest), expr(ExprSeq)}.

%% @private Translates the specified Erlang expression sequence into its
%% equivalent single nested Pat let expression.
expr(ExprSeq) ->
  [Expr] = expr_seq(ExprSeq),
  Expr.

%% @private Translates the specified Erlang expression sequence into its
%% equivalent Pat expression sequence.
%%
%% The returned sequence is the singleton list whose one element consists of
%% the equivalent single nested Pat expression.
expr_seq([]) ->
  [];
expr_seq([{atom, _, ok} | ExprSeq]) ->
  [pat_syntax:unit() | expr_seq(ExprSeq)];
expr_seq([Lit | ExprSeq]) when ?IS_LIT(Lit) ->
  % Erlang literal expressions.
  ?TRACE("Translating literal ~s ~p.", [element(1, Lit), ?GET_LIT(Lit)]),
  [pat_syntax:lit(?GET_LIT(Lit)) | expr_seq(ExprSeq)];
expr_seq([{var, _, Name} | ExprSeq]) ->
  % Erlang variable expression.
  ?TRACE("Translating variable '~s'.", [Name]),
  [pat_syntax:var(Name) | expr_seq(ExprSeq)];
expr_seq([{tuple, _, [_Tag = {atom, _, Name} | Args]} | ExprSeq]) ->
  % Erlang Pat message expression.
  ?TRACE("Translating message with tag '~s'.", [Name]),
  [pat_syntax:msg_expr(Name, args(Args)) | expr_seq(ExprSeq)];
expr_seq([Expr = {call, _, {atom, _, spawn}, _MFArgs = [_, _Fun, _Args]} | ExprSeq]) ->
  % Erlang spawn function call expression.
  ?TRACE("Translating call to spawn ~s/~b.", [element(3, _Fun), erl_syntax:list_length(_Args)]),
  [spawn_expr(Expr) | expr_seq(ExprSeq)];
expr_seq([Expr = {call, Anno, _Fun = {atom, _, Name}, Args} | ExprSeq]) ->
  % Erlang implicitly-qualified local function call (i.e. function name is an
  % atom). Only mailbox-annotated 'new' implicitly-qualified function calls are
  % permitted by the translation.
  %
  % Erlang explicit function calls (i.e. function name is an expression) are
  % unsupported, as are remote function calls.

  % Check whether the function is an externally-defined opaque function that can
  % be substituted for the concrete data unit value of its corresponding return
  % type. This avoids catering for all possible external Erlang functions that
  % the source code uses, replacing them by a concrete value instead.
  case get_fun_return_unit(Expr) of
    undefined ->
      % Unknown externally-defined function that is translated normally.
      case paterl_anno_3:interface(Anno) of
        undefined ->
          % Call to function outside mailbox context.
          [pat_syntax:call_expr(Name, args(Args)) | expr_seq(ExprSeq)];

        _Interface ->
          % Call to function inside mailbox context. Only the new modality is
          % permitted at this point.
          Modality = paterl_anno_3:modality(Anno),
          ?assertEqual(Modality, new),

          ?TRACE("Translating call to ~s/~b [~s, ~s].", [
            Name, length(Args), _Interface, Modality
          ]),
          [new_call_expr(Expr) | expr_seq(ExprSeq)]
      end;

    RetType ->
      % Known externally-defined function that is replaced by a concrete unit
      % value.
      [RetType | expr_seq(ExprSeq)]
  end;
expr_seq([{match, _, Pat, Expr} | ExprSeq]) ->
  % Erlang match expression.
  ?TRACE("Translating match expression."),
  Expr0 = expr([Expr]),

  % Rest of Erlang expression sequence is translated because Pat let expressions
  % induce nested evaluation context rooted at this top-level let expression.
  % This means that an Erlang expression sequence is always reduced to a
  % singleton list of Pat expressions consisting of one let.
  Binders = pat(Pat),
  Body =
    case ExprSeq of
      [] ->
        % Empty expression sequence. Use binders to complete let body.
        Binders;
      ExprSeq ->
        % Non-empty expression sequence.
        expr(ExprSeq)
    end,
  [pat_syntax:let_expr(Binders, Expr0, Body)];
expr_seq([{op, _, Op, Expr0, Expr1} | ExprSeq]) ->
  % Erlang binary operator expression.
  % TODO: Should be changed to values eventually when we have ANF.
  ?TRACE("Translating binary operator expression ~s.", [Op]),
  ExprL = expr([Expr0]),
  ExprR = expr([Expr1]),
  [pat_syntax:op_expr(to_pat_op(Op), ExprL, ExprR) | expr_seq(ExprSeq)];
expr_seq([{op, _, Op, Expr} | ExprSeq]) ->
  % Erlang unary operator expression.
  ?TRACE("Translating unary operator expression ~s.", [Op]),
  Expr0 = expr([Expr]),
  [pat_syntax:op_expr(to_pat_op(Op), Expr0) | expr_seq(ExprSeq)];
expr_seq([{'if', _, [Clause0, Clause1]} | ExprSeq]) ->
  % Erlang expression sequence with exactly two clauses. This constraint pair
  % corresponds to the 'if' and 'else' branches in Pat.
  ?TRACE("Translating if-else expression."),
  {ExprC, ExprT} = if_clause(Clause0), % If.
  {{boolean, _, true}, ExprF} = if_clause(Clause1), % Else.
  [pat_syntax:if_expr(ExprC, ExprT, ExprF) | expr_seq(ExprSeq)];
expr_seq([Expr | _]) ->
  % Erlang unsupported expressions.
  ?ERROR("Unsupported Erlang expression: ~p", [Expr]),
  throw(lists:flatten(
    io_lib:format("Unsupported Erlang expression ~s", [erl_pp:expr(Expr)])
  )).

%% @private Translates the specified Erlang argument expression sequence.
args(ExprSeq) ->
  expr_seq(ExprSeq).


%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------

%% @private Translates the specified Erlang guard sequence.
guard_seq(GuardSeq) ->
  [guard(Guard) || Guard <- GuardSeq].

%% @private Translates the specified Erlang guard, which is a sequence of guard
%% tests.
guard(GuardTests) ->
  [guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates an Erlang guard test.
guard_test(Lit) when ?IS_LIT(Lit) ->
  % Erlang literal guard tests.
  pat_syntax:lit(?GET_LIT(Lit));
guard_test({var, _, Name}) ->
  % Erlang variable guard test.
  pat_syntax:var(Name);
guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Erlang decidable guard function call guard test.
  pat_syntax:call_expr(Name, guard(GuardTests));
guard_test({op, _, Op, GuardTestL, GuardTestR}) ->
  % Erlang binary operator guard test.
  pat_syntax:op_expr(to_pat_op(Op), guard_test(GuardTestL), guard_test(GuardTestR));
guard_test({op, _, Op, GuardTest}) ->
  % Erlang unary operator guard test.
  pat_syntax:op_expr(to_pat_op(Op), guard_test(GuardTest)).

%% @private Translates the specified Erlang parameter sequence.
params(PatSeq) ->
  Translate =
    fun(Pat) ->
      Type = paterl_anno_3:type(_Anno = element(2, Pat)),
      pat_syntax:param(pat(Pat), type(Type))
    end,
  [Translate(Pat) || Pat <- PatSeq].

%% @private Translates the specified Erlang pattern sequence.
pat_seq(PatSeq) ->
  [pat(Pat) || Pat <- PatSeq].

%% @private Translates the specified Erlang pattern.
pat(Lit) when ?IS_LIT(Lit) ->
  % Erlang literal patterns.
  pat_syntax:lit(?GET_LIT(Lit));
pat({var, _, Name}) ->
  % Erlang variable pattern.
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
  ?assertEqual(new, paterl_anno_3:modality(Anno)),

  % Create Erlang syntax of function to be called. The created function must be
  % annotated with the use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args), paterl_anno_3:set_modality(use, Anno))
  ),

  % Create local mailbox IDs.
  MbNew = fresh_mb(),
  MbCall = fresh_mb(),

  % Translate function call.
  Call = expr([Expr], MbNew),

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
  Interface = paterl_anno_3:interface(Anno),
  pat_syntax:let_expr(
    MbVarNew, pat_syntax:new_expr(pat_syntax:mb_type(Interface)), LetCall
  ).

%% @private Creates a spawn of a Pat function, injects a new mailbox, and frees
%% the mailbox once the function goes out of scope.
spawn_expr({call, Anno, {atom, _, spawn}, _MFArgs = [_, Fun, Args]}) ->
  % Only function calls with new mailbox-annotation modality are permitted.
  ?assertEqual(new, paterl_anno_3:modality(Anno)),

  % Create Erlang syntax of function to be spawned. The created function must be
  % annotated with the use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, erl_syntax:list_elements(Args)),
      paterl_anno_3:set_modality(use, Anno))
  ),

  % Create local mailbox IDs.
  MbNew = fresh_mb(),
  MbCall = fresh_mb(),

  % Translate function call.
  Call = expr([Expr], MbNew),

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
    pat_syntax:new_expr(pat_syntax:mb_type(paterl_anno_3:interface(Anno))),
    LetSpawn
  ).

%% @private Creates the auxiliary main function that is added to the Pat file
%% to complete it.
%%main_fun_def() ->
%%  Type = erl_syntax:revert(
%%    erl_syntax:type_application(erl_syntax:atom(any), [])
%%  ),
%%
%%  Call =
%%    erl_syntax:set_pos(
%%      erl_syntax:application(erl_syntax:atom(main), []),
%%      paterl_anno:set_modality(new, paterl_anno:set_interface(main_mbx, erl_anno:new(0)))
%%    ),
%%
%%  Clause =
%%    erl_syntax:revert(
%%      erl_syntax:set_pos(
%%        erl_syntax:clause([], [Call]),
%%        paterl_anno:set_type(Type, erl_anno:new(0))
%%
%%      )),
%%
%%  erl_syntax:revert(
%%    erl_syntax:set_pos(
%%      erl_syntax:function(erl_syntax:atom('main\''), [Clause]),
%%      paterl_anno:set_type(Type, erl_anno:new(0))
%%    )).

%% @private Returns a fresh mailbox name.
fresh_mb() ->
  paterl_tools:fresh_var(?MB_VAR_NAME).

%% @private Returns the unit data value of the type for the specified
%% externally-defined opaque Erlang function.
get_fun_return_unit({call, _, _Fun = {atom, _, Name}, _}) ->
  case maps:find(Name, ?OPAQUE_FUNS) of
%%    {ok, boolean} ->
%%      pat_syntax:lit(true);
%%    {ok, integer} ->
%%      pat_syntax:lit(0);
%%    {ok, float} ->
%%      pat_syntax:lit(0.0);
%%    {ok, string} ->
%%      pat_syntax:lit("");
%%    {ok, atom} ->
%%      pat_syntax:lit(atom);
%%    {ok, unit} ->
%%      pat_syntax:unit();
    {ok, Type} ->
      get_unit_value(Type);
    error ->
      undefined
  end;
get_fun_return_unit(_) ->
  undefined.

%% @private Returns the unit data value of the type.
get_unit_value(boolean) ->
  pat_syntax:lit(true);
get_unit_value(integer) ->
  pat_syntax:lit(0);
get_unit_value(float) ->
  pat_syntax:lit(0.0);
get_unit_value(string) ->
  pat_syntax:lit("");
get_unit_value(atom) ->
  pat_syntax:lit(atom);
get_unit_value(unit) ->
  pat_syntax:unit();
get_unit_value(_) ->
  pat_syntax:unit().


%% @private Returns the Pat operator equivalent to the specified Erlang
%% operator.
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
  Op;
to_pat_op(Op) when Op =:= '=<' ->
  '<='.