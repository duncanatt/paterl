%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2023 16:48
%%%-------------------------------------------------------------------
-module(erl_subsyntax).
-author("duncan").

-compile(export_all).

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Imports.
%%-import(proplists, [get_value/3, is_defined/2]).

%%% Public API.
-export([]).

%%% Checking config options.
-define(OPT_ALLOW_VAR_PAT, allow_var).
-define(OPT_ALLOW_LIT_PAT, allow_lit).
-define(OPT_BUILTIN_TYPES, builtin_types).
-define(OPT_ATOMIC_LIT, atomic_lit).
-define(OPT_ENABLE_PATS, enable_pats).

%%% Top-level error classes. Each class corresponds to the Abstract Erlang
%%% Format specification. Error classes are used to signal which Erlang
%%% language constructs are unsupported by the Erlang-to-Pat translation.
-define(E_FORM, e_form).
-define(E_LIT, e_lit).
-define(E_PAT, e_pat).
-define(E_EXPR, e_expr).
-define(E_CLAUSE, e_clause).
-define(E_GUARD, e_guard).
-define(E_TYPE, e_type).

%%% Specific error sub-classes. or reasons
-define(V_VAR, var). % Translate to R.
-define(V_LIT, lit).
-define(V_BUILTIN, builtin).
-define(V_TEST, test).
-define(V_UNTAGGED, untagged).
-define(V_EMPTY, empty).
-define(V_REMOTE, remote).
-define(V_INDIRECT, indirect).


%% TODO: Add module name in tuple for error translation.
-define(pushErr(Class, Node, Errors), [{Class, element(2, Node), Node} | Errors]).
-define(
pushErr(Class, Reason, Node, Errors),
  [{{Class, Reason}, element(2, Node), Node} | Errors]
).

%% Literal types supported by Pat.
%%-define(IS_LIT_TYPE(Type), Type =:= integer; Type =:= string; Type =:= atom).
-define(ATOMIC_LIT, [integer, string, atom]).
-define(BUILTIN_TYPES, ?ATOMIC_LIT).



check(Forms) ->
  Opts = [{?OPT_ATOMIC_LIT, ?ATOMIC_LIT}, {?OPT_BUILTIN_TYPES, ?BUILTIN_TYPES}],
  check_forms(Forms, orddict:from_list(Opts), []).
%%  check_forms(Forms, [
%%    {?OPT_ATOMIC_LIT, ?ATOMIC_LIT},
%%    {?OPT_BUILTIN_TYPES, ?BUILTIN_TYPES}
%%  ], []).


check_forms([], _, Errors) ->
  lists:reverse(Errors);
check_forms([Form | Forms], Opts, Errors) ->
  ?DEBUG("Checking form: ~p", [Form]),
  Errors0 = check_form(Form, Opts, Errors),
  check_forms(Forms, Opts, Errors0).

%% @private Export attribute.
check_form({attribute, _, export, _Sigs}, _, Errors) when is_list(_Sigs) ->
  Errors;

%% @private Import attribute.
check_form({attribute, _, import, {_Mod, _Funs}}, _, Errors)
  when is_atom(_Mod), is_list(_Funs) ->
  Errors;

%% @private Module attribute.
check_form({attribute, _, module, _Mod}, _, Errors) when is_atom(_Mod) ->
  Errors;

%% @private File attribute.
check_form({attribute, _, file, {_File, _Line}}, _, Errors)
  when is_list(_File), is_integer(_Line) ->
  Errors;

%% @private Function declaration.
check_form({function, _, Name, _Arity, Clauses}, Opts, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Clauses) ->

  ?TRACE("Opts in function before: ~p", [Opts]),
  Opts0 = orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts),
  ?TRACE("Opts in function After: ~p", [Opts0]),

  Errors0 = check_clause_seq(Clauses, Opts0, Errors),
  Errors0;

%% @private Local function spec.
check_form({attribute, _, spec, {{Name, _Arity}, Types}}, Opts, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Types) ->
  Opts0 = orddict:append(?OPT_BUILTIN_TYPES, none, Opts),
  Errors0 = check_fun_type_seq(Types, Opts0, Errors),
  Errors0;

%% @private Type declaration.
check_form({attribute, _, type, {Name, T, Vars}}, Opts, Errors)
  when is_atom(Name), is_list(Vars) ->

  % Forbid type variables.
  Errors0 =
    if [] =/= Vars ->
      ?pushErr(?E_TYPE, ?V_VAR, hd(Vars), Errors);
      true -> Errors
    end,

  % Allow pid() builtin type only in type specs. This enables user-defined types
  % that describe mailbox interfaces to include pid() in the type union, which
  % makes these type-check successfully under other tools like Dialyzer. The
  % pid() builtin type is forbidden in function specs.
  Opts0 = orddict:append(?OPT_BUILTIN_TYPES, pid, Opts),
  Opts1 = orddict:append(?OPT_BUILTIN_TYPES, none, Opts0),
%%  ?TRACE("Opts: ~p", [Opts0]),
%%  check_type(T, [{?OPT_BUILTIN_TYPES, [pid | ?ATOMIC_LIT]}], Errors0);
  check_type(T, Opts1, Errors0);
%%  check_type(T, [{?BUILTIN_TYPES, [pid | []]}], Errors0);

%% @private EOF.
check_form({eof, _}, _, Errors) ->
  Errors;

%% @private Wild attribute.
check_form({attribute, _, N, T}, _, Errors) when is_atom(N) ->
%%  , is_list(T) ->

  % TODO: This should be moved to the type collection bit.
%%  % Checks whether the specified tuple is a Fun/Arity.
%%  IsFa =
%%    fun({F, A}, Flag) when is_atom(A), is_integer(A) -> Flag and true;
%%      (_, _) -> false
%%    end,
%%
%%  case lists:foldl(IsFa, true, Sigs) of
%%    true ->
%%      ?TRACE("Mailbox type ~s implemented by ~p", [Name, Sigs]),
%%      Errors;
%%    false ->
%%      Errors
%%  end;
  Errors;

%% @private Unsupported forms:
%% - Callback
%% - Remote function spec
%% - Record declaration
%% - Opaque type
check_form(Form, _, Errors) ->
  ?pushErr(?E_FORM, Form, Errors).


%%% ----------------------------------------------------------------------------
%%% Atomic literals (integer, float, string, atom, etc).
%%% ----------------------------------------------------------------------------

%%check_lit({Type, _, _}, Errors) when ?IS_LIT_TYPE(Type) ->
%%  Errors;
%%check_lit(Lit, Errors) ->
%%  ?pushErr(?E_LIT, Lit, Errors).

check_lit(Lit = {L, _, _}, Opts, Errors) when is_atom(L) ->
%%  case is_defined(L, get_value(?OPT_ATOMIC_LIT, Opts, [])) of
%%    true ->
%%      Errors;
%%    false ->
%%      ?pushErr(?E_LIT, Lit, Errors)
%%  end.
  ?TRACE("Checking LIT: ~p", [Lit]),
  ?TRACE("---Opts: ~p", [Opts]),
  case lists:member(L, orddict:fetch(?OPT_ATOMIC_LIT, Opts)) of
    true ->
      Errors;
    false ->
      ?pushErr(?E_LIT, Lit, Errors)
  end.


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

% Erlang case clauses forbid patterns that are variables or atomic literals.


%% @private Compound pattern.
check_pat({match, _, P_1, P_2}, Opts, Errors) ->

  % Permit variable but forbid literal patterns.
  Opts0 = orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts),
%%  Errors0 = check_pat(P_1, [?OPT_ALLOW_VAR], Errors),
  Errors0 = check_pat(P_1, Opts0, Errors),
%%  check_pat(P_2, [?OPT_ALLOW_VAR], Errors0);
  check_pat(P_2, Opts0, Errors0);

%% @private Binary operator pattern.
check_pat({op, _, Op, P_1, P_2}, Opts, Errors) ->

%%  ?TRACE("Binary operator pattern: ~p", [Opts]),
%%  Opts0 = orddict:store(?OPT_ALLOW_LIT, true, Opts),

  % Forbid variable and atomic literal patterns.
  Errors0 = check_pat(P_1, Opts, Errors),
  check_pat(P_2, Opts, Errors0);

%% @private Unary operator pattern.
check_pat({op, _, Op, P_0}, Opts, Errors) ->

  % Forbid variable or atomic literal patterns.
  check_pat(P_0, Opts, Errors);

%% @private Tuple pattern. Pat supports only tagged tuples and no implicit
%% pattern matching except for the tag.
check_pat(Pat = {tuple, _, Pats}, Opts, Errors) when is_list(Pats) ->


  ?TRACE("Checking TUPLE pattern: ~p", [Pat]),
  {Pats0, Errors0} =
    case tuple_elems(Pats) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message pattern.
        ?assertMatch([_ | _], Pats),
        {tl(Pats), Errors};
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message pattern.
        {Pats, ?pushErr(?E_PAT, Other, Pat, Errors)}
    end,

  % Pat does not support implicit pattern matching in tuples. Check that rest of
  % tuple elements are strictly variables.
  check_pat_seq(Pats0, orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts), Errors0);

%% @private Variable pattern.
check_pat(Pat = {var, _, Name}, Opts, Errors) when is_atom(Name) ->
  ?TRACE("Checking VAR pattern ~p", [Pat]),
  ?TRACE("Opts: ~p", [Opts]),
%%  case proplists:is_defined(?OPT_ALLOW_VAR_PAT, Opts) of
  case orddict:is_key(?OPT_ALLOW_VAR_PAT, Opts) of
    true ->

      % Variable pattern allowed.
      ?TRACE("VAR pattern ok"),
      Errors;
    false ->

      % Variable pattern forbidden.
      ?TRACE("VAR pattern disallowed"),
      ?pushErr(?E_PAT, ?V_VAR, Pat, Errors)
  end;

%% @private Atomic literal pattern.
check_pat(Pat = {L, _, _}, Opts, Errors) when is_atom(L) ->
%%  when ?IS_LIT_TYPE(Type) ->
  ?TRACE("Checking literal pat: ~p", [Pat]),
  Errors0 =
%%    case proplists:is_defined(?OPT_ALLOW_LIT_PAT, Opts) of
  case orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts) of
    true ->

      % Literal pattern allowed.
      Errors;
    false ->

      ?TRACE("Literal pattern forbidden."),
      % Literal pattern forbidden.
      ?pushErr(?E_PAT, ?V_LIT, Pat, Errors)
  end,
  ?TRACE("AFter checking literal pattern"),
%%    Errors0;
  check_lit(Pat, Opts, Errors0);

%% @private Unsupported patterns:
%% - Bitstring
%% - Cons
%% - Map
%% - Nil
%% - Record field index
%% - Record
%% - Tuple
%% - Universal (i.e., _)
check_pat(Pat, _, Errors) ->
  ?pushErr(?E_PAT, Pat, Errors).

%% @private Pattern sequence.
check_pat_seq([], _, Errors) ->
  Errors;
check_pat_seq([Pat | Pats], Opts, Errors) ->
  Errors0 = check_pat(Pat, Opts, Errors),
  check_pat_seq(Pats, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Local function call expression. Currently only direct (i.e., atoms)
%% local function calls are supported.
check_expr(Expr = {call, _, E_0, Exprs}, Opts, Errors) when is_list(Exprs) ->
  Errors0 =
    case fun_call_type(E_0) of
      local ->

        % Local function call. Supported by Pat.
        Errors;
      Other ->

        % Remote or indirect function call. Not supported by Pat.
        ?pushErr(?E_EXPR, Other, Expr, Errors)
    end,
  ?INFO("-- Function call ~p", [Expr]),
  check_expr_seq(Exprs, Opts, Errors0);

%% @private If condition expression.
check_expr(_Expr = {'if', _, Clauses}, Opts, Errors) when is_list(Clauses) ->
  ?TRACE("Checking IF expression: ~p", [_Expr]),

  Opts0 = orddict:erase(?OPT_ALLOW_VAR_PAT, orddict:erase(?OPT_ALLOW_LIT_PAT, Opts)),

  % Restricted clause sequence. Forbid variable or atomic literal patterns.
  check_clause_seq(Clauses, Opts0, Errors);

%% @private Match operator expression.
check_expr({match, _, P, E_0}, Opts, Errors) ->

  Opts0 = orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts),
%%  Errors0 = check_pat(P, [?OPT_ALLOW_VAR], Errors),
  Errors0 = check_pat(P, Opts0, Errors),
  check_expr(E_0, Opts, Errors0);

%% @private Binary operator expression.
check_expr({op, _, Op, E_1, E_2}, Opts, Errors) ->
  Errors0 = check_expr(E_1, Opts, Errors),
  check_expr(E_2, Opts, Errors0);

%% @private Unary operator expression.
check_expr({op, _, Op, E_0}, Opts, Errors) ->
  check_expr(E_0, Opts, Errors);

%% @private Receive expression.
check_expr(_Expr = {'receive', _, Clauses}, Opts, Errors) when is_list(Clauses) ->
  ?TRACE("Checking RECEIVE expression: ~p", [_Expr]),

  Opts0 = orddict:erase(?OPT_ALLOW_VAR_PAT, orddict:erase(?OPT_ALLOW_LIT_PAT, Opts)),

  % Restricted clause sequence. Forbid variable or atomic literal patterns.
  check_clause_seq(Clauses, Opts0, Errors);

%% @private Tuple expression. Currently only tagged tuple expressions accepted.
check_expr(Expr = {tuple, _, Exprs}, Opts, Errors) when is_list(Exprs) ->
  {Exprs0, Errors0} =
    case tuple_elems(Exprs) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message expression.
        ?assertMatch([_ | _], Exprs),
        {tl(Exprs), Errors};
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message expression.
        {Exprs, ?pushErr(?E_EXPR, Other, Expr, Errors)}
    end,

  % Check that rest of tuple elements are valid expressions.
  check_expr_seq(Exprs0, Opts, Errors0);

%% @private Variable expression.
check_expr({var, _, A}, _, Errors) when is_atom(A) ->
  Errors;

%% @private Atomic literal expression.
check_expr(Expr = {L, _, _}, Opts, Errors) when is_atom(L) ->
%%  when ?IS_LIT_TYPE(Name) ->
  check_lit(Expr, Opts, Errors);

%% @private Unsupported expressions:
%% - Bitstring comprehension
%% - Bitstring constructor
%% - Block
%% - Case
%% - Catch
%% - Cons skeleton
%% - Local fun reference
%% - Remote fun reference
%% - Anonymous fun
%% - Named fun
%% - Remote function call
%% - List comprehension
%% - Map creation
%% - Map update
%% - Conditional match operator
%% - Maybe
%% - Maybe with else
%% - Nil
%% - Receive-after
%% - Record creation
%% - Record field access
%% - Record field index
%% - Record update
%% - Tuple skeleton (non-tagged)
%% - Try-catch
%% - Try-of-catch
%% - Try-after
%% - Try-of-after
%% - Try-catch-after
%% - Try-of-catch-after
check_expr(Expr, _, Errors) ->
  ?pushErr(?E_EXPR, Expr, Errors).

%% @private Expression sequence.
check_expr_seq([], _, Errors) ->
  Errors;
check_expr_seq([Expr | Exprs], Opts, Errors) ->
%%  ?TRACE("Errors in check_expr_seq: ~p", [Errors]),
  Errors0 = check_expr(Expr, Opts, Errors),
  check_expr_seq(Exprs, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Clauses.
%%% ----------------------------------------------------------------------------

%% @private Case clause.
check_clause(_Clause = {clause, _, [P], [], B}, Opts, Errors) ->
  ?TRACE("Checking CASE CLAUSE ~p", [_Clause]),
  Errors0 = check_pat(P, Opts, Errors),
  ?TRACE("After checking pattern in CASE CLASE ~p", [P]),
  check_expr_seq(B, Opts, Errors0);

%% @private Function clause without guards.
check_clause(_Clause = {clause, _, Ps, [], B}, Opts, Errors) ->
  ?TRACE("Checking FUNCTION CLAUSE ~p", [_Clause]),
  Errors0 = check_pat_seq(Ps, Opts, Errors),
  check_expr_seq(B, Opts, Errors0);

%% @private If clause with guard sequence.
check_clause(_Clause = {clause, _, [], Gs, B}, Opts, Errors) ->
  ?TRACE("Checking IF CLAUSE ~p", [_Clause]),
  Errors0 = check_guard_seq(Gs, Opts, Errors),
  check_expr_seq(B, Opts, Errors0);

%% @private Unsupported expressions:
%% - Case clause with guard sequence
%% - Catch clause with explicit throw
%% - Catch clause with pattern
%% - Catch clause with pattern and variable
%% - Catch clause with guard sequence
%% - Catch clause with pattern and guard sequence
%% - Catch clause with pattern, variable, and guard sequence
%% - Function clause with guard sequence
check_clause(Clause, _, Errors) ->
  ?pushErr(?E_CLAUSE, Clause, Errors).

%% @private Clause sequence.
check_clause_seq([], _, Errors) ->
  Errors;
check_clause_seq([Clause | Clauses], Opts, Errors) ->
  Errors0 = check_clause(Clause, Opts, Errors),
  check_clause_seq(Clauses, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Guards. Currently limited only to if conditions. Will be removed in future
%%% to consolidate to case expressions with a single guard.
%%% ----------------------------------------------------------------------------

%% @private Guard sequence.
check_guard_seq([], _, Errors) ->
  Errors;
check_guard_seq([Guard | Guards], Opts, Errors) ->
  Errors0 = check_guard(Guard, Opts, Errors),
  check_guard_seq(Guards, Opts, Errors0).

%% @private Binary operator test.
check_test({op, _, Op, Gt_1, Gt_2}, Opts, Errors) ->
  Errors0 = check_test(Gt_1, Opts, Errors),
  check_test(Gt_2, Opts, Errors0);

%% @private Unary operator test.
check_test({op, _, Op, Gt_0}, Opts, Errors) ->
  check_test(Gt_0, Opts, Errors);

%% @private Tuple skeleton test.
%%check_test({tuple, _, Guard}, Errors) ->
%% IF we decide to support this, we need to make the tagged/empty tuple check
%%  check_guard(Guard, Errors);

%% @private Variable test.
check_test(Test = {var, _, Name}, Opts, Errors) when is_atom(Name) ->
  % TODO: Var must not be supported.
  Errors;

%%  case orddict:is_key(?OPT_ALLOW_VAR_PAT, Opts) of
%%    true ->
%%
%%      % Variable pattern allowed.
%%      ?TRACE("VAR TEST pattern ok"),
%%      Errors;
%%    false ->
%%
%%      % Variable pattern forbidden.
%%      ?TRACE("VAR TEST pattern disallowed"),
%%      ?pushErr(?E_PAT, ?V_VAR, Test, Errors)
%%  end;

%% @private Atomic literal test.
check_test(Test = {L, _, _}, Opts, Errors) when is_atom(L) ->
%%  when ?IS_LIT_TYPE(Name) ->
  % TODO: The only literal that can be supported is the atom true
  check_lit(Test, Opts, Errors);

%% @private Unsupported guard tests:
%% - Bitstring constructor
%% - Cons skeleton
%% - Local function call
%% - Remote function call
%% - Map creation
%% - Map update
%% - Nil
%% - Binary operator
%% - Unary operator
%% - Record creation
%% - Record field access
%% - Record field index
%% - Tuple skeleton
%% - Variable
check_test(Test, _, Errors) ->
  ?pushErr(?E_GUARD, ?V_TEST, Test, Errors).

%% @private Guard as a sequence of test. Allows exactly one guard test.
check_guard([Test], Opts, Errors) ->
  check_test(Test, Opts, Errors);
check_guard([_, Test | _], _, Errors) ->
  ?pushErr(?E_GUARD, Test, Errors).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% @private Tuple type. Currently only tagged tuple with first element as an
%% atom is supported. This encodes Erlang messages. The tagged tuple must have
%% at least one element, which is the message tag itself.
check_type(Type = {type, _, tuple, Types}, Opts, Errors) ->
  {Types0, Errors0} =
    case tuple_elems(Types) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message. Now check that rest of
        % tuple elements are also valid type specifications.
        ?assertMatch([_ | _], Types),
        {tl(Types), Errors};

      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message. Now check
        % that all tuple elements are also valid type specifications.
        {Types, ?pushErr(?E_TYPE, Other, Type, Errors)}
    end,

  % Check that rest of tuple elements are valid type specs.
  check_type_seq(Types0, Opts, Errors0);

%% @private Type union.
check_type({type, _, union, Types}, Opts, Errors) when is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Built-in types.
check_type(Type = {type, _, N, Types}, Opts, Errors) when is_atom(N) ->

%%  ?TRACE("Checking built in type: ~p", [N]),
%%  ?TRACE("Allowed built-ins: ~p", [proplists:get_value(?OPT_BUILTIN_TYPES, Opts, [])]),
%%  ?TRACE("Permitted: ~p", [lists:member(N, proplists:get_value(?OPT_BUILTIN_TYPES, Opts, []))]),
  Errors0 =
%%    case lists:member(N, proplists:get_value(?BUILTIN_TYPES, Opts, [])) of
%%  case proplists:is_defined(N, proplists:get_value(?OPT_BUILTIN_TYPES, Opts, [])) of
  case lists:member(N, orddict:fetch(?OPT_BUILTIN_TYPES, Opts)) of
    true ->
      Errors;
    false ->
      ?pushErr(?E_TYPE, ?V_BUILTIN, Type, Errors)
  end,

  check_type_seq(Types, Opts, Errors0);

%% @private Type variable.
check_type({var, _, Name}, _, Errors) when is_atom(Name) ->
  Errors;

%% @private User-defined type.
check_type({user_type, _, N, Types}, Opts, Errors) when is_atom(N), is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Atomic literal type.
check_type(Type = {L, _, _}, Opts, Errors) when is_atom(L) ->
%%  when ?IS_LIT_TYPE(Lit) ->
  ?TRACE("Checking literal type: ~p", [Type]),
  check_lit(Type, Opts, Errors);

%% @private Unsupported types:
%% - Annotated
%% - Bitstring
%% - Nil
%% - Fun
%% - Specific fun
%% - Fun with function type
%% - Integer range
%% - Map
%% - Specific map
%% - Binary operator
%% - Unary operator
%% - Built-in with parameters
%% - Record
%% - Remote function
%% - Tuple
%% - Specific tuple
%% - Constrained function
%% - Function constraints
%% - Map field association
%% - Map field assignment
%% - Record field
check_type(Type, _, Errors) ->
  ?pushErr(?E_TYPE, Type, Errors).

%% @private Type sequence.
check_type_seq([], _, Errors) ->
  Errors;
check_type_seq([Type | Types], Opts, Errors) ->
  Errors0 = check_type(Type, Opts, Errors),
  check_type_seq(Types, Opts, Errors0).

%% @private Function type. Used only by forms of the shape 'spec'.
check_fun_type({type, _, 'fun', [{type, _, product, Types}, T_0]}, Opts, Errors)
  when is_list(Types) ->

  % TODO: Here we should forbid basic types since we did not add the builtin types.
  % Forbid PID parameter and return types. Only support these indirectly via
  % user defined types.
  Errors0 = check_type_seq(Types, Opts, Errors),
  check_type(T_0, Opts, Errors0).

%% @private Function type sequence. Used only by forms of the shape 'spec'.
check_fun_type_seq([], _, Errors) ->
  Errors;
check_fun_type_seq([FType | FTypes], Opts, Errors) ->
  Errors0 = check_fun_type(FType, Opts, Errors),
  check_fun_type_seq(FTypes, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Helper functions.
%%% ----------------------------------------------------------------------------

tuple_elems([]) ->
  ?V_EMPTY;
tuple_elems([{atom, _, _} | _]) ->
  tagged;
tuple_elems(_) ->
  ?V_UNTAGGED.


fun_call_type({atom, _, _}) ->
  local;
fun_call_type({remote, _, _, _}) ->
  ?V_REMOTE;
fun_call_type(_) ->
  ?V_INDIRECT.


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

format_err({Class, ANNO, Node}) ->
  Msg0 =
    case err_msg(Class) of
      {Msg, Text} ->
        [Msg, " not supported ", Text, $\s];
      Msg ->
        [Msg, " not supported "]
    end,
  [$:, integer_to_list(ANNO), ": Error: ", Msg0, $', erl_prettypr:format(Node), $'].


err_msg(?E_FORM) ->
  "form";
err_msg(?E_LIT) ->
  "literal";
err_msg({?E_PAT, ?V_UNTAGGED}) ->
  {"untagged tuple", "in pattern"};
err_msg({?E_PAT, ?V_EMPTY}) ->
  {"empty tuple", "in pattern"};
err_msg({?E_PAT, ?V_VAR}) ->
  {"variable", "in pattern"};
err_msg({?E_PAT, ?V_LIT}) ->
  {"literal", "in pattern"};
err_msg(?E_PAT) ->
  "pattern";
err_msg({?E_EXPR, ?V_UNTAGGED}) ->
  "untagged tuple expression";
err_msg({?E_EXPR, ?V_EMPTY}) ->
  "empty tuple expression";
err_msg({?E_EXPR, ?V_REMOTE}) ->
  "remote function call expression";
err_msg({?E_EXPR, ?V_INDIRECT}) ->
  "indirect function call expression";
err_msg(?E_EXPR) ->
  "expression";
err_msg(?E_CLAUSE) ->
  "clause";
err_msg({?E_GUARD, ?V_TEST}) ->
  "guard test";
err_msg(?E_GUARD) ->
  "second guard test";
err_msg({?E_TYPE, ?V_VAR}) ->
  "type variable";
err_msg({?E_TYPE, ?V_BUILTIN}) ->
  {"built-in type", "here"};
err_msg({?E_TYPE, ?V_UNTAGGED}) ->
  "untagged tuple type";
err_msg({?E_TYPE, ?V_EMPTY}) ->
  "empty tuple type";
err_msg(?E_TYPE) ->
  "type";
err_msg(_) ->
  "unknown error".
