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

%%% Public API.
-export([]).

%%% Checking config options.
-define(ALLOW_VAR, allow_var).
-define(ALLOW_LIT, allow_lit).
-define(BUILTIN_TYPES, builtin_types).

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
-define(IS_LIT_TYPE(Type), Type =:= integer; Type =:= float; Type =:= string; Type =:= atom).
-define(LIT_TYPES, [integer, float, string, atom]).


check(Forms) ->
  check_forms(Forms, []).

check_forms([], Errors) ->
  lists:reverse(Errors);
check_forms([Form | Forms], Errors) ->
  ?DEBUG("Checking form: ~p", [Form]),
  Errors0 = check_form(Form, Errors),
  check_forms(Forms, Errors0).

%% @private Export attribute.
check_form({attribute, _, export, _Sigs}, Errors) when is_list(_Sigs) ->
  Errors;

%% @private Import attribute.
check_form({attribute, _, import, {_Mod, _Funs}}, Errors)
  when is_atom(_Mod), is_list(_Funs) ->
  Errors;

%% @private Module attribute.
check_form({attribute, _, module, _Mod}, Errors) when is_atom(_Mod) ->
  Errors;

%% @private File attribute.
check_form({attribute, _, file, {_File, _Line}}, Errors)
  when is_list(_File), is_integer(_Line) ->
  Errors;

%% @private Function declaration.
check_form({function, _, Name, _Arity, Clauses}, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Clauses) ->
  Errors0 = check_clause_seq(Clauses, [?ALLOW_VAR], Errors),
  Errors0;

%% @private Local function spec.
check_form({attribute, _, spec, {{Name, _Arity}, Types}}, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Types) ->
  Errors0 = check_fun_type_seq(Types, Errors),
  Errors0;

%% @private Type declaration.
check_form({attribute, _, type, {Name, T, Vars}}, Errors)
  when is_atom(Name), is_list(Vars) ->

  % Forbid type variables.
  Errors0 =
    if [] =/= Vars ->
      ?pushErr(?E_TYPE, ?V_VAR, hd(Vars), Errors);
      true -> Errors
    end,

  % Allow only these built-in types.
  check_type(T, [{?BUILTIN_TYPES, [pid | ?LIT_TYPES]}], Errors0);


%% @private Errors and warnings.
check_form({EW, _}, Errors) when EW =:= error; EW =:= warning ->
  Errors;

%% @private EOF.
check_form({eof, _}, Errors) ->
  Errors;

%% @private Wild attribute associating mailbox type to functions.
check_form({attribute, _, Name, Sigs}, Errors)
  when is_atom(Name), is_list(Sigs) ->

  % Checks whether the specified tuple is a Fun/Arity.
  IsFa =
    fun({F, A}, Flag) when is_atom(A), is_integer(A) -> Flag and true;
      (_, _) -> false
    end,

  case lists:foldl(IsFa, true, Sigs) of
    true ->
      ?TRACE("Mailbox type ~s implemented by ~p", [Name, Sigs]),
      Errors;
    false ->
      Errors
  end;

%% @private Unsupported forms:
%% - Callback
%% - Remote function spec
%% - Record declaration
%% - Opaque type
%% - Wild attribute
check_form(Form, Errors) ->
  ?pushErr(?E_FORM, Form, Errors).


%%% ----------------------------------------------------------------------------
%%% Atomic literals (integer, float, string, atom, etc).
%%% ----------------------------------------------------------------------------

check_lit({Type, _, _}, Errors) when ?IS_LIT_TYPE(Type) ->
  Errors;
check_lit(Lit, Errors) ->
  ?pushErr(?E_LIT, Lit, Errors).


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

% Erlang case clauses forbid patterns that are variables or atomic literals.

%% @private Atomic literal pattern.
check_pat(Pat = {Type, _, _}, Opts, Errors) when ?IS_LIT_TYPE(Type) ->
  Errors0 =
    case proplists:is_defined(?ALLOW_LIT, Opts) of
      true ->

        % Literal pattern allowed.
        Errors;
      false ->

        % Literal pattern forbidden.
        ?pushErr(?E_PAT, ?V_LIT, Pat, Errors)
    end,
  check_lit(Pat, Errors0);

%% @private Compound pattern.
check_pat({match, _, P_1, P_2}, _, Errors) ->

  % Permit only pattern matching on variables but not literals.
  Errors0 = check_pat(P_1, [?ALLOW_VAR], Errors),
  check_pat(P_2, [?ALLOW_VAR], Errors0);

%% @private Binary operator pattern.
check_pat({op, _, Op, P_1, P_2}, _, Errors) ->

  % Forbid variable or atomic literal patterns.
  Errors0 = check_pat(P_1, [], Errors), %TODO: Too restrictive. Test X + Y.
  check_pat(P_2, [], Errors0);

%% @private Unary operator pattern.
check_pat({op, _, Op, P_0}, _, Errors) -> %TODO: Too restrictive. Test +Y.

  % Forbid variable or atomic literal patterns.
  check_pat(P_0, [], Errors);

%% @private Tuple pattern. Only tagged tuple patterns accepted.
check_pat(Pat = {tuple, ANNO, Pats}, Opts, Errors) when is_list(Pats) ->

  ?TRACE("Checking TUPLE pattern: ~p", [Pat]),
  Errors0 =
    case tuple_elems(Pats) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message pattern.
        Errors;
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message pattern.
        ?pushErr(?E_PAT, Other, Pat, Errors)
    end,

  % Pat does not support implicit pattern matches in messages. Check that rest
  % of tuple elements are strictly pattern variables.
  Pats0 = if [] =:= Pats -> []; true -> tl(Pats) end,
  check_pat_seq(Pats0, [?ALLOW_VAR | Opts], Errors0);

%% @private Variable pattern.
check_pat(Pat = {var, _, Name}, Opts, Errors) when is_atom(Name) ->
  ?TRACE("Checking VAR pattern ~p", [Pat]),
  ?TRACE("Opts: ~p", [Opts]),
  case proplists:is_defined(?ALLOW_VAR, Opts) of
    true ->

      % Variable pattern allowed.
      ?TRACE("VAR pattern ok"),
      Errors;
    false ->

      % Variable pattern forbidden.
      ?TRACE("VAR pattern disallowed"),
      ?pushErr(?E_PAT, ?V_VAR, Pat, Errors)
  end;

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

%% @private Atomic literal expression.
check_expr(Expr = {Name, _, _}, Errors) when ?IS_LIT_TYPE(Name) ->
  check_lit(Expr, Errors);

%% @private Local function call expression. Currently only direct (i.e., atoms)
%% local function calls are supported.
check_expr(Expr = {call, _, E_0, Exprs}, Errors) when is_list(Exprs) ->
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
  check_expr_seq(Exprs, Errors0);

%% @private If condition expression.
check_expr({'if', _, Clauses}, Errors) when is_list(Clauses) ->
  check_clause_seq(Clauses, [], Errors);

%% @private Match operator expression.
check_expr({match, _, P, E_0}, Errors) ->
  Errors0 = check_pat(P, [?ALLOW_VAR], Errors),
  check_expr(E_0, Errors0);

%% @private Binary operator expression.
check_expr({op, _, Op, E_1, E_2}, Errors) ->
  Errors0 = check_expr(E_1, Errors),
  check_expr(E_2, Errors0);

%% @private Unary operator expression.
check_expr({op, _, Op, E_0}, Errors) ->
  check_expr(E_0, Errors);

%% @private Receive expression.
check_expr(_Expr = {'receive', _, Clauses}, Errors) when is_list(Clauses) ->
  ?TRACE("Checking RECEIVE expression: ~p", [_Expr]),

  % Restricted clause sequence. Forbid variable or atomic literal patterns.
  check_clause_seq(Clauses, [], Errors);

%% @private Tuple expression. Currently only tagged tuple expressions accepted.
check_expr(Expr = {tuple, ANNO, Exprs}, Errors) when is_list(Exprs) ->
  Errors0 =
    case tuple_elems(Exprs) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message expression.
        Errors;
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message expression.
        ?pushErr(?E_EXPR, Other, Expr, Errors)
    end,

  % Check that rest of tuple elements are valid expressions.
  Exprs0 = if [] =:= Exprs -> []; true -> tl(Exprs) end,
  check_expr_seq(Exprs0, Errors0);

%% @private Variable expression.
check_expr({var, _, Name}, Errors) when is_atom(Name) ->
  Errors;

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
check_expr(Expr, Errors) ->
  ?pushErr(?E_EXPR, Expr, Errors).

%% @private Expression sequence.
check_expr_seq([], Errors) ->
  Errors;
check_expr_seq([Expr | Exprs], Errors) ->
  ?TRACE("Errors in check_expr_seq: ~p", [Errors]),
  Errors0 = check_expr(Expr, Errors),
  check_expr_seq(Exprs, Errors0).


%%% ----------------------------------------------------------------------------
%%% Clauses.
%%% ----------------------------------------------------------------------------

%% @private Case clause.
check_clause(_Clause = {clause, _, [P], [], B}, Opts, Errors) ->
  ?TRACE("Checking CASE CLAUSE ~p", [_Clause]),
  Errors0 = check_pat(P, Opts, Errors),
  check_expr_seq(B, Errors0);

%% @private Function clause without guards.
check_clause(_Clause = {clause, _, Ps, [], B}, Opts, Errors) ->
  ?TRACE("Checking FUNCTION CLAUSE ~p", [_Clause]),
  Errors0 = check_pat_seq(Ps, Opts, Errors),
  check_expr_seq(B, Errors0);

%% @private If clause with guard sequence.
check_clause({clause, _, [], Gs, B}, Opts, Errors) ->
  Errors0 = check_guard_seq(Gs, Errors),
  check_expr_seq(B, Errors0);

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
check_guard_seq([], Errors) ->
  Errors;
check_guard_seq([Guard | Guards], Errors) ->
  Errors0 = check_guard(Guard, Errors),
  check_guard_seq(Guards, Errors0).

%% @private Atomic literal test.
check_test(Test = {Name, _, _}, Errors) when ?IS_LIT_TYPE(Name) ->
  check_lit(Test, Errors);

%% @private Binary operator test.
check_test({op, _, Op, Gt_1, Gt_2}, Errors) ->
  Errors0 = check_test(Gt_1, Errors),
  check_test(Gt_2, Errors0);

%% @private Unary operator test.
check_test({op, _, Op, Gt_0}, Errors) ->
  check_test(Gt_0, Errors);

%% @private Tuple skeleton test.
%%check_test({tuple, _, Guard}, Errors) ->
%% IF we decide to support this, we need to make the tagged/empty tuple check
%%  check_guard(Guard, Errors);

%% @private Variable test.
check_test({var, _, Name}, Errors) when is_atom(Name) ->
  Errors;

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
check_test(Test, Errors) ->
  ?pushErr(?E_GUARD, ?V_TEST, Test, Errors).

%% @private Guard as a sequence of test. Allows exactly one guard test.
check_guard([Test], Errors) ->
  check_test(Test, Errors);
check_guard([_, Test | _], Errors) ->
  ?pushErr(?E_GUARD, Test, Errors).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% @private Atomic literal type.
check_type(Type = {Lit, _, _}, _, Errors) when ?IS_LIT_TYPE(Lit) ->
  ?TRACE("Checking literal type: ~p", [Type]),
  check_lit(Type, Errors);

%% @private Tuple type. Currently only tagged tuple with first element as an
%% atom is supported. This encodes Erlang messages. The tagged tuple must have
%% at least one element, which is the message tag itself.
check_type(Type = {type, _, tuple, Types}, Opts, Errors) ->
  ?TRACE("Checking tuple with types: ~p", [Types]),
  Errors0 =
    case tuple_elems(Types) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message.
        Errors;
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message.
        ?pushErr(?E_TYPE, Other, Type, Errors)
    end,

  % Check that rest of tuple elements are valid type specifications.
  Types0 = if [] =:= Types -> []; true -> tl(Types) end,
  check_type_seq(Types0, Opts, Errors0);

%% @private Type union.
check_type({type, _, union, Types}, Opts, Errors) when is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Built-in types.
check_type(Type = {type, _, N, Types}, Opts, Errors) when is_atom(N) ->

  ?TRACE("Checking built in type: ~p", [N]),
  ?TRACE("Allowed built-ins: ~p", [proplists:get_value(?BUILTIN_TYPES, Opts, [])]),
  ?TRACE("Permitted: ~p", [lists:member(N, proplists:get_value(?BUILTIN_TYPES, Opts, []))]),
  Errors0 =
    case lists:member(N, proplists:get_value(?BUILTIN_TYPES, Opts, [])) of
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
check_fun_type({type, _, 'fun', [{type, _, product, Types}, T_0]}, Errors)
  when is_list(Types) ->

  % Forbid PID parameter and return types. Only support these indirectly via
  % user defined types.
  Errors0 = check_type_seq(Types, [{?BUILTIN_TYPES, ?LIT_TYPES}], Errors),
  check_type(T_0, [{?BUILTIN_TYPES, ?LIT_TYPES}], Errors0).

%% @private Function type sequence. Used only by forms of the shape 'spec'.
check_fun_type_seq([], Errors) ->
  Errors;
check_fun_type_seq([FType | FTypes], Errors) ->
  Errors0 = check_fun_type(FType, Errors),
  check_fun_type_seq(FTypes, Errors0).












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
