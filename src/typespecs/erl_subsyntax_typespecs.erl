%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2023 16:48
%%%-------------------------------------------------------------------
-module(erl_subsyntax_typespecs).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([check_forms/1, format_error/1]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Checking config options.
-define(OPT_ALLOW_VAR_PAT, allow_var).
-define(OPT_ALLOW_LIT_PAT, allow_lit).
-define(OPT_ALLOW_LIST_EXPR, allow_list).
-define(OPT_BUILTIN_TYPES, builtin_types).
-define(OPT_ATOMIC_LIT, atomic_lit).

%% Error classes used to label the syntactic fragments that the Erlang-to-Pat
%% translation does not currently handle. Each class identifies general errors
%% related to the class; specific error types are further qualified by one error
%% reason listed below. Every error class corresponds to a syntactic category
%% described in the Abstract Erlang Format spec document:
%% https://www.erlang.org/doc/apps/erts/absform.html
-define(E_FORM, e_form). % Unsupported form.
-define(E_LIT, e_lit). % Unsupported atomic literal.
-define(E_PAT, e_pat). % Unsupported pattern.
-define(E_EXPR, e_expr). % Unsupported expression.
-define(E_CLAUSE, e_clause). % Unsupported case or receive clause.
-define(E_GUARD, e_guard). % Unsupported guard.
-define(E_TYPE, e_type). % Unsupported type spec.

%% Reason codes that qualify the type of syntactic error.
-define(R_VAR, var).
-define(R_LIT, lit).
-define(R_BUILTIN, builtin).
-define(R_TEST, test).
-define(R_UNTAGGED, untagged).
-define(R_EMPTY, empty).
-define(R_REMOTE, remote).
-define(R_INDIRECT, indirect).
-define(R_LIST, list).

%% Literal and built-in types supported by Pat.
-define(ATOMIC_LIT, [integer, float, string, atom]).
-define(BUILTIN_TYPES, [no_return | ?ATOMIC_LIT]).

%% Error creation macros.
-define(
pushError(Class, Node, Errors),
%%  [{element(2, Node), ?MODULE, {Class, Node}} | Errors]
  [{erl_syntax:get_pos(Node), ?MODULE, {Class, Node}} | Errors]
).
-define(
pushError(Class, Reason, Node, Errors),
%%  [{element(2, Node), ?MODULE, {Class, Reason, Node}} | Errors]
  [{erl_syntax:get_pos(Node), ?MODULE, {Class, Reason, Node}} | Errors]
).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec check_forms(erl_syntax:forms()) -> errors:errors().
check_forms(Forms) ->
  Opts = [{?OPT_ATOMIC_LIT, ?ATOMIC_LIT}, {?OPT_BUILTIN_TYPES, ?BUILTIN_TYPES}],
  check_forms(Forms, orddict:from_list(Opts), []).


%%% ----------------------------------------------------------------------------
%%% AST checking functions.
%%% ----------------------------------------------------------------------------

%% @private Check a list of forms.
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

  % Permit variable but forbid atomic literal patterns in function declaration.
  Opts0 = orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts),
  check_clause_seq(Clauses, Opts0, Errors);

%% @private Local function spec.
check_form({attribute, _, spec, {{Name, _Arity}, Types}}, Opts, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Types) ->

  % Forbid use of 'pid' built-in type in local function spec. This forces
  % function specs that use the 'pid' built-in type to employ user-defined type
  % specs as the only means of referring to 'pid' types. This makes it possible
  % to refer to the mailbox interface from a function, while at the same time,
  % allows type specs to type-check successfully under other tools like
  % Dialyzer.
  check_fun_type_seq(Types, Opts, Errors);

%% @private Type declaration.
check_form({attribute, _, type, {Name, T, Vars}}, Opts, Errors)
  when is_atom(Name), is_list(Vars) ->

  % Permit use 'pid' built-in type in type spec. This allows the syntax to
  % recover 'pid' types indirectly via user-define types and makes them
  % accessible to function specs.
  Opts0 = orddict:append(?OPT_BUILTIN_TYPES, pid, Opts),

  % Forbid type variables in type spec.
  Errors0 =
    if [] =/= Vars ->
      ?pushError(?E_TYPE, ?R_VAR, hd(Vars), Errors);
      true -> Errors
    end,
  check_type(T, Opts0, Errors0);

%% @private EOF.
check_form({eof, _}, _, Errors) ->
  Errors;

%% @private Wild attribute.
check_form({attribute, _, N, T}, _, Errors) when is_atom(N) ->

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
  ?pushError(?E_FORM, Form, Errors).


%%% ----------------------------------------------------------------------------
%%% Atomic literals (integer, float, string, atom, etc).
%%% ----------------------------------------------------------------------------

check_lit(Lit = {L, _, _}, Opts, Errors) when is_atom(L) ->

  % Check that atomic literal is supported.
  case lists:member(L, orddict:fetch(?OPT_ATOMIC_LIT, Opts)) of
    true ->
      Errors; % Atomic literal supported.
    false ->
      ?pushError(?E_LIT, Lit, Errors) % Atomic literal unsupported.
  end.


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% @private Compound pattern.
check_pat({match, _, P_1, P_2}, Opts, Errors) ->

  % Permit use of variable but forbid atomic literal patterns.
  ?assertNot(orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts)),

  Opts0 = orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts),
  Errors0 = check_pat(P_1, Opts0, Errors),
  check_pat(P_2, Opts0, Errors0);

%% @private Binary operator pattern.
check_pat({op, _, Op, P_1, P_2}, Opts, Errors) ->

  % Forbid use of variable and atomic literal patterns.
  ?assertNot(orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts)),

  Errors0 = check_pat(P_1, Opts, Errors),
  check_pat(P_2, Opts, Errors0);

%% @private Unary operator pattern.
check_pat({op, _, Op, P_0}, Opts, Errors) ->

  % Forbid use of variable or atomic literal patterns.
  ?assertNot(orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts)),

  check_pat(P_0, Opts, Errors);

%% @private Tuple pattern. Current Pat implementation only supports tagged
%% tuples without implicit pattern matching, except for the tag.
check_pat(Pat = {tuple, _, Pats}, Opts, Errors) when is_list(Pats) ->

  % Check that tuple pattern is tagged. This can be encoded as a Pat message
  % pattern. Empty or untagged tuple patterns cannot be encoded as Pat message
  % patterns.
  {Pats0, Errors0} =
    case tuple_elems(Pats) of
      tagged ->
        ?assertMatch([_ | _], Pats), % Tagged tuple.
        {tl(Pats), Errors};
      Other ->
        {Pats, ?pushError(?E_PAT, Other, Pat, Errors)} % Empty or untagged tuple.
    end,

  % Current Pat implementation does not support implicit pattern matching. Check
  % that rest of tuple elements are strictly variables.
  ?assertNot(orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts)),
  check_pat_seq(Pats0, orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts), Errors0);

%% @private Variable pattern.
check_pat(Pat = {var, _, A}, Opts, Errors) when is_atom(A) ->

  % Check that use of variable pattern is permitted.
  case orddict:is_key(?OPT_ALLOW_VAR_PAT, Opts) of
    true ->
      Errors; % Use of pattern permitted.
    false ->
      ?pushError(?E_PAT, ?R_VAR, Pat, Errors) % Use of pattern forbidden.
  end;

%% @private Atomic literal pattern.
check_pat(Pat = {L, _, _}, Opts, Errors) when is_atom(L) ->

  % Check that use of atomic literal pattern is permitted.
  Errors0 =
    case orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts) of
      true ->
        Errors; % Use of pattern permitted.
      false ->
        ?pushError(?E_PAT, ?R_LIT, Pat, Errors) % Use of pattern forbidden.
    end,
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
  ?pushError(?E_PAT, Pat, Errors).

%% @private Pattern sequence.
check_pat_seq([], _, Errors) ->
  Errors;
check_pat_seq([Pat | Pats], Opts, Errors) ->
  Errors0 = check_pat(Pat, Opts, Errors),
  check_pat_seq(Pats, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Local function call expression. Current Pat implementation only
%% supports direct (i.e., atoms) local (i.e., from same module) function calls.
check_expr(Expr = {call, _, E_0, Exprs}, Opts, Errors) when is_list(Exprs) ->

  % Check that function call is local.
  Errors0 =
    case fun_call_type(E_0) of
      local ->
        Errors; % Local call.
      Other ->
        ?pushError(?E_EXPR, Other, Expr, Errors) % Remote or indirect call.
    end,

  % Permit list expressions to be use in the specific local function calls.
  Opts0 =
    if
      element(1, E_0) =:= atom,
      element(3, E_0) =:= format;
      element(3, E_0) =:= spawn ->
        orddict:store(?OPT_ALLOW_LIST_EXPR, true, Opts);
      true ->
        Opts
    end,
  check_expr_seq(Exprs, Opts0, Errors0);

%% @private Cons skeleton.
check_expr(Expr = {cons, _, E_h, E_t}, Opts, Errors) ->

  % Check that use of list expression is permitted (see function call).
  Errors0 =
    case orddict:is_key(?OPT_ALLOW_LIST_EXPR, Opts) of
      true ->
        Errors; % Use of list permitted.
      false ->
        ?pushError(?E_EXPR, ?R_LIST, Expr, Errors) % Use of list forbidden.
    end,

  Errors1 = check_expr(E_h, Opts, Errors0),
  check_expr(E_t, Opts, Errors1);

%% @private Nil.
check_expr(Expr = {nil, _}, Opts, Errors) ->

  % Check that use of list expression is permitted (see function call).
  case orddict:is_key(?OPT_ALLOW_LIST_EXPR, Opts) of
    true ->
      Errors; % Use of list permitted.
    false ->
      ?pushError(?E_EXPR, ?R_LIST, Expr, Errors) % Use of list forbidden.
  end;

%% @private If condition expression.
check_expr(_Expr = {'if', _, Clauses}, Opts, Errors) when is_list(Clauses) ->

  % Current Pat implementation forbids variable or atomic literal patterns in
  % if clauses. Remove respective keys from options.
  Opts0 = orddict:erase(
    ?OPT_ALLOW_VAR_PAT, orddict:erase(?OPT_ALLOW_LIT_PAT, Opts)
  ),
  check_clause_seq(Clauses, Opts0, Errors);

%% @private Match operator expression.
check_expr({match, _, P, E_0}, Opts, Errors) ->

  % TODO: Could be refined to detect variable assignment (allowed) and variable
  % TODO: pattern matching (forbidden). This requires a symbol table to track.
  % Current Pat implementation does not support atomic literal pattern matching
  % except variable assignment. Permit variable but forbid atomic literal
  % patterns (LHS of match). Permit variable and atomic literal expressions on
  % RHS of match.
  ?assertNot(orddict:is_key(?OPT_ALLOW_LIT_PAT, Opts)),

  Opts0 = orddict:store(?OPT_ALLOW_VAR_PAT, true, Opts),
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

  % Current Pat implementation forbids variable or atomic literal patterns in
  % if clauses. Remove respective keys from options.
  Opts0 = orddict:erase(
    ?OPT_ALLOW_VAR_PAT, orddict:erase(?OPT_ALLOW_LIT_PAT, Opts)
  ),
  check_clause_seq(Clauses, Opts0, Errors);

%% @private Tuple expression. Current Pat implementation only supports tagged
%% tuple expressions.
check_expr(Expr = {tuple, _, Exprs}, Opts, Errors) when is_list(Exprs) ->

  % Check that tuple expression is tagged, which can be encoded as a Pat message
  % expression. Empty or untagged tuple patterns cannot be encoded as Pat
  % message patterns.
  {Exprs0, Errors0} =
    case tuple_elems(Exprs) of
      tagged ->
        ?assertMatch([_ | _], Exprs), % Tagged tuple.
        {tl(Exprs), Errors};
      Other ->
        {Exprs, ?pushError(?E_EXPR, Other, Expr, Errors)} % Empty or untagged tuple.
    end,

  % Check that rest of tuple elements are valid expressions.
  check_expr_seq(Exprs0, Opts, Errors0);

%% @private Variable expression.
check_expr({var, _, A}, _, Errors) when is_atom(A) ->
  Errors;

%% @private Atomic literal expression.
check_expr(Expr = {L, _, _}, Opts, Errors) when is_atom(L) ->
  check_lit(Expr, Opts, Errors);

%% @private Unsupported expressions:
%% - Bitstring comprehension
%% - Bitstring constructor
%% - Block
%% - Case
%% - Catch
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
  ?pushError(?E_EXPR, Expr, Errors).

%% @private Expression sequence.
check_expr_seq([], _, Errors) ->
  Errors;
check_expr_seq([Expr | Exprs], Opts, Errors) ->
  Errors0 = check_expr(Expr, Opts, Errors),
  check_expr_seq(Exprs, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Clauses.
%%% ----------------------------------------------------------------------------

%% @private Case clause.
check_clause(_Clause = {clause, _, [P], [], B}, Opts, Errors) ->
  Errors0 = check_pat(P, Opts, Errors),
  check_expr_seq(B, Opts, Errors0);

%% @private Function clause without guards.
check_clause(_Clause = {clause, _, Ps, [], B}, Opts, Errors) ->
  Errors0 = check_pat_seq(Ps, Opts, Errors),
  check_expr_seq(B, Opts, Errors0);

%% @private If clause with guard sequence.
check_clause(_Clause = {clause, _, [], Gs, B}, Opts, Errors) ->
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
  ?pushError(?E_CLAUSE, Clause, Errors).

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

%% @private Variable test.
check_test(Test = {var, _, L}, Opts, Errors) when is_atom(L) ->
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
  ?pushError(?E_GUARD, ?R_TEST, Test, Errors).

%% @private Guard as a sequence of test. Permits exactly one guard test.
check_guard([Test], Opts, Errors) ->
  check_test(Test, Opts, Errors);
check_guard([_, Test | _], _, Errors) ->
  ?pushError(?E_GUARD, Test, Errors).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% @private Tuple type. Currently only tagged tuple with first element as an
%% atom is supported. This encodes Erlang messages. The tagged tuple must have
%% at least one element, which is the message tag itself.
check_type(Type = {type, _, tuple, Types}, Opts, Errors) ->

  % Check that tuple type is tagged, which can be encoded as a Pat message type.
  % Empty or untagged tuple types cannot be encodes as Pat message types.
  {Types0, Errors0} =
    case tuple_elems(Types) of
      tagged ->
        ?assertMatch([_ | _], Types), % Tagged tuple.
        {tl(Types), Errors};
      Other ->
        {Types, ?pushError(?E_TYPE, Other, Type, Errors)} % Empty or untagged tuple.
    end,

  % Check that rest of tuple elements are valid type specs.
  check_type_seq(Types0, Opts, Errors0);

%% @private Type union.
check_type({type, _, union, Types}, Opts, Errors) when is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Built-in types.
check_type(Type = {type, _, N, Types}, Opts, Errors) when is_atom(N) ->

  % Check that use of built in type is supported in type specs.
  Errors0 =
    case lists:member(N, orddict:fetch(?OPT_BUILTIN_TYPES, Opts)) of
      true ->
        Errors; % Built-in type supported.
      false ->
        ?pushError(?E_TYPE, ?R_BUILTIN, Type, Errors) % Built-in type unsupported.
    end,
  check_type_seq(Types, Opts, Errors0);

%%%% private Type variable.
%%check_type({var, _, A}, _, Errors) when is_atom(A) ->
%%  Errors;

%% @private User-defined type.
check_type({user_type, _, N, Types}, Opts, Errors) when is_atom(N), is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Atomic literal type.
check_type(Type = {L, _, _}, Opts, Errors) when is_atom(L) ->
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
  ?pushError(?E_TYPE, Type, Errors).

%% @private Type sequence.
check_type_seq([], _, Errors) ->
  Errors;
check_type_seq([Type | Types], Opts, Errors) ->
  Errors0 = check_type(Type, Opts, Errors),
  check_type_seq(Types, Opts, Errors0).

%% @private Function type. Used only by forms of the shape 'spec'.
check_fun_type({type, _, 'fun', [{type, _, product, Types}, T_0]}, Opts, Errors)
  when is_list(Types) ->

  % Use of 'pid' type in function parameters and return values is forbidden. The
  % syntax can recover 'pid' types indirectly via user-defined types made
  % accessible to function specs.
  ?assertNot(lists:member('pid', orddict:fetch(?OPT_BUILTIN_TYPES, Opts))),

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

%% @private Infers the shape of the specified tuple.
tuple_elems([]) ->
  ?R_EMPTY;
tuple_elems([{atom, _, _} | _]) ->
  tagged;
tuple_elems(_) ->
  ?R_UNTAGGED.

%% @private Infers the structure of the specified function call.
fun_call_type({atom, _, _}) ->
  local;
fun_call_type({remote, _, _, _}) ->
  ?R_REMOTE;
fun_call_type(_) ->
  ?R_INDIRECT.


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
format_error({?E_FORM, Node}) ->
  io_lib:format("form not supported: '~s'", [erl_prettypr:format(Node)]);
format_error({?E_LIT, Node}) ->
  io_lib:format("literal '~s' not supported", [erl_prettypr:format(Node)]);
format_error({?E_PAT, Node}) ->
  io_lib:format("pattern '~s' not supported", [erl_prettypr:format(Node)]);
format_error({?E_EXPR, Node}) ->
  io_lib:format("expression '~s' not supported", [erl_prettypr:format(Node)]);
format_error({?E_CLAUSE, Node}) ->
  io_lib:format("clause '~s' not supported", [erl_prettypr:format(Node)]);
format_error({?E_TYPE, Node}) ->
  io_lib:format("type spec '~s' not supported", [erl_prettypr:format(Node)]);
format_error({?E_GUARD, Node}) ->
  io_lib:format(
    "illegal second guard test '~s' in if expression",
    [erl_prettypr:format(Node)]
  );
format_error({?E_PAT, ?R_UNTAGGED, Node}) ->
  io_lib:format(
    "illegal untagged tuple '~s' in pattern", [erl_prettypr:format(Node)]
  );
format_error({?E_PAT, ?R_EMPTY, _}) ->
  "illegal empty tuple in pattern";
format_error({?E_PAT, ?R_VAR, Node}) ->
  io_lib:format(
    "variable '~s' not supported in pattern", [erl_prettypr:format(Node)]
  );
format_error({?E_PAT, ?R_LIT, Node}) ->
  io_lib:format(
    "literal '~s' not supported in pattern", [erl_prettypr:format(Node)]
  );
format_error({?E_EXPR, ?R_UNTAGGED, Node}) ->
  io_lib:format(
    "illegal untagged tuple '~s' expression", [erl_prettypr:format(Node)]
  );
format_error({?E_EXPR, ?R_EMPTY, _}) ->
  "illegal empty tuple expression";
format_error({?E_EXPR, ?R_REMOTE, Node}) ->
  io_lib:format(
    "remote function call expression not supported: '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_EXPR, ?R_INDIRECT, Node}) ->
  io_lib:format(
    "indirect function call expression not supported: '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_EXPR, ?R_LIST, Node}) ->
  io_lib:format(
    "list expression '~s' not supported here", [erl_prettypr:format(Node)]
  );
format_error({?E_GUARD, ?R_TEST, Node}) ->
  io_lib:format(
    "illegal test '~s' not supported in if expression",
    [erl_prettypr:format(Node)]
  );
format_error({?E_TYPE, ?R_VAR, Node}) ->
  io_lib:format(
    "illegal type variable '~s' in type spec", [erl_prettypr:format(Node)]
  );
format_error({?E_TYPE, ?R_BUILTIN, Node}) ->
  io_lib:format(
    "illegal use of built-in type '~s' in function type spec",
    [erl_prettypr:format(Node)]
  );
format_error({?E_TYPE, ?R_UNTAGGED, Node}) ->
  io_lib:format(
    "illegal untagged tuple type '~s'", [erl_prettypr:format(Node)]
  );
format_error({?E_TYPE, ?R_EMPTY, _}) ->
  "illegal empty tuple type";
format_error(E) ->
  io_lib:format("unknown error ~p", [E]).
