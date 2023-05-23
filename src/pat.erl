%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2023 12:29
%%%-------------------------------------------------------------------
-module(pat).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").


%% API
-export([]).

-compile(export_all).

%% Literal types supported by Pat.
-define(IS_LIT_TYPE(Type), Type =:= integer; Type =:= float; Type =:= string; Type =:= atom).


-define(ALLOW_VAR, allow_var).
-define(ALLOW_LIT, allow_lit).

-define(LIT_TYPES, [integer, float, string, atom]).


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

%%% Specific error sub-classes.
-define(V_VAR, var).
-define(V_LIT, lit).
-define(V_BUILTIN, builtin).
-define(V_TEST, test).
-define(V_UNTAGGED, untagged).
-define(V_EMPTY, empty).
-define(V_REMOTE, remote).
-define(V_INDIRECT, indirect).

%% TODO: Macro handling errors.

file(File) ->
  case epp:parse_file(File, []) of
    {ok, Forms} ->
      {TSpecs, Errors} = check_forms(Forms, #{}, []),
      io:format("Errors: ~p~n~n", [Errors]),

      % Extract relative file name for error reporting.
      FName = lists:last(filename:split(File)),
      IoList = [[FName, pat:format_err(Error), $\n] || Error <- Errors],
      file:write(standard_error, IoList);
    {error, OpenError} ->
      OpenError
  end.


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
  {"built-in type", "directly"};
err_msg(?E_TYPE) ->
  "type";
err_msg(_) ->
  "unknown error".


%%% ----------------------------------------------------------------------------
%%% Module declaration and forms.
%%% ----------------------------------------------------------------------------

check_forms([], TSpec, Errors) ->
  {TSpec, lists:reverse(Errors)};
check_forms([Form | Forms], TSpec, Errors) ->
  ?DEBUG("Checking form: ~p", [Form]),
  {TSpec0, Errors0} = check_form(Form, TSpec, Errors),
  check_forms(Forms, TSpec0, Errors0).

%% @private Export attribute.
check_form({attribute, _, export, _Sigs}, TSpecs, Errors) when is_list(_Sigs) ->
  {TSpecs, Errors};

%% @private Import attribute.
check_form({attribute, _, import, {_Mod, _Funs}}, TSpecs, Errors)
  when is_atom(_Mod), is_list(_Funs) ->
  {TSpecs, Errors};

%% @private Module attribute.
check_form({attribute, _, module, _Mod}, TSpecs, Errors) when is_atom(_Mod) ->
  {TSpecs, Errors};

%% @private File attribute.
check_form({attribute, _, file, {_File, _Line}}, TSpecs, Errors)
  when is_list(_File), is_integer(_Line) ->
  {TSpecs, Errors};

%% @private Function declaration.
check_form({function, _, Name, _Arity, Clauses}, TSpecs, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Clauses) ->
  Errors0 = check_clause_seq(Clauses, [?ALLOW_VAR], Errors),
  {TSpecs, Errors0};

%% @private Local function spec.
check_form({attribute, _, spec, {{Name, _Arity}, Types}}, TSpecs, Errors)
  when is_atom(Name), is_integer(_Arity), is_list(Types) ->
  Errors0 = check_fun_type_seq(Types, Errors),
  {TSpecs, Errors0};

%% @private Type declaration.
check_form({attribute, ANNO, type, {Name, T, Vars}}, TSpecs, Errors)
  when is_atom(Name), is_list(Vars) ->
%%  io:format("Type name: ~p~n", [Name]),
%%  io:format("Type: ~p~n", [T]),
%%  io:format("Type vars ~p~n", [Vars]),

  % Do not allow type variables.
  Errors0 =
    if [] =/= Vars ->
      [{{?E_TYPE, ?V_VAR}, ANNO, hd(Vars)} | Errors];
      true -> Errors
    end,

  % Allow only these built-in types.
  {TSpecs, check_type(T, [{type, [pid | ?LIT_TYPES]}], Errors0)};


%% @private Errors and warnings.
check_form({EW, _}, TSpecs, Errors) when EW =:= error; EW =:= warning ->
  {TSpecs, Errors};

%% @private EOF.
check_form({eof, _}, TSpecs, Errors) ->
  {TSpecs, Errors};

%% @private Wild attribute associating mailbox type to functions.
check_form({attribute, _, Name, Sigs}, TSpecs, Errors)
  when is_atom(Name), is_list(Sigs) ->

  % Checks whether the specified tuple is a Fun/Arity.
  IsFa =
    fun({F, A}, Flag) when is_atom(A), is_integer(A) -> Flag and true;
      (_, _) -> false
    end,

  case lists:foldl(IsFa, true, Sigs) of
    true ->
      ?TRACE("Mailbox type ~s implemented by ~p", [Name, Sigs]),
      {TSpecs, Errors};
    false ->
      {TSpecs, Errors}
  end;

%% @private Unsupported forms:
%% - Callback
%% - Remote function spec
%% - Record declaration
%% - Opaque type
%% - Wild attribute
check_form(Form, TSpecs, Errors) ->
  {TSpecs, [{?E_FORM, element(2, Form), Form} | Errors]}.


%%% ----------------------------------------------------------------------------
%%% Atomic literals.
%%% ----------------------------------------------------------------------------

check_lit({Type, _, _}, Errors) when ?IS_LIT_TYPE(Type) ->
  Errors;
check_lit(Lit, Errors) ->
  [{?E_LIT, element(2, Lit), Lit} | Errors].


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

% For case clauses disallow patterns that are just variables or atoms. This applies only to case clauses.

% Opts
%%{allow_vars}
%%{allow_lit}
%%  TODO: Complex code to allow/disallow vars pats.

%% @private Atomic literal pattern.
check_pat(Pat = {Type, ANNO, _}, Opts, Errors) when ?IS_LIT_TYPE(Type) ->
  Errors0 =
    case proplists:is_defined(?ALLOW_LIT, Opts) of
      true -> Errors;
      false -> [{{?E_PAT, ?V_LIT}, ANNO, Pat} | Errors]
    end,

%%  Errors0 =
%%    if AllowPrimPat -> Errors;
%%      true -> [{{pat, lit}, ANNO, Pat} | Errors]
%%    end,
  check_lit(Pat, Errors0);


% I need to allow variables but not literals. So might have to use option lists.

%% @private Compound pattern.
check_pat({match, _, P_1, P_2}, _, Errors) ->
  Errors0 = check_pat(P_1, [?ALLOW_VAR], Errors),
%%  check_pat(P_2, [?ALLOW_VAR, ?ALLOW_LIT], Errors0);
  check_pat(P_2, [?ALLOW_VAR], Errors0);

%% @private Binary operator pattern.
check_pat({op, _, Op, P_1, P_2}, _, Errors) ->
  Errors0 = check_pat(P_1, [], Errors),
  check_pat(P_2, [], Errors0);

%% @private Unary operator pattern.
check_pat({op, _, Op, P_0}, _, Errors) ->
  check_pat(P_0, [], Errors);

%% @private Tuple pattern. Currently only tagged tuple patterns accepted.
check_pat(Pat = {tuple, ANNO, Pats}, Opts, Errors) when is_list(Pats) ->
% But this must be made specific with the first element as an atom.

  ?TRACE("Checking TUPLE pattern: ~p", [Pat]),
%%  {Pats0, Errors0} =
%%    case check_tuple(Pat) of
%%      ok ->
%%        % Tagged tuple. Check that rest of elements are variables only.
%%        ?TRACE("TUPLE pattern valid"),
%%        {tl(Pats), Errors}; % Pass on the rest of the tuple elements
%%      ?EMPTY_TUPLE ->
%%        % Empty tuple.
%%        ?TRACE("TUPLE pattern empty"),
%%        {Pats, [{{?PAT, ?EMPTY_TUPLE}, ANNO, Pat} | Errors]};
%%      Error ->
%%        % Untagged tuple. Check that rest of elements are variables only.
%%        ?TRACE("TUPLE pattern does not have a tag"),
%%        {tl(Pats), [{{?PAT, Error}, ANNO, Pat} | Errors]}
%%    end,

  Errors0 =
    case tuple_elems(Pats) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message pattern.
        Errors;
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message pattern.
        [{{?E_EXPR, Other}, ANNO, Pat} | Errors]
    end,

  % Pat does not support implicit pattern matches in messages. Check that rest
  % of tuple elements are strictly pattern variables.
  Pats0 = if [] =:= Pats -> []; true -> tl(Pats) end,
  check_pat_seq(Pats0, [?ALLOW_VAR | Opts], Errors0); %TODO: See whether these ALLOW** flags can be improved.

%% @private Variable pattern.
check_pat(Pat = {var, ANNO, Name}, Opts, Errors) when is_atom(Name) ->
  ?TRACE("Checking VAR pattern ~p", [Pat]),
  ?TRACE("Opts: ~p", [Opts]),
  case proplists:is_defined(?ALLOW_VAR, Opts) of
    true ->
      ?TRACE("VAR pattern ok"),
      Errors;
    false ->
      ?TRACE("VAR pattern disallowed"),
      [{{?E_PAT, ?V_VAR}, ANNO, Pat} | Errors]
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
  [{?E_PAT, element(2, Pat), Pat} | Errors].

%% @private Pattern sequence.
check_pat_seq([], _, Errors) ->
  Errors;
check_pat_seq([Pat | Pats], Opts, Errors) ->
  Errors0 = check_pat(Pat, Opts, Errors),
  check_pat_seq(Pats, Opts, Errors0).
%%  [check_pat(Pat, Errors) | check_pat_seq(Pats, Errors)].


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Atomic literal expression.
check_expr(Expr = {Name, _, _}, Errors) when ?IS_LIT_TYPE(Name) ->
  check_lit(Expr, Errors);

%% @private Local function call expression. Currently only direct (i.e., atoms)
%% local function calls are supported.
%% The tuple check ensures that a
%% remote call is not supported.
check_expr(Expr = {call, ANNO, E_0, Exprs}, Errors) when is_list(Exprs) ->
%%  when not is_tuple(E_0), is_list(Exprs) ->

%%  Errors0 =
%%    case check_fun_call(Expr) of
%%      ok ->
%%%%        check_expr(E_0, Errors),
%%        Errors;
%%      Error ->
%%        [{{?EXPR, Error}, ANNO, Expr} | Errors]
%%    end,

  Errors0 =
    case fun_call_type(E_0) of
      local ->

        % Local function call.
        Errors;
      Other ->

        % Remote or indirect function call not supported by Pat.
        [{{?E_EXPR, Other}, ANNO, Expr} | Errors]
    end,
  ?INFO("-- Function call ~p", [Expr]),
%%  Errors0 = check_expr(E_0, Errors), % Evaluates to a function name or a remote call.
  check_expr_seq(Exprs, Errors0);

%% @private If condition expression.
check_expr({'if', _, Clauses}, Errors) when is_list(Clauses) ->
  check_clause_seq(Clauses, [], Errors);
%%If E is an if expression if Ic_1 ; ... ; Ic_k end, where each Ic_i is an if
%% clause, then Rep(E) = {'if',ANNO,[Rep(Ic_1), ..., Rep(Ic_k)]}.

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

  % Special clause seq. Variable or literal clause patterns are not allowed.
  check_clause_seq(Clauses, [], Errors);

%% @private Tuple expression. Currently only tagged tuple expressions accepted.
check_expr(Expr = {tuple, ANNO, Exprs}, Errors) when is_list(Exprs) ->
%%  Errors0 =
%%    case check_tuple(Expr) of
%%      ok ->
%%        Errors;
%%      Error ->
%%        [{{?EXPR, Error}, ANNO, Expr} | Errors]
%%    end,
  Errors0 =
    case tuple_elems(Exprs) of
      tagged ->

        % Tagged tuple. Can be encoded as a Pat message expression.
        Errors;
      Other ->

        % Empty or untagged tuple. Cannot be encoded as a Pat message expression.
        [{{?E_EXPR, Other}, ANNO, Expr} | Errors]
    end, %TODO: Check whether we also need to check that the other elements are
  %  only variables. Perform a simple test to see that we cannot have hardcoded atoms for instance.

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
  [{?E_EXPR, element(2, Expr), Expr} | Errors].

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
  Errors0 = check_pat(P, Opts, Errors), % Here I need to not allow variables and literals.
  check_expr_seq(B, Errors0);

%% @private Function clause without guards.
check_clause(_Clause = {clause, _, Ps, [], B}, Opts, Errors) ->
  ?TRACE("Checking FUNCTION CLAUSE ~p", [_Clause]),
  Errors0 = check_pat_seq(Ps, Opts, Errors), % Even here I need to allow variables but not literals.
  check_expr_seq(B, Errors0);


%%% TODO: Consider removing this since Pat does not support Erlang-style guards.
%%%% @private Case clause with guard sequence.
%%check_clause({clause, _, [P], Gs, B}, Errors) ->
%%  Errors0 = check_pat(P, Errors),
%%  Errors1 = check_guard_seq(Gs, Errors0),
%%  check_expr_seq(B, Errors1);
%%

%% @private If clause.
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
  [{?E_CLAUSE, element(2, Clause), Clause} | Errors].

%% @private Clause sequence.
check_clause_seq([], _, Errors) ->
  Errors;
check_clause_seq([Clause | Clauses], Opts, Errors) ->
  Errors0 = check_clause(Clause, Opts, Errors),
  check_clause_seq(Clauses, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Guards (required only for if conditions). In the future remove them and
%%% consolidate them to cases with a single guard.
%%% ----------------------------------------------------------------------------

check_guard_seq([], Errors) ->
  Errors;
check_guard_seq([Guard | Guards], Errors) ->
  Errors0 = check_guard(Guard, Errors),
  check_guard_seq(Guards, Errors0).

%% @private Atomic literal test. We support only strings to test the commutative
%% regexp with when. If this is not correspond to the Pat syntax we want, then
%% we'll remove this and not support any form of guard tests.
%%check_test(Test = {Name, _, _}, Errors)
%%  when
%%%%  Name =:= atom; Name =:= char; Name =:= float; Name =:= integer;
%%  Name =:= string ->
%%  check_lit(Test, Errors);

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
  [{{?E_GUARD, ?V_TEST}, element(2, Test), Test} | Errors].


check_guard([], Errors) ->
  Errors;
check_guard([Test], Errors) ->
  check_test(Test, Errors);
check_guard([_, Test | _], Errors) ->
  [{?E_GUARD, element(2, Test), Test} | Errors].

%%check_guard([], Errors) ->
%%  Errors.
%%check_guard([Test | Tests], Errors) ->
%%  Errors0 = check_test(Test, Errors),
%%  check_guard(Tests, Errors0).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% TODO: This is used only for checking but not to add types to the type map.
%% The type map is only used by the forms parsing.

%% @private Atomic literal type.
check_type(Type = {Lit, _, _}, _, Errors) when ?IS_LIT_TYPE(Lit) ->
  ?TRACE("Checking literal type: ~p", [Type]),
  check_lit(Type, Errors);

%% TODO: Add pid and other basic types such as none. Consider moving this after the tuple and uniton
%% @private Built-in types.
check_type(Type = {type, ANNO, N, Types}, Opts, Errors) when is_atom(N), N =/= union, N =/= tuple -> % Could use a marco for basic types.
%%  when ?IS_LIT_TYPE(N) ->

  ?TRACE("Checking built in type: ~p", [N]),
  ?TRACE("Allowed built-ins: ~p", [proplists:get_value(type, Opts, [])]),
  ?TRACE("Permitted: ~p", [lists:member(N, proplists:get_value(type, Opts, []))]),
  Errors0 =
    case lists:member(N, proplists:get_value(type, Opts, [])) of
      true -> Errors;
      false -> [{{?E_TYPE, ?V_BUILTIN}, ANNO, Type} | Errors]
    end,
%%
%%    case proplists:is_defined(?ALLOW_TYPE_PID, Opts) of
%%      true -> Errors;
%%      false -> [{{type, pid}, ANNO, Type} | Errors]
%%    end,

  check_type_seq(Types, Opts, Errors0);

%% @private Type union.
check_type({type, _, union, Types}, Opts, Errors) when is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Type variable.
check_type({var, _, Name}, Opts, Errors) when is_atom(Name) ->
  Errors;

%% @private User-defined type.
check_type({user_type, _, N, Types}, Opts, Errors) when is_atom(N), is_list(Types) ->
  check_type_seq(Types, Opts, Errors);

%% @private Tuple type. Currently only tagged tuple with first element as an
%% atom is supported. This encodes Erlang messages. The tagged tuple must have
%% at least one element, which is the message tag itself.
check_type({type, _, tuple, Types}, Opts, Errors) ->
%%check_type({type, _, tuple, [Tag | Types]}, Errors) ->
%%  when element(1, Tag) =:= atom ->
  % TODO: Add the checking code here to accept only tagged tuples.
  ?TRACE("Checking tuple with types: ~p", [Types]),

%%  case check_tuple()

%%  case check_type_tuple()

%%  case tuple_elems(Types) of
%%    tagged ->
%%
%%  end,

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
  [{?E_TYPE, element(2, Type), Type} | Errors].

%% @private Type sequence.
check_type_seq([], _, Errors) ->
  Errors;
check_type_seq([Type | Types], Opts, Errors) ->
  Errors0 = check_type(Type, Opts, Errors),
  check_type_seq(Types, Opts, Errors0).

%% TODO: Potentially need to pass in the TSpecs to construct function types as well. We'll see.
%% @private Function type. Currently only allowable in -spec.
check_fun_type({type, _, 'fun', [{type, _, product, Types}, T_0]}, Errors)
  when is_list(Types) ->

  % Forbid PID parameter and return types. Only support these indirectly via
  % user defined types.
  Errors0 = check_type_seq(Types, [{type, ?LIT_TYPES}], Errors),
  check_type(T_0, [{type, ?LIT_TYPES}], Errors0).

%% @private Function type sequence. Currently only allowable in -spec.
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


%%check_tuple({tuple, _, [{atom, _, _} | _]}) ->
%%  ok;
%%check_tuple({tuple, _, []}) ->
%%  ?EMPTY_TUPLE;
%%check_tuple({tuple, _, _}) ->
%%  ?UNTAGGED_TUPLE.

%%check_fun_call({call, _, {atom, _, _}, _}) ->
%%  ok;
%%check_fun_call({call, _, {remote, _, _, _}, _}) ->
%%  ?REMOTE_CALL;
%%check_fun_call({call, _, _, _}) ->
%%  ?INDIRECT_CALL.
