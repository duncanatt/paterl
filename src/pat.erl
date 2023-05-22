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


file(File) ->
  case epp:parse_file(File, []) of
    {ok, Forms} ->
      {TSpecs, Errors} = check_forms(Forms, #{}, []),
      io:format("Errors: ~p~n~n", [Errors]),
%%      There is a problem with lists because the way I am building them is not a
%%      proper list but an improper one that needs to be flattened. Rather than
%%      flattening it, I'll build it correctly.

%%      IoList = lists:foldr(
%%        fun(Err, Acc) -> [[pat:format_err(Err), $\n] | Acc] end, [], Errors
%%      ),

      IoList = [[pat:format_err(Error), $\n] || Error <- Errors],
      file:write(standard_error, IoList);
    {error, OpenError} ->
      OpenError
  end.


format_err({Class, ANNO, Node}) ->
  [
    $[, integer_to_list(ANNO), $], $\s, err_msg(Class),
    " not supported: ", erl_prettypr:format(Node)
  ].


err_msg(form) ->
  "form";
err_msg(lit) ->
  "literal";
err_msg({pat, untagged_tuple}) ->
  "untagged tuple pattern";
err_msg({pat, empty_tuple}) ->
  "empty tuple pattern";
err_msg(pat) ->
  "pattern";
err_msg({expr, untagged_tuple}) ->
  "untagged tuple expression";
err_msg({expr, empty_tuple}) ->
  "empty tuple expression";
err_msg(expr) ->
  "expression";
err_msg(clause) ->
  "clause";
err_msg({type, var}) ->
  "type variables";
err_msg(type) ->
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
  Errors0 =
    if [] =/= Vars ->
      [{{type, var}, ANNO, hd(Vars)} | Errors];
      true -> Errors
    end,
  {TSpecs, check_type(T, Errors0)};


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
  {TSpecs, [{form, element(2, Form), Form} | Errors]}.


%%% ----------------------------------------------------------------------------
%%% Atomic literals.
%%% ----------------------------------------------------------------------------

check_lit({Type, _, _}, Errors) when ?IS_LIT_TYPE(Type) ->
  Errors;
check_lit(Lit, Errors) ->
  [{lit, element(2, Lit), Lit} | Errors].


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
      false -> [{{pat, lit}, ANNO, Pat} | Errors]
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
  {Pats0, Errors0} =
    case check_tuple(Pat) of
      ok ->
        % Tagged tuple. Check that rest of elements are variables only.
        ?TRACE("TUPLE pattern valid"),
        {tl(Pats), Errors}; % Pass on the rest of the tuple elements
      empty_tuple ->
        % Empty tuple.
        ?TRACE("TUPLE pattern empty"),
        {Pats, [{{pat, empty_tuple}, ANNO, Pat} | Errors]};
      Error ->
        % Untagged tuple. Check that rest of elements are variables only.
        ?TRACE("TUPLE pattern does not have a tag"),
        {tl(Pats), [{{pat, Error}, ANNO, Pat} | Errors]}
    end,
%%  check_pat_seq(Pats0, [?ALLOW_VAR], Errors0); % For instance, here I need to allow variables but not literals.
  check_pat_seq(Pats0, [?ALLOW_VAR | Opts], Errors0); % For instance, here I need to allow variables but not literals.

%% @private Variable pattern.
check_pat(Pat = {var, ANNO, Name}, Opts, Errors) when is_atom(Name) ->
  ?TRACE("Checking VAR pattern ~p", [Pat]),
  case proplists:is_defined(?ALLOW_VAR, Opts) of
    true ->
      ?TRACE("VAR pattern ok"),
      Errors;
    false ->
      ?TRACE("VAR pattern disallowed"),
      [{{pat, var}, ANNO, Pat} | Errors]
  end;
%%,
%%  Errors;

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
  [{pat, element(2, Pat), Pat} | Errors].

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

  Errors0 =
    case check_fun_call(Expr) of
      ok ->
%%        check_expr(E_0, Errors),
        Errors;
      Error ->
        [{{expr, Error}, ANNO, Expr} | Errors]
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
  Errors0 = check_pat(P, true, Errors),
  check_expr_seq(E_0, Errors0);

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
  Errors0 =
    case check_tuple(Expr) of
      ok ->
        Errors;
      Error ->
        [{{expr, Error}, ANNO, Expr} | Errors]
    end,
  check_expr_seq(Exprs, Errors0);


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
%% - Tuple skeleton %% TODO Should be supported to send messages, but partially with just a tag, not any tuple!
%% - Try-catch
%% - Try-of-catch
%% - Try-after
%% - Try-of-after
%% - Try-catch-after
%% - Try-of-catch-after
check_expr(Expr, Errors) ->
  [{expr, element(2, Expr), Expr} | Errors].

%% @private Expression sequence.
check_expr_seq([], Errors) ->
  Errors;
check_expr_seq([Expr | Exprs], Errors) ->
  Errors0 = check_expr(Expr, Errors),
  check_expr_seq(Exprs, Errors0).


%%tuple_shape(Tuple = {tuple, ANNO, Elems = [{atom, _, _} | Exprs]}) ->
%%%%  ?INFO("Tuple elements: ~p", [Elems]),
%%  ok;
%%tuple_shape({tuple, ANNO, []}) ->
%%%%  ?ERROR("--- We have an empty tuple"),
%%  empty;
%%tuple_shape(_) ->
%%  untagged.


check_tuple({tuple, _, [{atom, _, _} | _]}) ->
  ok;
check_tuple({tuple, _, []}) ->
  empty_tuple;
check_tuple({tuple, _, _}) ->
  untagged_tuple.


check_receive_clause_seq() ->
  ok.

check_fun_call({call, _, {atom, _, _}, _}) ->
  ok;
check_fun_call({call, _, {remote, _, _, _}, _}) ->
  remote_call;
check_fun_call({call, _, _, _}) ->
  indirect_call.

%%{tuple, 59, []},
%%{tuple, 60, [{atom, 60, hello}]},
%%{tuple, 61, [{atom, 61, there}, {integer, 61, 5}]},
%%{tuple, 62, [{integer, 62, 5}]},
%%{tuple, 63, [{'fun', 63, {clauses, [{clause, 63, [], [], [{atom, 63, ok}]}]}}]}

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

%%% TODO: Consider removing this since Pat does not support Erlang-style guards. NO Should be here, test IF condition.
%%%% @private If clause.
%%check_clause({clause, _, [], Gs, B}, Opts, Errors) ->
%%  Errors0 = check_guard_seq(Gs, Errors),
%%  check_expr_seq(B, Errors0);

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
  [{clause, element(2, Clause), Clause} | Errors].

%% @private Clause sequence.
check_clause_seq([], _, Errors) ->
  Errors;
check_clause_seq([Clause | Clauses], Opts, Errors) ->
  Errors0 = check_clause(Clause, Opts, Errors),
  check_clause_seq(Clauses, Opts, Errors0).


%%% ----------------------------------------------------------------------------
%%% Guards.
%%% ----------------------------------------------------------------------------

%%check_guard_seq([], Errors) ->
%%  Errors;
%%check_guard_seq([Guard | Guards], Errors) ->
%%  Errors0 = check_guard(Guard, Errors),
%%  check_guard_seq(Guards, Errors0).

%% TODO: Pat does not support guards for now, and neither shall we.


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% TODO: This is used only for checking but not to add types to the

%% @private Atomic literal type.
check_type(Type = {Lit, _, _}, Errors) when ?IS_LIT_TYPE(Lit) ->
  ?TRACE("Checking literal type: ~p", [Type]),
  check_lit(Type, Errors);

%% TODO: Add pid and other basic types such as none.
%% @private Built-in types.
check_type(_Type = {type, _, N, Types}, Errors) when ?IS_LIT_TYPE(N) ->
  ?TRACE("Checking built in type: ~p", [_Type]),
  check_type_seq(Types, Errors);

%% @private Type union.
check_type({type, _, union, Types}, Errors) when is_list(Types) ->
  check_type_seq(Types, Errors);

%% @private Type variable.
check_type({var, _, Name}, Errors) when is_atom(Name) ->
  Errors;

%% @private User-defined type.
check_type({user_type, _, N, Types}, Errors) when is_atom(N), is_list(Types) ->
  check_type_seq(Types, Errors);

%% @private Tuple type. Currently only tagged tuple with first element as an
%% atom is supported. This encodes Erlang messages. The tagged tuple must have
%% at least one element, which is the message tag itself.
check_type({type, _, tuple, Types}, Errors) ->
%%check_type({type, _, tuple, [Tag | Types]}, Errors) ->
%%  when element(1, Tag) =:= atom ->
  ?TRACE("Checking tuple with types: ~p", [Types]),
  check_type_seq(Types, Errors);

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
check_type(Type, Errors) ->
  [{type, element(2, Type), Type} | Errors].

%% @private Type sequence.
check_type_seq([], Errors) ->
  Errors;
check_type_seq([Type | Types], Errors) ->
  Errors0 = check_type(Type, Errors),
  check_type_seq(Types, Errors0).

%% TODO: Potentially need to pass in the TSpecs.
%% @private Function type. Currently only allowable in -spec.
check_fun_type({type, _, 'fun', [{type, _, product, Types}, T_0]}, Errors)
  when is_list(Types) ->
  Errors0 = check_type_seq(Types, Errors),
  check_type(T_0, Errors0).

%% @private Function type sequence. Currently only allowable in -spec.
check_fun_type_seq([], Errors) ->
  Errors;
check_fun_type_seq([FType | FTypes], Errors) ->
  Errors0 = check_fun_type(FType, Errors),
  check_fun_type_seq(FTypes, Errors0).


%%%% @private Function type sequence.
%%trans_fun_types([], TInfo = #t_info{}) ->
%%  {[], TInfo};
%%trans_fun_types([Type | Types], TInfo = #t_info{}) ->
%%  {Type0, TInfo0} = trans_fun_type(Type, TInfo),
%%  {Types0, TInfo1} = trans_fun_types(Types, TInfo0),
%%%%  [trans_fun_type(Type) | trans_fun_types(Types)].
%%  {[Type0 | Types0], TInfo1}.

%%check_type(Type, TSpec, Errors) ->
%%  {TSpec, Errors}.