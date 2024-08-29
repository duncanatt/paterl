%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2023 12:44
%%%-------------------------------------------------------------------
-module(trans_typespecs).
-author("Duncan Paul Attard").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%% API
-export([]).

-compile(export_all).

%%% Callbacks.
-export([parse_transform/2]).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%FIX THIS
-define(
NO_SUPPORT(Node, Class),
  io:format(
    standard_io,
    "\e[31m[~p:~p] Unsupported ~s: ~s\e[0m~n",
%%    "[~p:~p] Unsupported ~p.~n",
    tuple_to_list(element(2, Node)) ++ [Class, erl_prettypr:format(Node)]
  )
).

%%-define(NO_SUPPORT_SYNTAX(Node), ?NO_SUPPORT(Node, 'Syntax')).
%%-define(NO_SUPPORT_TYPE(Node), ?NO_SUPPORT(Node, 'Type')).

%%-define(LIT_TYPES, [integer, float, string]).

%% Literal types supported by Pat.
-define(IS_LIT_TYPE(Type), Type =:= integer; Type =:= float; Type =:= string; Type =:= atom).

%% Function signatures.
-type sigs() :: [{atom(), integer()}].

%% Program type information.
-record(t_info, {
  mb_sigs = #{} :: #{atom() => sigs()},
  types = #{} :: #{atom() => tuple()}
}).

-type errors() :: list().

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------


file(File) ->
  case filelib:is_file(File) of
    true ->

      CompileOpts = [
        {parse_transform, ?MODULE}
      ],

      compile:file(File, CompileOpts);
    false ->
      invalid_file
  end.



parse_transform(Ast, Opts) ->
  {Forms, #t_info{mb_sigs = MbSigs, types = Types}} = trans_mod(Ast, #t_info{}),
  ?DEBUG("Forms are: ~p~n", [Forms]),

  io:format("MbSigs: ~p~n~n", [MbSigs]),
  io:format("Types: ~p~n", [Types]),


  Forms.


%%% ----------------------------------------------------------------------------
%%% Module declaration and forms.
%%% ----------------------------------------------------------------------------

%% @private Module declaration.
trans_mod([], TInfo = #t_info{}) ->
  {[], TInfo};
trans_mod([Form | Forms], TInfo = #t_info{}) ->
  io:format("Form: ~p~n", [Form]),
  {Form0, TInfo0} = trans_form(Form, TInfo),
  {Forms0, TInfo1} = trans_mod(Forms, TInfo0),
%%  {[Form0 | trans_mod(Forms, TInfo = #t_info{})], TInfo}.
  {[Form0 | Forms0], TInfo1}.

%% @private Export annotation.
trans_form(Form = {attribute, _, export, _Funs}, TInfo = #t_info{}) when is_list(_Funs) ->
  {Form, TInfo};

%% @private Import annotation.
trans_form(Form = {attribute, _, import, {_Mod, _Funs}}, TInfo = #t_info{})
  when is_atom(_Mod), is_list(_Funs) ->
  {Form, TInfo};

%% @private Module attribute.
trans_form(Form = {attribute, _, module, _Mod}, TInfo = #t_info{}) when is_atom(_Mod) ->
  {Form, TInfo};

%% @private File attribute.
trans_form(Form = {attribute, _, file, {_File, _Line}}, TInfo = #t_info{})
  when is_list(_File), is_integer(_Line) ->
  {Form, TInfo};

%% @private Function declaration.
trans_form(Form = {function, _, Name, Arity, Clauses}, TInfo = #t_info{})
  when is_atom(Name), is_integer(Arity), is_list(Clauses) ->
  trans_clauses(Clauses),
  {Form, TInfo};


%% @private Local function spec.
trans_form(Form = {attribute, _, spec, {{Name, Arity}, Types}}, TInfo = #t_info{})
  when is_atom(Name), is_integer(Arity), is_list(Types) ->
  {_, TInfo0} = trans_fun_types(Types, TInfo),
  {Form, TInfo0};

%% @private Type declaration.
trans_form(Form = {attribute, _, type, {Name, T, Vars}}, TInfo = #t_info{})
  when is_atom(Name), is_list(Vars) ->
  {_, TInfo0} = trans_type(T, TInfo),

  io:format("Type name: ~p~n", [Name]),
  io:format("Type: ~p~n", [T]),
  io:format("Type vars ~p~n", [Vars]),
  trans_type_vars(Vars),
  {Form, TInfo0};


%% @private Errors and warnings.
trans_form(Form = {EW, _}, TInfo = #t_info{}) when EW =:= error; EW =:= warning ->
  {Form, TInfo};

%% @private EOF.
trans_form(Form = {eof, _}, TInfo = #t_info{}) ->
  {Form, TInfo};

%% @private Wild attribute associating mailbox types to functions.
trans_form(Form = {attribute, _, Name, Sigs}, TInfo = #t_info{mb_sigs = MbTypes}) when is_atom(Name), is_list(Sigs) ->

  % Checks whether the specified tuple is a Fun/Arity.
  IsFa =
    fun({Name, Arity}, Flag) when is_atom(Name), is_integer(Arity) -> Flag and true;
      (_, _) -> false
    end,

  case lists:foldl(IsFa, true, Sigs) of
    true ->
      io:format("== Interface ~s associated implemented by functions ~p~n", [Name, Sigs]),
      {Form, TInfo#t_info{mb_sigs = maps:put(Name, Sigs, MbTypes)}};
    false ->
      {Form, TInfo}
  end;



%% @private Wild attribute potentially defining a mailbox type usage.
%%trans_form(Form = {attribute, _, A,T}) when is_atom(A)->
%%  Form;

%% @private Unsupported forms:
%% - Callback
%% - Remote function spec
%% - Record declaration
%% - Opaque type
%% - Wild attribute
trans_form(Form, TInfo = #t_info{}) ->
%%  ?NO_SUPPORT_SYNTAX(Form),
  ?NO_SUPPORT(Form, 'Form'),
  {Form, TInfo}.


%% Function clauses.
%%trans_fun_clauses([]) ->
%%  [];
%%trans_fun_clauses([Clause | Clauses]) ->
%%  [Clause | trans_fun_clauses(Clauses)].

%% Function types.
%%trans_fun_types([]) ->
%%  [];
%%trans_fun_types([Type | Types]) ->
%%  [Type | trans_fun_types(Types)].

%% Type specification.
%%trans_type(Type) ->
%%  Type.

%% Type variables.
%%trans_type_vars([]) ->
%%  [];
%%trans_type_vars([Var | Vars]) ->
%%  [Var | trans_type_vars(Vars)].


%%% ----------------------------------------------------------------------------
%%% Atomic literals.
%%% ----------------------------------------------------------------------------

%% Atomic literals.
%%trans_lit(Lit = {atom, _, L}) when is_atom(L) ->
%%  Lit;
%%trans_lit(Lit = {char, _, L}) when is_integer(L) ->
%%  Lit;
%%trans_lit(Lit = {float, _, L}) when is_float(L) ->
%%  Lit;
%%trans_lit(Lit = {integer, _, L}) when is_integer(L) ->
%%  Lit;
%%trans_lit(Lit = {string, _, Chars}) when is_list(Chars) ->
%%  Lit.

%%trans_lit(Lit = {Type, _, _}) ->
%%%%  io:format("Checking ~p in ~p~n", [Type, ?LIT_TYPES]),
%%  case lists:member(Type, ?LIT_TYPES) of
%%    false ->
%%      ?NO_SUPPORT_TYPE(Lit);
%%    true ->
%%      ok
%%  end,
%%  Lit.

%% @private Atomic literal.
%%{string,{37,10},"Regexp Standalone"}
trans_lit(Lit = {Type, _, _}) when ?IS_LIT_TYPE(Type) ->
  Lit;
trans_lit(Lit) ->
%%  io:format("===> Lit we won't support ~p~n", [Lit]),
  ?NO_SUPPORT(Lit, 'Literal'),
  Lit.


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% @private Atomic literal pattern.
trans_pat(Pat = {Type, _, _}) when ?IS_LIT_TYPE(Type) ->
  trans_lit(Pat);

%% @private Compound pattern.
trans_pat(Pat = {match, _, P_1, P_2}) ->
  trans_pat(P_1),
  trans_pat(P_2),
  Pat;

%% @private Binary operator pattern.
trans_pat(Pat = {op, _, Op, P_1, P_2}) ->
  trans_pat(P_1),
  trans_pat(P_2),
  Pat;

%% @private Unary operator pattern.
trans_pat(Pat = {op, _, Op, P_0}) ->
  trans_pat(P_0),
  Pat;

%% @private Variable pattern.
trans_pat(Pat = {var, _, Name}) when is_atom(Name) ->
  Pat;

%% @private Unsupported patterns:
%% - Bitstring
%% - Cons
%% - Map
%% - Nil
%% - Record field index
%% - Record
%% - Tuple
%% - Universal (i.e., _)
trans_pat(Pat) ->
  ?NO_SUPPORT(Pat, 'Pattern'),
  Pat.

%% @private Pattern sequence.
trans_pats([]) ->
  [];
trans_pats([Pat | Pats]) ->
  [trans_pat(Pat) | trans_pats(Pats)].


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Atomic literal expression.
trans_expr(Expr = {Name, _, _})
  when
%%  Name =:= atom; Name =:= char; Name =:= float; Name =:= integer; Name =:= string ->
  ?IS_LIT_TYPE(Name) ->
  trans_lit(Expr);

%% @private Local function call expression. The remote check is required to
%% emit a legible error message.
trans_expr(Expr = {call, _, E_0, Exprs}) when element(1, remote) =/= remote, is_list(Exprs) ->
  trans_expr(E_0),
  trans_exprs(Exprs),
  Expr;

%% @private If condition expression.
trans_expr(Expr = {'if', _, Clauses}) when is_list(Clauses) ->
  trans_clauses(Clauses),
  Expr;

%% @private Match operator expression.
trans_expr(Expr = {match, _, P, E_0}) ->
  trans_pat(P),
  trans_expr(E_0),
  Expr;

%% @private Binary operator expression.
trans_expr(Expr = {op, _, Op, E_1, E_2}) ->
  trans_expr(E_1),
  trans_expr(E_2),
  Expr;

%% @private Unary operator expression.
trans_expr(Expr = {op, _, Op, E_0}) ->
  trans_expr(E_0),
  Expr;

%% @private Receive expression.
trans_expr(Expr = {'receive', _, Clauses}) when is_list(Clauses) ->
  trans_clauses(Clauses),
  Expr;

%% @private Variable expression.
trans_expr(Expr = {var, _, Name}) when is_atom(Name) ->
  Expr;

%% @private Unsupported expressions:
%% - Bitstring comprehension
%% - Bitstring constructor
%% - Block
%% - Case
%% - Catch
%% - Cons skeleton
%% - Local fun reference
%% - Remote fun reference
%% - Remote function call
%% - List comprehension
%% - Map creation
%% - Map update
%% - Conditional match operator
%% - Maybe
%% - Nil
%% - Receive-after
%% - Record creation
%% - Record field access
%% - Record field index
%% - Record update
%% - Tuple skeleton
%% - Try-catch
%% - Try-of-catch
%% - Try-after
%% - Try-of-after
%% - Try-catch-after
%% - Try-of-catch-after
%% - Qualifier
%% - Generator
%% - Bitstring generator
%% - Type specified
%% - Map field association
%% - Map field assignment
trans_expr(Expr) ->
%%  io:format("----> ~p~n", [Expr]),
  ?NO_SUPPORT(Expr, 'Expression'),
  Expr.

%% @private Expression sequence.
trans_exprs([]) ->
  [];
trans_exprs([Expr | Exprs]) ->
  [trans_expr(Expr) | trans_exprs(Exprs)].

%% @private Body.
trans_body([]) ->
  [];
trans_body([Expr | Exprs]) ->
  [trans_expr(Expr) | trans_body(Exprs)].

%% @private If clauses.
%%trans_if_clause(Clause) ->
%%  Clause.
%%trans_if_clauses([]) ->
%%  [];
%%trans_if_clauses([Clause | Clauses]) ->
%%  [trans_if_clause(Clause) | trans_if_clauses(Clauses)].

%% @private Case clauses.
%%trans_case_clause(Clause) ->
%%  Clause.
%%trans_case_clauses([]) ->
%%  [];
%%trans_case_clauses([Clause | Clauses]) ->
%%  [trans_case_clause(Clause) | trans_case_clauses(Clauses)].


%%% ----------------------------------------------------------------------------
%%% Clauses.
%%% ----------------------------------------------------------------------------

%% @private Function clause.
trans_clause(Clause = {clause, _, Ps, [], B}) ->
  trans_pats(Ps),
  trans_body(B),
  Clause;

%% @private Case clause.
trans_clause(Clause = {clause, _, [P], [], B}) ->
  trans_pat(P),
  trans_body(B),
  Clause;

%% @private Case clause with guard sequence.
trans_clause(Clause = {clause, _, [P], Gs, B}) ->
  trans_pat(P),
  trans_guards(Gs),
  trans_body(B),
  Clause;

%% @private Guard sequence.
trans_clause(Clause = {clause, _, [], Gs, B}) ->
  trans_guards(Gs),
  trans_body(B),
  Clause;

%% @private Unsupported expressions:
%% - Case clause
%% - Case clause with guard sequence
%% - Catch clause with explicit throw
%% - Catch clause with pattern
%% - Catch clause with pattern and variable
%% - Catch clause with guard sequence
%% - Catch clause with pattern and guard sequence
%% - Catch clause with pattern, variable, and guard sequence
%% - Function clause with guard sequence
trans_clause(Clause) ->
  ?NO_SUPPORT(Clause, 'Clause'),
  Clause.

%% @private Clause sequence.
trans_clauses([]) ->
  [];
trans_clauses([Clause | Clauses]) ->
  [trans_clause(Clause) | trans_clauses(Clauses)].


%%% ----------------------------------------------------------------------------
%%% Guards.
%%% ----------------------------------------------------------------------------

%% @private Guard sequence.
trans_guards([]) ->
  [];
trans_guards([Guard | Guards]) ->
  [trans_guard(Guard) | trans_guards(Guards)].

%% TODO: Might have to support string guards to specify regular expressions on
%% TODO: receive expressions in when.

%% @private Atomic literal test. We support only strings to test the commutative
%% regexp with when. If this is not correspond to the Pat syntax we want, then
%% we'll remove this and not support any form of guard tests.
trans_test(Test = {Name, _, _})
  when
%%  Name =:= atom; Name =:= char; Name =:= float; Name =:= integer;
  Name =:= string ->
  trans_lit(Test);

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
trans_test(Test) ->
  ?NO_SUPPORT(Test, 'Guard Test'),
  Test.

%% @private Guard (i.e. a sequence of guard tests).
trans_guard([]) ->
  [];
trans_guard([Test | Tests]) ->
  [trans_test(Test) | trans_guard(Tests)].


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------


%% @private Annotated type.
%% TODO: Delete this
%%trans_type(Type = {ann_type, _, [A, T_0]}, TInfo = #t_info{}) ->
%%  trans_type_var(A),
%%  trans_type(T_0, TInfo),
%%  {Type, TInfo};

%% @private Atomic literal type.
trans_type(Type = {Lit, _, _}, TInfo = #t_info{})
  when ?IS_LIT_TYPE(Lit) ->
%%  ->
  ?DEBUG("Translating literal type: ~p", [Type]),
  {trans_lit(Type), TInfo};


%% Build macro.

%%TODO: HERE: Continue testing

%%TODO: Map out a the future example from Pat to Erlang.

%% TODO: Set the primitive type atoms that Pat supports = N.
%% @private Built-in types.
trans_type(Type = {type, _, N, Types}, TInfo = #t_info{types = Ty}) ->
%%  when ?IS_LIT_TYPE(N) ->

  ?DEBUG("About to translate type: ~p", [Type]),
  {_, TInfo0} = trans_types(Types, TInfo),



  {Type, TInfo0};

%% @private Type union.
trans_type(Union = {type, _, union, _}, TInfo = #t_info{}) ->
  trans_type_union(Union, TInfo);

%% @private Type variable.
trans_type(Var = {var, _, _}, TInfo = #t_info{}) ->
  trans_type_var(Var);

%% @private User-defined type.
trans_type(Type = {user_type, _, N, Types}, TInfo = #t_info{}) when is_atom(N), is_list(Types) ->
  {_, TInfo0} = trans_types(Types, TInfo),
  {Type, TInfo0};

%% @private Function type.
trans_type(Type = {type, _, 'fun', [{type, _, product, _}, _]}, TInfo = #t_info{}) ->
  trans_fun_type(Type, TInfo);


%%Support tuple type with tags.

%% @private Tagged tuple for messages. Tuple must have at least one component.
trans_type(Type = {type, _, tuple, Types}, TInfo = #t_info{})
%%  when element(1, Tag) =:= atom ->
  ->
  io:format("---> Processing tuple type: ~p~n", [Type]),
  {_, TInfo0} = trans_types(Types, TInfo),
  {Type, TInfo0};

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
trans_type(Type, TInfo = #t_info{}) ->
  ?NO_SUPPORT(Type, 'Type'),
  {Type, TInfo}.


%% @private Type sequence.
trans_types([], TInfo = #t_info{}) ->
  {[], TInfo};
trans_types([Type | Types], TInfo = #t_info{}) ->
  ?DEBUG("-> Translating type: ~p", [Type]),
  {Type0, TInfo0} = trans_type(Type, TInfo),
  {Types0, TInfo1} = trans_types(Types, TInfo0),
%%  [trans_type(Type) | trans_types(Types)].
  {[Type0 | Types0], TInfo1}.

%% @private Function type.
trans_fun_type(Type = {type, _, 'fun', [{type, _, product, Types}, T_0]}, TInfo = #t_info{}) when is_list(Types) ->
  {_, TInfo0} = trans_types(Types, TInfo),
  {_, TInfo1} = trans_type(T_0, TInfo0),
  {Type, TInfo1}.

%% @private Function type sequence.
trans_fun_types([], TInfo = #t_info{}) ->
  {[], TInfo};
trans_fun_types([Type | Types], TInfo = #t_info{}) ->
  {Type0, TInfo0} = trans_fun_type(Type, TInfo),
  {Types0, TInfo1} = trans_fun_types(Types, TInfo0),
%%  [trans_fun_type(Type) | trans_fun_types(Types)].
  {[Type0 | Types0], TInfo1}.


%% @private Type variable.
trans_type_var(Var = {var, _, Name}) when is_atom(Name) ->
  Var.

%% @private Type variable sequence.
trans_type_vars([]) ->
  [];
trans_type_vars([Var | Vars]) ->
  [trans_type_var(Var) | trans_type_vars(Vars)].


%% @private Type union.
trans_type_union(Union = {type, _, union, Types}, TInfo = #t_info{}) when is_list(Types) ->
  {_, TInfo0} = trans_types(Types, TInfo),
  {Union, TInfo0}.


% consider using with "string" as the commutative regex.
% interfaces can be defined as unions of messages.