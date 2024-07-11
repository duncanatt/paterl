%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2024 10:32
%%%-------------------------------------------------------------------
-module(paterl_ir_3).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%%% API
-export([module/1]).
-compile(export_all).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Temporary variable name.
-define(TEMP_VAR_NAME, v).

%%-define(IS_LIT(Expr), true).
%%
%%-define(IS_VAR(Expr), true).
%%
%%-define(IS_TUPLE(Expr), true).
%%
%%-define(IS_VAL(Expr), ?IS_LIT(Expr)
%%  orelse ?IS_VAR(Expr)
%%  orelse ?IS_TUPLE(Expr)).

-define(synCat(Expr), element(1, Expr)).

-define(anno(Expr), element(2, Expr)).

-define(isLit(Expr), ?synCat(Expr) =:= integer
  orelse ?synCat(Expr) =:= float
  orelse ?synCat(Expr) =:= string
  orelse ?synCat(Expr) =:= atom
).

-define(isVar(Expr), ?synCat(Expr) =:= var).

-define(isTuple(Expr), ?synCat(Expr) =:= tuple).

-define(isVal(Expr), ?isLit(Expr) andalso ?isVar(Expr) andalso ?isTuple(Expr)).


%% TODO: Add code to normalize Erlang ASTs. Called assignment transformation.
%% For now this only involves expanding specific Erlang expressions to match expressions.
%% Later, I need to add calls to functions to be externalized so that values are bound to variables,
%% which is a huge task and is called a-normal form.


%% @doc Transforms an Erlang abstract syntax representation to its Erlang
%% intermediate representation.
module(Forms) ->
  erl_syntax:revert_forms(forms(Forms)).


%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

%% @private Transforms Erlang forms.
forms(Forms) ->
  [form(Form) || Form <- Forms].

%% @private Transforms Erlang functions.
form({function, Anno, Name, _, Clauses}) ->
  erl_syntax:set_pos(
    erl_syntax:function(erl_syntax:atom(Name), fun_clauses(Clauses)),
    Anno
  );
form(Form) ->
  % Other Erlang forms.
  Form.

%% @private Generic function to transform Erlang clauses.
clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  [Fun(Clause) || Clause <- Clauses].

%% @private Transforms Erlang function clauses.
fun_clauses(Clauses) ->
  clauses(fun fun_clause/1, Clauses).

%% @private Transforms Erlang case and receive clauses.
case_clauses(Clauses) ->
  clauses(fun case_clause/1, Clauses).

%% @private Transforms Erlang if clauses.
if_clauses(Clauses) ->
  clauses(fun if_clause/1, Clauses).

%% @private Transforms Erlang function clause.
fun_clause({clause, Anno, PatSeq, GuardSeq, Body}) ->
  erl_syntax:set_pos(erl_syntax:clause(PatSeq, GuardSeq, expr_seq(Body)), Anno).

%% @private Transforms Erlang case and receive clause.
case_clause({clause, Anno, [Pat], GuardSeq, Body}) ->
  % Receive and case clause.
  erl_syntax:set_pos(erl_syntax:clause([Pat], GuardSeq, expr_seq(Body)), Anno).

%% @private Transforms Erlang if clause.
if_clause({clause, Anno, [], GuardSeq, Body}) ->
  % If clause.
  erl_syntax:set_pos(erl_syntax:clause(GuardSeq, expr_seq(Body)), Anno).

%% @private Transforms Erlang expression sequences.
expr_seq([]) ->
  [];
%%expr_seq([Expr])
%%  when element(1, Expr) =:= 'receive'; element(1, Expr) =:= 'if'; element(1, Expr) =:= match ->
  % Singleton expression sequence that are not values. Transform.
%%  [expr(Expr)];
%%expr_seq([Expr]) when ?isVal(Expr) ->
%%  % Singleton expression sequence where the expression is a value.
%%  [Expr];
expr_seq([Expr]) ->
  % Singleton expression sequence where the expression is a value.
  [expr(Expr, false)];
expr_seq([Expr | ExprSeq]) ->
  [expr(Expr, true) | expr_seq(ExprSeq)].

% Assign flag true = make the expression in a match; false = do not make the expression in a match but do make its body.
%% @private Transforms Erlang values and expressions.
expr(Lit, true) when ?isLit(Lit) ->
%%  element(1, Lit) =:= integer;
%%  element(1, Lit) =:= float;
%%  element(1, Lit) =:= string;
%%  element(1, Lit) =:= atom ->
  % Literal expressions.
%%  Anno = element(2, Lit),
  Anno = erl_syntax:get_pos(Lit),
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(erl_syntax:match_expr(Var, Lit), Anno);
expr(Lit, false) when ?isLit(Lit) ->
  Lit;
expr(Expr = {var, Anno, _}, true) ->
  % Variable expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {var, Anno, _}, false) ->
  Expr;
expr(Expr = {tuple, _, [{atom, _, Name}, _]}, _)
  when Name =:= new; Name =:= use; Name =:= state ->
  % Erlang mailbox-annotation expression remains unchanged.
  Expr;
expr(Expr = {tuple, Anno, [_Tag = {atom, _, _Name} | _Args]}, true) ->
  % Erlang Pat message expression. The tuple expression itself is not translated
  % since its arguments are assumed to be values and not expressions.
  % TODO: ANF.
  Var = erl_syntax:set_pos(erl_syntax:variable(
    fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {tuple, Anno, [_Tag = {atom, _, _Name} | _Args]}, false) ->
  Expr;
expr(Expr = {call, Anno, _Fun = {atom, _, _Name}, _Args}, true) ->
  % Erlang call expression. The call expression itself is not translated since
  % its arguments are assumed to be values and not expressions.
  % TODO: ANF.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {call, Anno, _Fun = {atom, _, _Name}, _Args}, false) ->
  Expr;
expr({match, Anno, Pat, Expr}, _) ->
  % Erlang match expression. Retain pattern and transform RHS expression only if
  % not a value.
%%  ?TRACE("Body = ~p", [Expr]),
%%  ExprSeq0 = expr(Expr),
%%  ?TRACE("ExprSeq0 = ~p", [ExprSeq0]),

%%  Expr0 = if ?isVal(Expr) -> Expr; true -> expr(Expr) end,

  erl_syntax:set_pos(
%%    erl_syntax:match_expr(Pat, Expr0), Anno
    erl_syntax:match_expr(Pat, expr(Expr, false)), Anno
  );
expr(Expr = {op, Anno, _, _, _}, true) ->
  % Erlang binary operator expression. The expression itself is not translated
  % since its operands are assumed to be values and not expressions.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {op, Anno, _, _, _}, false) ->
  Expr;
expr(Expr = {op, Anno, _, _}, true) ->
  % Erlang unary operator expression. The expression itself is not translated
  % since its operand is assumed to be a value and not an expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {op, Anno, _, _}, false) ->
  Expr;
expr({'if', Anno, Clauses}, true) ->
  % Erlang if expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  If = erl_syntax:set_pos(erl_syntax:if_expr(if_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, If),
    Anno
  );
expr({'if', Anno, Clauses}, false) ->
  erl_syntax:set_pos(erl_syntax:if_expr(if_clauses(Clauses)), Anno);
expr({'receive', Anno, Clauses}, true) ->
  % Erlang receive expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  Receive = erl_syntax:set_pos(erl_syntax:receive_expr(case_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Receive),
    Anno
  );
expr({'receive', Anno, Clauses}, false) ->
  erl_syntax:set_pos(erl_syntax:receive_expr(case_clauses(Clauses)), Anno).

%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%% @private Returns a fresh variable name.
fresh_var() ->
  paterl_tools:fresh_var(?TEMP_VAR_NAME).

%%is_val(Expr)
%%  when ?synCat(Expr) =:= integer;
%%  ?synCat(Expr) =:= float;
%%  ?synCat(Expr) =:= string;
%%  ?synCat(Expr) =:= atom ->
%%  true;
%%is_val(Expr) when ?synCat(Expr) =:= var ->
%%  true;
%%is_val(Expr) when ?synCat(Expr) =:= tuple ->
%%  true;
%%is_val(_) ->
%%  false.


-spec fib2() -> integer().
fib2() ->
%%  T = U = V = 50,
  U = 50,
  60,
  T = if 1 == 1 -> 10; true -> Y = 20, 30, 40, ?mb_assert_regex("Resp"), receive {a, X} -> X end end.