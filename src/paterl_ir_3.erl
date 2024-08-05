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

-define(litValue(Expr), element(3, Expr)).

-define(isLit(Expr), ?synCat(Expr) =:= integer
  orelse ?synCat(Expr) =:= float
  orelse ?synCat(Expr) =:= string
  orelse ?synCat(Expr) =:= atom
).

-define(isInteger(Expr), ?synCat(Expr) =:= integer).

-define(isFloat(Expr), ?synCat(Expr) =:= float).

-define(isString(Expr), ?synCat(Expr) =:= string).

-define(isAtom(Expr), ?synCat(Expr) =:= atom).

-define(isVar(Expr), ?synCat(Expr) =:= var).

-define(isTuple(Expr), ?synCat(Expr) =:= tuple).

-define(isReceive(Expr), ?synCat(Expr) =:= 'receive').

-define(isOp(Expr), ?synCat(Expr) =:= op).

-define(isIf(Expr), ?synCat(Expr) =:= 'if').

-define(isCase(Expr), ?synCat(Expr) =:= 'case').

-define(isMatch(Expr), ?synCat(Expr) =:= match).

-define(isCall(Expr), ?synCat(Expr) =:= call).

-define(isVal(Expr), ?isLit(Expr) andalso ?isVar(Expr) andalso ?isTuple(Expr)).

-define(isMsg(Expr), ?isTuple(Expr)
  andalso length(element(3, Expr)) >= 1
  andalso ?isAtom(hd(element(3, Expr)))
).

-define(isMbAnno(Expr), ?isTuple(Expr)
  andalso length(element(3, Expr)) =:= 2
  andalso ?isAtom(hd(element(3, Expr)))
  andalso (?litValue(hd(element(3, Expr))) =:= new
    orelse ?litValue(hd(element(3, Expr))) =:= use
    orelse ?litValue(hd(element(3, Expr))) =:= state
  )
).

-define(isImplicitCall(Expr), ?isCall(Expr)
  andalso ?isAtom(element(3, Expr))
  andalso is_list(element(4, Expr))
).

-define(isExplicitCall(Expr), ?isCall(Expr)
  andalso not(?isAtom(element(3, Expr)))
  andalso is_list(element(4, Expr))
).



test_expr_cat(Expr) when ?isMbAnno(Expr) ->
  io:format("Is MB annotation ~p.~n", [Expr]);
test_expr_cat(Expr) when ?isMsg(Expr) ->
  io:format("Is message ~p.~n", [Expr]);
test_expr_cat(Expr) when ?isImplicitCall(Expr) ->
  io:format("Is implicit call ~p.~n", [Expr]);
test_expr_cat(Expr) when ?isExplicitCall(Expr) ->
  io:format("Is explicit call ~p.~n", [Expr]);
test_expr_cat(Expr) ->
  io:format("Unrecognized expression ~p.~n", [Expr]).



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
  % Literal expressions.
  Anno = erl_syntax:get_pos(Lit),
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(erl_syntax:match_expr(Var, Lit), Anno);
expr(Lit, false) when ?isLit(Lit) ->
  % Literal expressions.
  Lit;
expr(Expr, true) when ?isVar(Expr) ->
  % Variable expression.
  Anno = erl_syntax:get_pos(Expr),

  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr, false) when ?isVar(Expr) ->
  % Variable expression.
  Expr;
expr(Expr, _) when ?isMbAnno(Expr) ->
  % Erlang mailbox-annotation expression remains unchanged.
  Expr;
expr(Expr, true) when ?isMsg(Expr) ->
  % Erlang Pat message expression. The tuple expression itself is not translated
  % since its arguments are assumed to be values and not expressions.
  % TODO: ANF.
  Anno = erl_syntax:get_pos(Expr),

  Var = erl_syntax:set_pos(erl_syntax:variable(
    fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
%%expr(Expr = {tuple, Anno, [_Tag = {atom, _, _Name} | _Args]}, false) ->
expr(Expr, false) when ?isMsg(Expr) ->
  Expr;
%%expr(Expr = {call, Anno, _Fun = {atom, _, _Name}, _Args}, true) ->
expr(Expr, true) when ?isImplicitCall(Expr) ->
  % Erlang call expression. The call expression itself is not translated since
  % its arguments are assumed to be values and not expressions.
  % TODO: ANF.
  Anno = erl_syntax:get_pos(Expr),

  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
%%expr(Expr = {call, Anno, _Fun = {atom, _, _Name}, _Args}, false) ->
expr(Expr, false) when ?isImplicitCall(Expr) ->
  Expr;
expr(Expr, _) when ?isMatch(Expr) ->
  % Erlang match expression. Retain pattern and transform RHS expression only if
  % RHS is not a value.
  Anno = erl_syntax:get_pos(Expr),
  Pat = erl_syntax:match_expr_pattern(Expr),
  Body = erl_syntax:match_expr_body(Expr),

  erl_syntax:set_pos(
    erl_syntax:match_expr(Pat, expr(Body, false)), Anno
  );
expr(Expr, true) when ?isOp(Expr) ->
  % Erlang binary and unary operator expressions. The expressions themselves are
  % not translated since its operands are assumed to be values, not expressions.
  Anno = erl_syntax:get_pos(Expr),

  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr, false) when ?isOp(Expr) ->
  % Erlang binary and unary operator expressions.
  Expr;
%%expr({'if', Anno, Clauses}, true) ->
expr(Expr, true) when ?isIf(Expr) ->
  % Erlang if expression.
  Anno = erl_syntax:get_pos(Expr),
  Clauses = erl_syntax:if_expr_clauses(Expr),

  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  If = erl_syntax:set_pos(erl_syntax:if_expr(if_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, If),
    Anno
  );
expr(Expr, false) when ?isIf(Expr) ->
  % Erlang if expression.
  Anno = erl_syntax:get_pos(Expr),
  Clauses = erl_syntax:if_expr_clauses(Expr),

  erl_syntax:set_pos(erl_syntax:if_expr(if_clauses(Clauses)), Anno);
expr(Expr, true) when ?isReceive(Expr) ->
  % Receive and receive with timeout expressions.
  Anno = erl_syntax:get_pos(Expr),
  Clauses = erl_syntax:receive_expr_clauses(Expr),

  % Erlang receive expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  Receive = erl_syntax:set_pos(erl_syntax:receive_expr(case_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Receive),
    Anno
  );
expr(Expr, false) when ?isReceive(Expr) ->
  % Receive and receive with timeout expressions.
  Anno = erl_syntax:get_pos(Expr),
  Clauses = erl_syntax:receive_expr_clauses(Expr),
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