%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2024 10:32
%%%-------------------------------------------------------------------
-module(paterl_ir_2).
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

%% TODO: Add code to normalize Erlang ASTs.
%% For now this only involves expanding specific Erlang expressions to match expressions.
%% Later, I need to add calls to functions to be externalized so that values are bound to variables, which is a huge task.

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
expr_seq([Expr])
  when element(1, Expr) =:= 'receive'; element(1, Expr) =:= 'if' ->
  % Singleton expression sequence that have a body. Transform.
  [expr(Expr)];
expr_seq([Expr]) ->
  % Singleton expression sequence. Do not transform.
  [Expr];
expr_seq([Expr | ExprSeq]) ->
  [expr(Expr) | expr_seq(ExprSeq)].


%% @private Transforms Erlang values and expressions.
expr(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal expressions.
  Anno = element(2, Lit),
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(erl_syntax:match_expr(Var, Lit), Anno);
expr(Expr = {var, Anno, _}) ->
  % Variable expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {tuple, _, [{atom, _, Name}, _]})
  when Name =:= new; Name =:= use; Name =:= state ->
  % Erlang mailbox-annotation expression remains unchanged.
  Expr;
expr(Expr = {tuple, Anno, [_Tag = {atom, _, _Name} | _Args]}) ->
  % Erlang Pat message expression.
  Var = erl_syntax:set_pos(erl_syntax:variable(
    fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
%%expr(Expr = {call, Anno, {atom, _, spawn}, _MFArgs = [_, _Fun, _Args]}) ->
%%  % Spawn expression.
%%  Expr;
%%expr(Expr = {call, Anno, {atom, _, format}, _}) ->
%%  % Format call expressions.
%%  Expr;
expr(Expr = {call, Anno, _Fun = {atom, _, _Name}, _Args}) ->
  % Erlang call expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {match, _, _, _}) ->
  % Erlang match expression remains unchanged.
  Expr;
expr(Expr = {op, Anno, _, _, _}) ->
  % Erlang binary operator expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {op, Anno, _, _}) ->
  % Erlang unary operator expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr({'if', Anno, Clauses}) ->
  % Erlang if expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  Clauses0 = erl_syntax:set_pos(erl_syntax:if_expr(if_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Clauses0),
    Anno
  );
expr({'receive', Anno, Clauses}) ->
  % Erlang receive expression.
  Var = erl_syntax:set_pos(
    erl_syntax:variable(fresh_var()), Anno
  ),
  Clauses0 = erl_syntax:set_pos(erl_syntax:receive_expr(case_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Clauses0),
    Anno
  ).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%% @private Returns a fresh variable name.
fresh_var() ->
  paterl_tools:fresh_var(?TEMP_VAR_NAME).
