%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2024 10:32
%%%-------------------------------------------------------------------
-module(paterl_ir).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%%% API
-export([module/1]).
-compile(export_all).

%% TODO: Add code to normalize Erlang ASTs.
%% For now this only involves expanding specific Erlang expressions to match expressions.
%% Later, I need to add calls to functions to be externalized so that values are bound to variables, which is a huge task.

module(Forms) ->
  erl_syntax:revert_forms(forms(Forms)).

forms(Forms) ->
  [form(Form) || Form <- Forms].

form(Form = {function, Anno, Name, Arity, Clauses}) ->
  erl_syntax:set_pos(
    erl_syntax:function(erl_syntax:atom(Name), fun_clauses(Clauses)),
    Anno
  );
form(Form) ->
  % Other Erlang forms.
  Form.


fun_clauses(Clauses) ->
  clauses(fun fun_clause/1, Clauses).

case_clauses(Clauses) ->
  clauses(fun case_clause/1, Clauses).

if_clauses(Clauses) ->
  clauses(fun if_clause/1, Clauses).

clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  ?TRACE("Clauses ~p~n", [Clauses]),
  [Fun(Clause) || Clause <- Clauses].


fun_clause({clause, Anno, PatSeq, GuardSeq, Body}) ->
  erl_syntax:set_pos(erl_syntax:clause(PatSeq, GuardSeq, expr_seq(Body)), Anno).

case_clause(Clause = {clause, Anno, [Pat], GuardSeq, Body}) ->
  % Receive or case clause.
  erl_syntax:set_pos(erl_syntax:clause([Pat], GuardSeq, expr_seq(Body)), Anno).

%% @private Translates an if clause.
%%{clause,ANNO,[],Rep(Gs),Rep(B)}
if_clause(Clause = {clause, Anno, [], GuardSeq, Body}) ->
  % If clause.
  erl_syntax:set_pos(erl_syntax:clause(GuardSeq, expr_seq(Body)), Anno).


%% @private Translates value and expression sequences.
%%expr_seq(ExprSeq) when is_list(ExprSeq) ->
%%  [expr(Expr) || Expr <- ExprSeq].

expr_seq([]) ->
  [];
expr_seq([Expr])
  when element(1, Expr) =:= 'receive'; element(1, Expr) =:= 'if' ->
  [expr(Expr)];
expr_seq([Expr]) ->
  [Expr];
expr_seq([Expr | ExprSeq]) ->
  [expr(Expr) | expr_seq(ExprSeq)].


%% @private Translates values and expressions.
expr(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  Anno = element(2, Lit),
  Var = erl_syntax:set_pos(erl_syntax:variable('LitVar'), Anno),
  erl_syntax:set_pos(erl_syntax:match_expr(Var, Lit), Anno);
expr(Expr = {var, Anno, Name}) ->
  % Variable.
  Var = erl_syntax:set_pos(erl_syntax:variable('VarVar'), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr = {tuple, Anno, [{atom, _, Name}, _]})
  when Name =:= new; Name =:= use; Name =:= state ->
  % Mailbox-annotation remains unchanged.
  Expr;
expr(Expr = {tuple, Anno, [_Tag = {atom, _, Name} | Args]}) ->
  % Message.
  Var = erl_syntax:set_pos(erl_syntax:variable('MsgVar'), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
%%expr(Expr = {call, Anno, {atom, _, spawn}, _MFArgs = [_, _Fun, _Args]}) ->
%%  % Spawn expression.
%%  Expr;
%%expr(Expr = {call, Anno, {atom, _, format}, _}) ->
%%  % Format call expressions.
%%  Expr;
expr(Expr = {call, Anno, Fun = {atom, _, Name}, Args}) ->
  % Call expr.
  Var = erl_syntax:set_pos(erl_syntax:variable('CAllVar'), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr0 = {match, Anno, Pat, Expr}) ->
  % Match expression remains unchanged.
  Expr0;
expr(Expr = {op, Anno, Op, Expr0, Expr1}) ->
  % Binary operator expression.
  Var = erl_syntax:set_pos(erl_syntax:variable('TempBin'), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr), Anno
  );
expr(Expr0 = {op, Anno, Op, Expr}) ->
  % Unary operator expression.
  Var = erl_syntax:set_pos(erl_syntax:variable('TempUn'), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Expr0), Anno
  );
expr(Expr = {'if', Anno, Clauses}) ->
  Var = erl_syntax:set_pos(erl_syntax:variable('IfVar'), Anno),
  Clauses0 = erl_syntax:set_pos(erl_syntax:if_expr(if_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Clauses0),
    Anno
  );
expr(Expr = {'receive', Anno, Clauses}) ->
  Var = erl_syntax:set_pos(erl_syntax:variable('ReceiveVar'), Anno),
  Clauses0 = erl_syntax:set_pos(erl_syntax:receive_expr(case_clauses(Clauses)), Anno),
  erl_syntax:set_pos(
    erl_syntax:match_expr(Var, Clauses0),
    Anno
  ).
%%  Expr;
%%expr(Expr) ->
%%  Expr.
