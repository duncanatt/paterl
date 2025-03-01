%%%
%%% %CopyrightBegin%
%%%
%%% Copyright the University of Glasgow 2022-2025. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% %CopyrightEnd%
-module(paterl_ir).
-moduledoc "Erlang syntactic subset SSA transformation.".
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("paterl_syntax.hrl").

%%% Public API.
-export([module/1]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Temporary variable name.
-define(TEMP_VAR_NAME, v).

%% TODO: Add code to normalize Erlang ASTs. Called assignment transformation.
%% TODO: For now this only involves expanding specific Erlang expressions to
%% TODO: match expressions.
%% TODO: Later, I need to add calls to functions to be externalized so that
%% TODO: values are bound to variables, which is called a-normal form.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Rewrites an Erlang abstract syntax representation to its equivalent Erlang
A-normal form representation.
""".
-spec module(erl_syntax:forms()) -> erl_syntax:forms().
module(Forms) ->
  erl_syntax:revert_forms(forms(Forms)).


%%% ----------------------------------------------------------------------------
%%% Rewriting on forms and types.
%%% ----------------------------------------------------------------------------

-doc "Rewrites a list of Erlang forms.".
-spec forms(erl_syntax:forms()) -> erl_syntax:forms().
forms(Forms) ->
  [form(Form) || Form <- Forms].

-doc "Rewrites an Erlang function.".
-spec form(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
form({function, Anno, Name, _, Clauses}) ->
  erl_syntax:set_pos(
    erl_syntax:function(erl_syntax:atom(Name), fun_clauses(Clauses)),
    Anno
  );
form(Form) ->
  % Other Erlang forms.
  Form.

-doc "Generic function to rewrite a list of Erlang clauses.".
-spec clauses(function(), [erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  [Fun(Clause) || Clause <- Clauses].

-doc "Rewrites a list of Erlang function clauses.".
-spec fun_clauses([erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
fun_clauses(Clauses) ->
  clauses(fun fun_clause/1, Clauses).

-doc "Rewrites a list of Erlang case and receive clauses.".
-spec case_clauses([erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
case_clauses(Clauses) ->
  clauses(fun case_clause/1, Clauses).

-doc "rewrites a list of Erlang clauses.".
-spec if_clauses([erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
if_clauses(Clauses) ->
  clauses(fun if_clause/1, Clauses).

-doc "Rewrites an Erlang function clause.".
-spec fun_clause(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
fun_clause({clause, Anno, PatSeq, GuardSeq, Body}) ->
  erl_syntax:set_pos(erl_syntax:clause(PatSeq, GuardSeq, expr_seq(Body)), Anno).

-doc "Rewrites an Erlang case or receive clause.".
-spec case_clause(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
case_clause({clause, Anno, [Pat], GuardSeq, Body}) ->
  % Receive and case clause.
  erl_syntax:set_pos(erl_syntax:clause([Pat], GuardSeq, expr_seq(Body)), Anno).

-doc "Rewrites an Erlang if clause".
-spec if_clause(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
if_clause({clause, Anno, [], GuardSeq, Body}) ->
  % If clause.
  erl_syntax:set_pos(erl_syntax:clause(GuardSeq, expr_seq(Body)), Anno).

-doc "Rewrites an Erlang expression sequence.".
-spec expr_seq([erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
expr_seq([]) ->
  [];
expr_seq([Expr]) ->
  % Singleton expression sequence where the expression is a value.
  [expr(Expr, false)];
expr_seq([Expr | ExprSeq]) ->
  [expr(Expr, true) | expr_seq(ExprSeq)].

-doc """
Rewrites an Erlang expression into single static assignment form.

- `RewriteNode`: rewrites the expression to a match expression when
`RewriteNode` is `true`, otherwise does not rewrite the expression but rewrites
its body.
""".
-spec expr(Expr :: erl_syntax:syntaxTree(), RewriteNode :: boolean()) -> erl_syntax:syntaxTree().
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
expr(Expr, true) when ?isStaticCall(Expr) ->
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
expr(Expr, false) when ?isStaticCall(Expr) ->
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

-doc "Returns a fresh variable name.".
-spec fresh_var() -> paterl_tools:name().
fresh_var() ->
  paterl_tools:fresh_var(?TEMP_VAR_NAME).


%%% ----------------------------------------------------------------------------
%%% Inline tests.
%%% ----------------------------------------------------------------------------

-ifdef(test).

%% Tests the expression category macros.
test_expr_cat(Expr) when ?isMbAnno(Expr) ->
  io:format("Is MB annotation ~p.~n", [Expr]);
test_expr_cat(Expr) when ?isMsg(Expr) ->
  io:format("Is message ~p.~n", [Expr]);
test_expr_cat(Expr) when ?isStaticCall(Expr) ->
  io:format("Is implicit call ~p.~n", [Expr]);
test_expr_cat(Expr) when ?isDynamicCall(Expr) ->
  io:format("Is explicit call ~p.~n", [Expr]);
test_expr_cat(Expr) ->
  io:format("Unrecognized expression ~p.~n", [Expr]).

-endif.