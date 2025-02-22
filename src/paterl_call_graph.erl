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
%%%
-module(paterl_call_graph).
-moduledoc "Mutually-recursive function and type defintion detection.".
-author("duncan").

%%% Includes.
-include("log.hrl").
-include("paterl_lib.hrl").
-include("paterl_syntax.hrl").

%%% Public API.
-export([module/1, rec_funs/1]).

%%% Public types.
-export_type([rec_fun_info/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Error types.

%% Unsupported expression.
-define(E_BAD__EXPR, e_bad__expr).

%% Unsupported function clause.
-define(E_BAD__CLAUSE, e_bad__clause).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Function call graph.".
-type call_graph() :: #{paterl_syntax:fun_ref() => [paterl_syntax:fun_ref()]}.

-doc "Direct and mutal recursion function information.".
-type rec_fun_info() :: #{paterl_syntax:fun_ref() => [paterl_syntax:fun_ref()]}.

-doc "Return result.".
-type result() :: {ok, CallGraph :: call_graph(), Warnings :: paterl_errors:warnings()} |
{error, Errors :: paterl_errors:errors(), Warnings :: paterl_errors:warnings()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Computes the function call graph of the specified list of `Forms`.

### Returns
- function call graph
""".
-spec module(paterl_syntax:forms()) -> result().
module(Forms) when is_list(Forms) ->
  Analysis = analyze_forms(Forms, #analysis{result = #{}}),
  paterl_lib:return(Analysis#analysis{file = paterl_syntax:get_file(Forms)}).

-doc """
Computes the SCCs of the specified `CallGraph` to determine the direct and
mutually recursive functions.

### Returns
- function recursion information
""".
-spec rec_funs(CallGraph) -> RecFunInfo
  when
  CallGraph :: call_graph(),
  RecFunInfo :: rec_fun_info().
rec_funs(CallGraph) ->
  % Compute SCCs to determine direct and mutual recursive functions.
  SCCs = paterl_scc:find_sccs(CallGraph),
  ?TRACE("SCCs: ~n~p", [SCCs]),

  % Maps each fun reference inside a SCC to point to that SCC.
  Fun =
    fun(SCC, RecFuns) ->
      lists:foldl(
        fun(FunRef, RecFuns0) -> RecFuns0#{FunRef => SCC} end, RecFuns, SCC
      )
    end,

  % Build recursive fun references map.
  lists:foldl(Fun, _RecFuns = #{}, SCCs).


%%% ----------------------------------------------------------------------------
%%% Analysis.
%%% ----------------------------------------------------------------------------

-doc "Analyzed a list of forms.".
analyze_forms([], Analysis) ->
  % Empty forms.
  Analysis;
analyze_forms([Form = {function, _, _, _, _} | Forms], Analysis) ->
  % Function.
  Analysis0 = analyze_function(Form, Analysis),
  analyze_forms(Forms, Analysis0);
analyze_forms([_ | Forms], Analysis) ->
  % Skip other forms.
  analyze_forms(Forms, Analysis).

-doc "Analyzes a function.".
analyze_function({function, Anno, Name, Arity, Clauses}, Analysis) ->
  % Recover fun reference.
  FunRef = {Name, Arity},
  ?DEBUG("Analyze function '~s'.", [
    erl_prettypr:format(paterl_syntax:fun_reference(FunRef, Anno))
  ]),

  CallGraph = Analysis#analysis.result,
  analyze_fun_clauses(Clauses, FunRef, Analysis#analysis{result = CallGraph#{FunRef => []}}).

-doc "Analyzes a list of function clauses.".
analyze_fun_clauses(Clauses, FunRef, Analysis) ->
  lists:foldl(
    fun(Clause, Analysis0) -> analyze_fun_clause(Clause, FunRef, Analysis0) end,
    Analysis, Clauses
  ).

-doc "Analyzes a function clause.".
analyze_fun_clause({clause, _, _PatSeq, _GuardSeq = [], Body}, FunRef, Analysis)
  when is_list(Body) ->
  % Unguarded function clause.
  ?DEBUG("Analyze unguarded function clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(_PatSeq, _GuardSeq, []))
  ]),

  % Analyze function body.
  analyze_expr_seq(Body, FunRef, Analysis);
analyze_fun_clause({clause, Anno, PatSeq, GuardSeq, _}, _, Analysis) ->
  % Guarded function clause. Unsupported.
  ErrNode = paterl_syntax:set_anno(erl_syntax:clause(PatSeq, GuardSeq, []), Anno),
  ?ERROR("Unsupported guarded function clause '~s'.", [
    erl_prettypr:format(ErrNode)
  ]),
  ?pushError(?E_BAD__CLAUSE, ErrNode, Analysis#analysis{result = ErrNode}).

-doc "Analyzes a list of clauses.".
analyze_clauses(Clauses, FunRef, Analysis) ->
  lists:foldl(
    fun(Clause, Analysis0) -> analyze_clause(Clause, FunRef, Analysis0) end,
    Analysis, Clauses
  ).

-doc "Analyses a clause.".
analyze_clause({clause, _, _PatSeq = [], GuardSeq, Body},
    FunRef, Analysis)
  when is_list(GuardSeq), is_list(Body) ->
  % If clause.
  ?TRACE("Analyze clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(_PatSeq, GuardSeq, []))
  ]),
  analyze_expr_seq(Body, FunRef, Analysis);
analyze_clause({clause, _, PatSeq = [_], GuardSeq = [], Body}, FunRef, Analysis) ->
  % Receive or case clause
  ?TRACE("Analyze clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(PatSeq, GuardSeq, []))
  ]),
  analyze_expr_seq(Body, FunRef, Analysis);
analyze_clause({clause, _, _PatSeq, _GuardSeq, _}, _, Analysis) ->
  ?TRACE("Skip clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(_PatSeq, _GuardSeq, []))
  ]),
  Analysis.

-doc "Analyzes an expression sequence.".
analyze_expr_seq(ExprSeq, FunRef, Analysis) ->
  lists:foldl(
    fun(Expr, Analysis0) -> analyze_expr(Expr, FunRef, Analysis0) end,
    Analysis, ExprSeq
  ).

-doc "Analyzes an expression.".
analyze_expr({call, _, _Operator = {atom, _, Fun}, _Exprs}, _, Analysis)
  when
  Fun =:= format;
  Fun =:= uniform;
  Fun =:= system_time;
  Fun =:= sleep;
  Fun =:= self;
  Fun =:= spawn ->
  ?TRACE("Skip BIF '~p'.", [Fun]),
  Analysis;
analyze_expr(Expr = {call, _, _Operator, _Exprs}, FunRef, Analysis) ->
  ?TRACE("Add function call '~s'.", [erl_prettypr:format(Expr)]),
  % Recover fun reference.
  case paterl_syntax:get_fun_ref(Expr) of
    {ok, FunRefCall = {_, _}} ->
      % Static function call.
      ?TRACE("FunRefCall = ~p", [FunRefCall]),

      CallGraph = Analysis#analysis.result,
      FunRefCalls = maps:get(FunRef, CallGraph),
      Analysis#analysis{result = CallGraph#{FunRef => [FunRefCall | FunRefCalls]}};
    {error, Term} ->
      % Dynamic function call. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Term), erl_prettypr:format(Expr)
      ]),
      ?pushError(?E_BAD__EXPR, Term, Analysis#analysis{result = Expr})
  end;
analyze_expr({'receive', _, Clauses}, FunRef, Analysis) ->
  % Receive expression.
  ?DEBUG("Analyze '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),
  analyze_clauses(Clauses, FunRef, Analysis);
analyze_expr(_Expr = {'if', _, Clauses}, FunRef, Analysis) ->
  % If expression.
  ?DEBUG("Analyze '~s'.", [erl_prettypr:format(erl_syntax:if_expr([]))]),
  analyze_clauses(Clauses, FunRef, Analysis);
analyze_expr(Expr0 = {match, _, Pat, Expr1}, FunRef, Analysis) ->
  % Match expression.
  case erl_syntax:type(Pat) of
    variable ->
      % Match pattern is a variable.
      ?DEBUG("Analyze '~s'.", [
        erl_prettypr:format(erl_syntax:match_expr(Pat, erl_syntax:underscore()))
      ]),

      analyze_expr(Expr1, FunRef, Analysis);
    _ ->
      % Match pattern not a variable. Invalid.
      ?ERROR("Unsupported expression '~s' in '~s'.", [
        erl_prettypr:format(Pat),
        erl_prettypr:format(erl_syntax:match_expr(Pat, erl_syntax:underscore()))
      ]),
      Analysis0 = ?pushError(?E_BAD__EXPR, Expr0, Analysis),

      % Analyze expression to uncover further possible errors.
      analyze_expr(Expr1, FunRef, Analysis0)
  end;
analyze_expr(Expr, _, Analysis) ->
  % Skip other expressions.
  ?TRACE("Skip analyze '~s'.", [erl_prettypr:format(Expr)]),
  Analysis.