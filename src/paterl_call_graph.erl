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
-export([]).
-compile(export_all).

%%% Public types.
-export_type([rec_fun_info/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%-define(call_info, {
%%  call_graph = #{},
%%  mb_scopes = []
%%}).


%%-record(state, {
%%  % Unique index counter. Incremented sequentially as nodes are visited.
%%  index = 0 :: integer(),
%%
%%  % Stack that tracks the current path in the DFS. Nodes are pushed on the stack
%%  % when they are first visited and remain there until their SCC is fully
%%  % identified.
%%  stack = [] :: [node()],
%%
%%  % Associates a node with its unique index. Index indicates the order in which
%%  % the node was first visited.
%%  indices = #{} :: #{node() => integer()},
%%
%%  % Associates the smallest index reachable from each node, including itself.
%%  % Used to determine whether a node is at the root of an SCC.
%%  low_links = #{} :: #{node() => integer()},
%%
%%  % Stores the SCCs identified so far. Each SSC is represented as a node list.
%%  sccs = [],
%%
%%  % Associates a node with a flag that tracks whether that node is currently on
%%  % the stack. This prevents revisiting nodes that are already in the current
%%  % DFS path, which would otherwise pollute the discovered SCC.
%%  on_stack = #{} :: #{node() => boolean()}
%%}).

%%-record(rec_fun_info, {
%%  fun_refs = [] :: [paterl_syntax:get_fun_ref()],
%%
%%}).

%%% Error types.

%% Unsupported expression.
-define(E_BAD__EXPR, e_bad__expr).

%% Unsupported function clause.
-define(E_BAD__CLAUSE, e_bad__clause).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%-type node() :: term().
%%
%%-type graph() :: #{node() => [node()]}.
%%
%%-type state() :: #state{}.

-type call_graph() :: #{paterl_syntax:fun_ref() => [paterl_syntax:fun_ref()]}.

-type rec_fun_info() :: #{paterl_syntax:fun_ref() => [paterl_syntax:fun_ref()]}.

-doc "Return result.".
-type result() :: {ok, CallGraph :: call_graph(), Warnings :: paterl_errors:warnings()} |
{error, Errors :: paterl_errors:errors(), Warnings :: paterl_errors:warnings()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------
-spec module(paterl_syntax:forms()) -> result().
module(Forms) when is_list(Forms) ->
  Analysis = analyze_forms(Forms, #analysis{result = #{}}),
  paterl_lib:return(Analysis#analysis{file = paterl_syntax:get_file(Forms)}).

-spec rec_funs(CallGraph) -> RecFunInfo
  when
  CallGraph :: call_graph(),
  RecFunInfo :: rec_fun_info().
rec_funs(CallGraph) ->
  ?TRACE("CallGraph: ~n~p", [CallGraph]),

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

analyze_function({function, Anno, Name, Arity, Clauses}, Analysis) ->
  % Recover fun reference.
  FunRef = {Name, Arity},
  ?DEBUG("Analyze function '~s'.", [
    erl_prettypr:format(paterl_syntax:fun_reference(FunRef, Anno))
  ]),

  CallGraph = Analysis#analysis.result,
  analyze_fun_clauses(Clauses, FunRef, Analysis#analysis{result = CallGraph#{FunRef => []}}).

analyze_fun_clauses([], _, Analysis) ->
  Analysis;
analyze_fun_clauses([Clause | Clauses], FunRef, Analysis) ->
  Analysis0 = analyze_fun_clause(Clause, FunRef, Analysis),
  analyze_fun_clauses(Clauses, FunRef, Analysis0).

analyze_fun_clause({clause, _, _PatSeq, _GuardSeq = [], Body}, FunRef, Analysis)
  when is_list(Body) ->
  % Unguarded function clause.
  ?DEBUG("Analyze unguarded function clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(_PatSeq, _GuardSeq, []))
  ]),

  % Annotate function body.
  analyze_expr_seq(Body, FunRef, Analysis);
analyze_fun_clause({clause, Anno, PatSeq, GuardSeq, _}, FunRef, Analysis) ->
  % Guarded function clause. Unsupported.
  ErrNode = paterl_syntax:set_anno(erl_syntax:clause(PatSeq, GuardSeq, []), Anno),
  ?ERROR("Unsupported guarded function clause '~s'.", [
    erl_prettypr:format(ErrNode)
  ]),
  ?pushError(?E_BAD__CLAUSE, ErrNode, Analysis#analysis{result = ErrNode}).


analyze_clauses([], FunRef, Analysis) ->
  Analysis;
analyze_clauses([Clause | Clauses], FunRef, Analysis) ->
  Analysis0 = annotate_clause(Clause, FunRef, Analysis),
  analyze_clauses(Clauses, FunRef, Analysis0).


annotate_clause(Clause = {clause, Anno, _PatSeq = [], GuardSeq, Body},
    FunRef, Analysis)
  when is_list(GuardSeq), is_list(Body) ->
  % If clause.
%%  ?TRACE("Analyze clause '~s'.", [erl_syntax:type(Clause)]),
  ?TRACE("Analyze clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(_PatSeq, GuardSeq, []))
  ]),
  analyze_expr_seq(Body, FunRef, Analysis);
annotate_clause(Clause = {clause, Anno, PatSeq = [_], GuardSeq = [], Body}, FunRef, Analysis) ->
  % Receive or case clause
%%  ?TRACE("Analyze clause '~s'.", [erl_syntax:type(Clause)]),
  ?TRACE("Analyze clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(PatSeq, GuardSeq, []))
  ]),
  analyze_expr_seq(Body, FunRef, Analysis);
annotate_clause({clause, _, _PatSeq, _GuardSeq, _}, _, Analysis) ->
%%  ?TRACE("Skip clause '~s'.", [erl_syntax:type(Clause)]),
  ?TRACE("Skip clause '~s'.", [
    erl_prettypr:format(erl_syntax:clause(_PatSeq, _GuardSeq, []))
  ]),
  Analysis.

analyze_expr_seq([], FunRef, Analysis) ->
  % Empty expression sequence.
  Analysis;
analyze_expr_seq([Expr | ExprSeq], FunRef, Analysis) ->
  % Expression.
  Analysis0 = analyze_expr(Expr, FunRef, Analysis),
  analyze_expr_seq(ExprSeq, FunRef, Analysis0).


analyze_expr({call, Anno, Operator = {atom, _, Fun}, Exprs}, FunRef, Analysis)
  when
  Fun =:= format;
  Fun =:= uniform;
  Fun =:= system_time;
  Fun =:= sleep;
  Fun =:= self;
  Fun =:= spawn ->
  ?TRACE("Skip BIF '~p'.", [Fun]),
  Analysis;

analyze_expr(Expr = {call, Anno, Operator, Exprs}, FunRef, Analysis) ->
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

analyze_expr(Expr = {'receive', _, Clauses}, FunRef, Analysis) ->
  % Receive expression.
  ?DEBUG("Analyze '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),
%%  annotate_expr(Expr, FunRef, Analysis);
  analyze_clauses(Clauses, FunRef, Analysis);

analyze_expr(_Expr = {'if', Anno, Clauses}, FunRef, Analysis) ->
  % Unannotated if expression.
  ?DEBUG("Analyze '~s'.", [erl_prettypr:format(erl_syntax:if_expr([]))]),
  analyze_clauses(Clauses, FunRef, Analysis);

analyze_expr(Expr0 = {match, Anno, Pat, Expr1}, FunRef, Analysis) ->
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

      % Annotate expression to uncover further possible errors.
      analyze_expr(Expr1, FunRef, Analysis0)
  end;
analyze_expr(Expr, FunRef, Analysis) ->
  % Skip other expressions.
  ?TRACE("Skip '~s'.", [erl_prettypr:format(Expr)]),
  Analysis.


%%% ----------------------------------------------------------------------------
%%% Tarjan's SCC algorithm.
%%% ----------------------------------------------------------------------------

%%-doc """
%%Computes the SCCs of the specifed `Graph`.
%%
%%### Returns
%%- list of SCCs
%%""".
%%-spec find_sccs(Graph :: graph()) -> SCCs :: [[node()]].
%%find_sccs(Graph) ->
%%%%  State = #{index => 0,
%%%%    stack => [],
%%%%    indices => #{},
%%%%    lowlinks => #{},
%%%%    sccs => [],
%%%%    on_stack => #{}},
%%%%  FinalState =
%%%%    lists:foldl(
%%%%      fun(Node, S) ->
%%%%        case maps:is_key(Node, maps:get(indices, S, #{})) of
%%%%          true -> S;  % Node already processed
%%%%          false -> strong_connect(Node, Graph, S)
%%%%        end
%%%%      end, #state{}, maps:keys(Graph)),
%%%%  maps:get(sccs, FinalState),
%%
%%  Fun =
%%    fun(Node, St = #state{indices = Indices}) ->
%%      % Check if current node has been visited. A node is considered visited
%%      % if it has a corresponding entry in the `indices` map.
%%      case maps:is_key(Node, Indices) of
%%        true ->
%%          % Node already visited. Skip and return unchanged state.
%%          St;
%%        false ->
%%          % Node unvisited. Initiate DFS from node to discover all SCCs
%%          % reachable from it.
%%          scc_dfs(Node, Graph, St)
%%      end
%%    end,
%%
%%  % Traverse each node in the graph.
%%  FinalState = lists:foldl(Fun, #state{}, maps:keys(Graph)),
%%  FinalState#state.sccs.
%%
%%-doc """
%%Performs Tarjan's algorithm for the specified `Node`.
%%
%%### Returns
%%- Updated [`state()`](`t:state`).
%%""".
%%-spec scc_dfs(Node, Graph, State) -> State0
%%  when
%%  Node :: node(),
%%  Graph :: graph(),
%%  State :: state(),
%%  State0 :: state().
%%scc_dfs(Node, Graph, State = #state{index = Index}) ->
%%%%  Index = maps:get(index, State),
%%%%  State1 = State#{index => Index + 1,
%%%%    indices => maps:put(Node, Index, maps:get(indices, State)),
%%%%    lowlinks => maps:put(Node, Index, maps:get(lowlinks, State)),
%%%%    stack => [Node | maps:get(stack, State)],
%%%%    on_stack => maps:put(Node, true, maps:get(on_stack, State))},
%%
%%  State1 = State#state{
%%    % Increment global index counter for the next node.
%%    index = Index + 1,
%%
%%    % Assign current node a unique index in the DFS traversal.
%%    indices = maps:put(Node, Index, State#state.indices),
%%
%%    % Initialize low_link of current node to its index.
%%    low_links = maps:put(Node, Index, State#state.low_links),
%%
%%    % Push current node onto stack to keep track of DFS path.
%%    stack = [Node | State#state.stack],
%%
%%    % Mark current node as on stack.
%%    on_stack = maps:put(Node, true, State#state.on_stack)
%%  },
%%
%%  %% Process each neighbour
%%%%  State2 = lists:foldl(
%%%%    fun(Neighbour, S) ->
%%%%      case maps:is_key(Neighbour, maps:get(indices, S, #{})) of
%%%%        false ->  % Neighbour has not been visited
%%%%          S1 = strong_connect(Neighbour, Graph, S),
%%%%          update_lowlink(Node, Neighbour, S1);
%%%%        true ->
%%%%          case maps:get(Neighbour, maps:get(on_stack, S, #{})) of
%%%%            true -> update_lowlink(Node, Neighbour, S);  % Back edge
%%%%            false -> S  % Ignore
%%%%          end
%%%%      end
%%%%    end, State1, maps:get(Node, Graph, [])),
%%
%%
%%  % Processes all neighbours of the current node.
%%  Fun =
%%    fun(Neighbor, State0) ->
%%      % Check if neighbor is visited.
%%      case maps:is_key(Neighbor, State0#state.indices) of
%%        false ->
%%          % Neighbor not yet visited. Perform a DFS on the unvisited neighbor.
%%          StateX = scc_dfs(Neighbor, Graph, State0),
%%
%%          % Update low link of current node based on neighbor.
%%          update_low_link(Node, Neighbor, StateX);
%%        true ->
%%          % Neighbor already visited. Check if neighbor is still on the stack
%%          % (part of the current DFS path).
%%          case maps:get(Neighbor, State0#state.on_stack) of
%%            true ->
%%              % Neighbor is on the stack indicating a back edge. Update low link
%%              % of current node based on the neighbor.
%%              update_low_link(Node, Neighbor, State0);
%%            false ->
%%              % Neighbor is not on the stack and its already part of a finished
%%              % SCC. Skip.
%%              State0
%%          end
%%      end
%%    end,
%%
%%  % Start with updated state and process all neighbours of the current node.
%%  Neighbors = maps:get(Node, Graph),
%%  State2 = lists:foldl(Fun, State1, Neighbors),
%%
%%  %% If Node is a root node, form an SCC
%%%%  case maps:get(Node, maps:get(lowlinks, State2)) == maps:get(Node, maps:get(indices, State2)) of
%%%%    true ->
%%%%      {NewSCC, NewStack, NewOnStack} = pop_scc(Node, maps:get(stack, State2), maps:get(on_stack, State2)),
%%%%      State2#{sccs => [NewSCC | maps:get(sccs, State2)],
%%%%        stack => NewStack,
%%%%        on_stack => NewOnStack};
%%%%    false -> State2
%%%%  end,
%%
%%  % Check if current node is at root of SCC.
%%  case is_scc_root(Node, State2) of
%%    true ->
%%      % Node is at root of SCC. Pop all nodes in the current SSC from the stack.
%%      {NewSCC, NewStack, NewOnStack} = pop_scc(Node, State2#state.stack, State2#state.on_stack),
%%
%%      % Update state with newly discovered SCC.
%%      State2#state{
%%        sccs = [NewSCC | State2#state.sccs],
%%        stack = NewStack,
%%        on_stack = NewOnStack
%%      };
%%    false ->
%%      % Node is not at root of SCC. Return current state without forming a new
%%      % SCC.
%%      State2
%%  end.
%%
%%-doc """
%%Checks if the current node is the root of the SCC.
%%
%%### Returns
%%- `true` if `Node` is the roof of the SCC
%%- `false` otherwise
%%""".
%%-spec is_scc_root(Node :: node(), State :: state()) -> boolean().
%%is_scc_root(Node, #state{low_links = LowLinks, indices = Indices}) ->
%%  % A node is the root of an SCC if its index equals its low link.
%%  maps:get(Node, LowLinks) == maps:get(Node, Indices).
%%
%%-doc """
%%Updates lowlink of node based on neighbor.
%%
%%### Returns
%%- updated [`state()`](`t:state/0`).
%%""".
%%-spec update_low_link(Node, Neighbor, State) -> State0
%%  when
%%  Node :: node(),
%%  Neighbor :: node(),
%%  State :: state(),
%%  State0 :: state().
%%update_low_link(Node, Neighbor, State = #state{low_links = LowLinks}) ->
%%%%  LowLinks = maps:get(lowlinks, State),
%%
%%
%%  % Calculate new low link for current node. The low link of a node is the
%%  % smallest index that the node or any of its reachable neighbors can access.
%%  NewLowLink = min(
%%    maps:get(Node, LowLinks), % Node low link.
%%    maps:get(Neighbor, LowLinks) % Neighbor low link.
%%  ),
%%%%  State#{lowlinks => maps:put(Node, NewLowlink, LowLinks)}.
%%
%%  % Update low link of current node.
%%  State#state{low_links = maps:put(Node, NewLowLink, LowLinks)}.
%%
%%-doc """
%%Pops an SCC from the stack.
%%
%%### Returns
%%- New SCC, updated stack, and on stack status.
%%""".
%%-spec pop_scc(SccRoot, Stack, OnStack) -> {SCC, Stack0, OnStack0}
%%  when
%%  SccRoot :: node(),
%%  Stack :: [node()],
%%  OnStack :: #{node() => boolean()},
%%  SCC :: [node()],
%%  Stack0 :: [node()],
%%  OnStack0 :: #{node() => boolean()}.
%%pop_scc(SccRoot, Stack, OnStack) ->
%%  % Pop nodes from stack to form an SCC until the root node of the SCC is
%%  % reached.
%%  pop_scc(SccRoot, Stack, OnStack, []).
%%
%%-doc """
%%Pops nodes from the stack until an SCC is reached.
%%
%%### Returns
%%- New SCC, updated stack, and on stack status.
%%""".
%%-spec pop_scc(SccRoot, Stack, OnStack, SCC) -> {SCC0, Stack0, OnStack0}
%%  when
%%  SccRoot :: node(),
%%  Stack :: [node()],
%%  OnStack :: #{node() => boolean()},
%%  SCC :: [node()],
%%  SCC0 :: [node()],
%%  Stack0 :: [node()],
%%  OnStack0 :: #{node() => boolean()}.
%%pop_scc(SccRoot, [Top | Nodes], OnStack, SCC) ->
%%  % Mark current stack node as removed from the stack.
%%  NewOnStack = maps:put(Top, false, OnStack),
%%
%%  % Add current stack node to SCC being formed.
%%  NewSCC = [Top | SCC],
%%
%%  % Check if top of stack is the root node of the SCC (i.e., the starting node
%%  % of the current DFS path).
%%  case Top == SccRoot of
%%    true ->
%%      % SCC root reached. Return new SCC, updated stack, and updated on stack
%%      % status.
%%      {NewSCC, Nodes, NewOnStack};
%%    false ->
%%      % SCC root not reached. Continue popping nodes from stack until SCC root
%%      % is reached.
%%      pop_scc(SccRoot, Nodes, NewOnStack, NewSCC)
%%  end.

