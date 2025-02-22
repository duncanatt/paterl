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
-module(paterl_scc).
-moduledoc """
Strongly-connected components computation using Tarjan's algorithm.
""".
-author("duncan").

%%% Includes.
-include("log.hrl").

%%% Public API.
-export([find_sccs/1]).

%%% Public types.
%%-export_type([graph/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Defines the state maintained by Tarjan's algorithm.
-record(state, {
  % Unique index counter. Incremented sequentially as nodes are visited.
  index = 0 :: integer(),

  % Stack that tracks the current path in the DFS. Nodes are pushed on the stack
  % when they are first visited and remain there until their SCC is fully
  % identified.
  stack = [] :: [node()],

  % Associates a node with its unique index. Index indicates the order in which
  % the node was first visited.
  indices = #{} :: #{node() => integer()},

  % Associates the smallest index reachable from each node, including itself.
  % Used to determine whether a node is at the root of an SCC.
  low_links = #{} :: #{node() => integer()},

  % Stores the SCCs identified so far. Each SSC is represented as a node list.
  sccs = [],

  % Associates a node with a flag that tracks whether that node is currently on
  % the stack. This prevents revisiting nodes that are already in the current
  % DFS path, which would otherwise pollute the discovered SCC.
  on_stack = #{} :: #{node() => boolean()}
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type node() :: term().

-type graph() :: #{node() => [node()]}.

-type state() :: #state{}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Computes the SCCs of the specifed `Graph`.

### Returns
- list of SCCs
""".
-spec find_sccs(Graph :: graph()) -> SCCs :: [[node()]].
find_sccs(Graph) ->
  Fun =
    fun(Node, St = #state{indices = Indices}) ->
      % Check if current node has been visited. A node is considered visited
      % if it has a corresponding entry in the `indices` map.
      case maps:is_key(Node, Indices) of
        true ->
          % Node already visited. Skip and return unchanged state.
          St;
        false ->
          % Node unvisited. Initiate DFS from node to discover all SCCs
          % reachable from it.
          scc_dfs(Node, Graph, St)
      end
    end,

  % Traverse each node in the graph.
  FinalState = lists:foldl(Fun, #state{}, maps:keys(Graph)),
  FinalState#state.sccs.

-doc """
Performs Tarjan's algorithm for the specified `Node`.

### Returns
- Updated [`state()`](`t:state`).
""".
-spec scc_dfs(Node, Graph, State) -> State0
  when
  Node :: node(),
  Graph :: graph(),
  State :: state(),
  State0 :: state().
scc_dfs(Node, Graph, State = #state{index = Index}) ->
  State1 = State#state{
    % Increment global index counter for the next node.
    index = Index + 1,

    % Assign current node a unique index in the DFS traversal.
    indices = maps:put(Node, Index, State#state.indices),

    % Initialize low_link of current node to its index.
    low_links = maps:put(Node, Index, State#state.low_links),

    % Push current node onto stack to keep track of DFS path.
    stack = [Node | State#state.stack],

    % Mark current node as on stack.
    on_stack = maps:put(Node, true, State#state.on_stack)
  },

  % Processes all neighbours of the current node.
  Fun =
    fun(Neighbor, State0) ->
      % Check if neighbor is visited.
      case maps:is_key(Neighbor, State0#state.indices) of
        false ->
          % Neighbor not yet visited. Perform a DFS on the unvisited neighbor.
          StateX = scc_dfs(Neighbor, Graph, State0),

          % Update low link of current node based on neighbor.
          update_low_link(Node, Neighbor, StateX);
        true ->
          % Neighbor already visited. Check if neighbor is still on the stack
          % (part of the current DFS path).
          case maps:get(Neighbor, State0#state.on_stack) of
            true ->
              % Neighbor is on the stack indicating a back edge. Update low link
              % of current node based on the neighbor.
              update_low_link(Node, Neighbor, State0);
            false ->
              % Neighbor is not on the stack and its already part of a finished
              % SCC. Skip.
              State0
          end
      end
    end,

  % Start with updated state and process all neighbours of the current node.
  Neighbors = maps:get(Node, Graph),
  State2 = lists:foldl(Fun, State1, Neighbors),

  % Check if current node is at root of SCC.
  case is_scc_root(Node, State2) of
    true ->
      % Node is at root of SCC. Pop all nodes in the current SSC from the stack.
      {NewSCC, NewStack, NewOnStack} = pop_scc(Node, State2#state.stack, State2#state.on_stack),

      % Update state with newly discovered SCC.
      State2#state{
        sccs = [NewSCC | State2#state.sccs],
        stack = NewStack,
        on_stack = NewOnStack
      };
    false ->
      % Node is not at root of SCC. Return current state without forming a new
      % SCC.
      State2
  end.

-doc """
Checks if the current node is the root of the SCC.

### Returns
- `true` if `Node` is the roof of the SCC
- `false` otherwise
""".
-spec is_scc_root(Node :: node(), State :: state()) -> boolean().
is_scc_root(Node, #state{low_links = LowLinks, indices = Indices}) ->
  % A node is the root of an SCC if its index equals its low link.
  maps:get(Node, LowLinks) == maps:get(Node, Indices).

-doc """
Updates lowlink of node based on neighbor.

### Returns
- updated [`state()`](`t:state/0`).
""".
-spec update_low_link(Node, Neighbor, State) -> State0
  when
  Node :: node(),
  Neighbor :: node(),
  State :: state(),
  State0 :: state().
update_low_link(Node, Neighbor, State = #state{low_links = LowLinks}) ->
  % Calculate new low link for current node. The low link of a node is the
  % smallest index that the node or any of its reachable neighbors can access.
  NewLowLink = min(
    maps:get(Node, LowLinks), % Node low link.
    maps:get(Neighbor, LowLinks) % Neighbor low link.
  ),

  % Update low link of current node.
  State#state{low_links = maps:put(Node, NewLowLink, LowLinks)}.

-doc """
Pops an SCC from the stack.

### Returns
- New SCC, updated stack, and on stack status.
""".
-spec pop_scc(SccRoot, Stack, OnStack) -> {SCC, Stack0, OnStack0}
  when
  SccRoot :: node(),
  Stack :: [node()],
  OnStack :: #{node() => boolean()},
  SCC :: [node()],
  Stack0 :: [node()],
  OnStack0 :: #{node() => boolean()}.
pop_scc(SccRoot, Stack, OnStack) ->
  % Pop nodes from stack to form an SCC until the root node of the SCC is
  % reached.
  pop_scc(SccRoot, Stack, OnStack, []).

-doc """
Pops nodes from the stack until an SCC is reached.

### Returns
- New SCC, updated stack, and on stack status.
""".
-spec pop_scc(SccRoot, Stack, OnStack, SCC) -> {SCC0, Stack0, OnStack0}
  when
  SccRoot :: node(),
  Stack :: [node()],
  OnStack :: #{node() => boolean()},
  SCC :: [node()],
  SCC0 :: [node()],
  Stack0 :: [node()],
  OnStack0 :: #{node() => boolean()}.
pop_scc(SccRoot, [Top | Nodes], OnStack, SCC) ->
  % Mark current stack node as removed from the stack.
  NewOnStack = maps:put(Top, false, OnStack),

  % Add current stack node to SCC being formed.
  NewSCC = [Top | SCC],

  % Check if top of stack is the root node of the SCC (i.e., the starting node
  % of the current DFS path).
  case Top == SccRoot of
    true ->
      % SCC root reached. Return new SCC, updated stack, and updated on stack
      % status.
      {NewSCC, Nodes, NewOnStack};
    false ->
      % SCC root not reached. Continue popping nodes from stack until SCC root
      % is reached.
      pop_scc(SccRoot, Nodes, NewOnStack, NewSCC)
  end.
