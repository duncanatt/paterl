%%%
%%% %CopyrightBegin%
%%%
%%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
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
-module(paterl_anno_tests).
-moduledoc "`paterl_anno` module tests.".
-author("duncan").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("paterl.hrl").
-include_lib("paterl_lib.hrl").
-include_lib("paterl_syntax.hrl").


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%-doc "Extended test.".
%%-type test_x() :: paterl_test_lib:pair(paterl_test_lib:erl(), paterl_syntax:result()).


%%% ----------------------------------------------------------------------------
%%% Mailbox interface type expansion tests.
%%% ----------------------------------------------------------------------------

-doc "Mailbox interface type expansion tests.".
mb_type_expand_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun no_mb/0,
      fun mb_pid_type/0,
      fun mb_inline_empty_msg_type/0,
      fun mb_inline_primitive_msg_type/0,
      fun mb_inline_mb_msg_type/0,
      fun mb_type_union/0,
      fun mb_msg_type/0,
      fun mb_msg_all_type/0
    ]]}.

-doc """
Tests no mailbox interface definitions.

> Should return `ok`.
""".
no_mb() -> {
  """
  -module(test).
  -type mb() :: pid().
  """,
  fun(_, Result) ->
    {"Test no mailbox interface definitions",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % No mailbox interface definitions.
        ?assertMatch([
          {attribute, _, module, test}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with Pid (an empty message set).

> Should return `ok`.
""".
mb_pid_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: pid().
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with Pid (an empty message set)",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with Pid (an empty message set).
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with empty inline message (a singleton message
set).

> Should return `ok`.
""".
mb_inline_empty_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg}.
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with empty inline message (a singleton message set)",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with empty inline message (a singleton message
        % set).
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface,
            {mb, {type, _, tuple, [{atom, _, msg}]}, []}}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with inline message with built-in primitive type
(a singleton message set).

> Should return `ok`.
""".
mb_inline_primitive_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, integer()}.
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with inline message with built-in primitive type (a singleton message set)",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with inline message with built-in primitive
        % type (a singleton message set)
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface,
            {mb, {type, _, tuple, [{atom, _, msg}, {type, _, integer, []}]},
              []}}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with inline message with mailbox interface type (a
singleton message set).

> Should return `ok`.
""".
mb_inline_mb_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, mb()}.
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with inline message with mailbox interface type (a singleton message set)",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with inline message with mailbox interface
        % type (a singleton message set).
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface,
            {mb, {type, _, tuple, [{atom, _, msg}, {user_type, _, mb, []}]},
              []}}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with type union (a non-trivial set).

> Should return `ok`.
""".
mb_type_union() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, integer()} | {msg, mb()}.
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with type union (a non-trivial set)",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with type union (a non-trivial set).
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface,
            {mb, {type, _, union,
              [{type, _, tuple, [{atom, _, msg}, {type, _, integer, []}]},
                {type, _, tuple, [{atom, _, msg}, {user_type, _, mb, []}]}]},
              []}}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with message type (a message set alias).

> Should return `ok`.
""".
mb_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: pid().
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with message type (a message set alias)",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with message type (a message set alias).
        % Message set alias is expanded in place.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}}
        ], Forms)
      end}
  end}.

-doc """
Tests mailbox interface type with all message types.

> Should return `ok`.
""".
mb_msg_all_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, integer()} | msg().
  -type msg() :: {msg, mb()} | pid().
  """,
  fun(_, Result) ->
    {"Test mailbox interface type with all message types",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Mailbox interface type with all message types.
        % Message set alias is expanded in place.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface,
            {mb, {type, _, union,
              [{type, _, tuple, [{atom, _, msg}, {type, _, integer, []}]},
                {type, _, tuple, [{atom, _, msg}, {user_type, _, mb, []}]},
                {type, _, pid, []}]},
              []}}
        ], Forms)
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Function mailbox annotation tests.
%%% ----------------------------------------------------------------------------

-doc "Function mailbox annotation tests.".
fun_anno_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun fun_no_guard_no_mb_scope/0,
      fun fun_guard_no_mb_scope/0,
      fun fun_no_guard_in_mb_scope/0,
      fun fun_guard_in_mb_scope/0,
      fun fun_no_guard_params_no_mb_scope/0,
      fun fun_guard_params_no_mb_scope/0,
      fun fun_no_guard_params_in_mb_scope/0,
      fun fun_guard_params_in_mb_scope/0
    ]]}.


-doc """
Tests unguarded function outside mailbox interface scope.

> Should return `ok`.
""".
fun_no_guard_no_mb_scope() -> {
  """
  -module(test).
  -spec f() -> ok.
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test unguarded function outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unguarded function outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {function, _, f, 0,
            [{clause, [{location, _}, {type, {atom, _, ok}}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests guarded function outside mailbox interface scope.

> Should return `error`: guarded function unsupported.
""".
fun_guard_no_mb_scope() -> {
  """
  -module(test).
  -spec f() -> ok.
  f() when true -> ok.
  """,
  fun(_, Result) ->
    {"Test guarded function outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Guarded function outside mailbox interface scope.
        ?assertMatch({_, _,
          {e_bad__clause,
            {clause, _,
              [], [[{atom, _, true}]], []}}},
          Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unguarded function inside mailbox interface scope.

> Should return `ok`.
""".
fun_no_guard_in_mb_scope() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test unguarded function inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unguarded function inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function, [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause, [{location, _},
              {type, {atom, _, ok}},
              {interface, mb}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests guarded function inside mailbox interface scope.

> Should return `error`: guarded function unsupported.
""".
fun_guard_in_mb_scope() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() when true -> ok.
  """,
  fun(_, Result) ->
    {"Test guarded function inside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Guarded function inside mailbox interface scope.
        ?assertMatch({_, _,
          {e_bad__clause,
            {clause, _, [], [[{atom, _, true}]], []}}},
          Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unguarded function with parameters outside mailbox interface scope.

> Should return `ok`.
""".
fun_no_guard_params_no_mb_scope() -> {
  """
  -module(test).
  -spec f(integer(), float()) -> ok.
  f(X, Y) -> ok.
  """,
  fun(_, Result) ->
    {"Test unguarded function with parameters outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unguarded function with parameters outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {function, _, f, 2,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'},
                {var, [{location, _}, {type, {type, _, float, []}}], 'Y'}],
              [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests guarded function with parameters outside mailbox interface scope.

> Should return `error`: guarded function unsupported.
""".
fun_guard_params_no_mb_scope() -> {
  """
  -module(test).
  -spec f(integer(), float()) -> ok.
  f(X, Y) when X > 0, Y =:= 1 -> ok.
  """,
  fun(_, Result) ->
    {"Test guarded function with parameters outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Guarded function with parameters outside mailbox interface scope.
        ?assertMatch({_, _,
          {e_bad__clause,
            {clause, _,
              [{var, _, 'X'}, {var, _, 'Y'}],
              [[{op, _, '>', {var, _, 'X'}, {integer, _, 0}},
                {op, _, '=:=', {var, _, 'Y'}, {integer, _, 1}}]],
              []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unguarded function with parameters inside mailbox interface scope.

> Should return `ok`.
""".
fun_no_guard_params_in_mb_scope() -> {
  """
  -module(test).
  -new({mb, [f/2]}).
  -type mb() :: pid().
  -spec f(integer(), float()) -> ok.
  f(X, Y) -> ok.
  """,
  fun(_, Result) ->
    {"Test unguarded function with parameters inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unguarded function with parameters inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 2,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'},
                {var, [{location, _}, {type, {type, _, float, []}}], 'Y'}],
              [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests guarded function with parameters inside mailbox interface scope.

> Should return `error`: guarded function unsupported.
""".
fun_guard_params_in_mb_scope() -> {
  """
  -module(test).
  -new({mb, [f/2]}).
  -type mb() :: pid().
  -spec f(integer(), float()) -> ok.
  f(X, Y) when X > 0, Y =:= 1 -> ok.
  """,
  fun(_, Result) ->
    {"Test guarded function with parameters inside mailbox interface scope.",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Guarded function with parameters inside mailbox interface scope.
        ?assertMatch({_, _,
          {e_bad__clause,
            {clause, _,
              [{var, _, 'X'}, {var, _, 'Y'}],
              [[{op, _, '>', {var, _, 'X'}, {integer, _, 0}},
                {op, _, '=:=', {var, _, 'Y'}, {integer, _, 1}}]],
              []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Expression sequence mailbox annotation tests.
%%% ----------------------------------------------------------------------------

-doc "Expression sequence mailbox annotation tests.".
expr_seq_mb_anno_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun mb_anno_end_expr_seq/0,
      fun mb_anno_succ_expr_seq/0
    ]]}.

-doc """
Tests mailbox annotation at end of expression list.

> Should return `error`.
""".
mb_anno_end_expr_seq() -> {{
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as'.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test mailbox annotation at end of expression list",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Mailbox annotation at end of expression list.
        ?assertMatch(
          {_, _,
            {e_exp__expr,
              {tree, macro,
                {attr, _, [], none},
                {macro, {atom, _, as}, [{atom, _, mb}]}}}},
          Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests successive mailbox annotations in expression list.

> Should return `error`.
""".
mb_anno_succ_expr_seq() -> {{
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as', '@as', 1 + 1.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test successive mailbox annotations in expression list",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Successive mailbox annotations in expression list.
        ?assertMatch(
          {_, _,
            {e_exp__expr,
              {tree, macro,
                {attr, _, [], none},
                {macro, {atom, _, as}, [{atom, _, mb}]}}}},
          Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Spawn call tests.
%%% ----------------------------------------------------------------------------

-doc "`spawn` call tests".
spawn_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun spawn_undef_static_fun_in_mb_scope/0,
      fun spawn_static_fun_undef_mb_in_mb_scope/0,
      fun spawn_static_fun_in_mb_scope/0,
      fun spawn_static_fun_override_in_mb_scope/0,
      fun spawn_undef_static_fun_no_mb_scope/0,
      fun spawn_static_fun_undef_mb_no_mb_scope/0,
      fun spawn_static_fun_no_mb_scope/0,
      fun anno_spawn/0,
      fun spawn_dynamic_fun/0
    ]]}.

-doc """
Tests unannotated `spawn` call with undefined static function inside mailbox
interface scope.

> Should return `error`.
""".
spawn_undef_static_fun_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with undefined static function inside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated spawn call with undefined static function inside mailbox
        % scope.
        ?assertMatch(
          {_, _, {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated `spawn` call with defined static function and undefined
mailbox interface inside mailbox interface scope.

> Should return `error`.
""".
spawn_static_fun_undef_mb_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with defined static function and undefined mailbox interface inside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated spawn call with defined static function and undefined
        % mailbox interface inside mailbox interface scope.
        ?assertMatch(
          {_, _, {e_undef__mb, {'fun', _, {function, g, 0}}}}, Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated `spawn` call with defined static function and defined mailbox
interface inside mailbox interface scope.

> Should return `ok`.
""".
spawn_static_fun_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0, g/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with defined static function and defined mailbox interface inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated spawn call with defined static function and defined
        % mailbox interface inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {type, _, pid, []}}, {interface, mb}],
              [], [],
              [{call,
                [{location, _}, {interface, mb}, {modality, new}],
                {atom, _, spawn},
                [{atom, _, test}, {atom, _, g}, {nil, _}]}]}]},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated `spawn` call with defined static function and defined mailbox
interface override inside mailbox interface scope.

> Should return `ok`: mailbox modality is overridden with `new`.
""".
spawn_static_fun_override_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -use({mb, [g/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with defined static function and defined mailbox interface override inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated spawn call with defined static function and defined
        % mailbox interface override inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {type, _, pid, []}}, {interface, mb}],
              [], [],
              [{call,
                [{location, _}, {interface, mb}, {modality, new}],
                {atom, _, spawn},
                [{atom, _, test}, {atom, _, g}, {nil, _}]}]}]},
          {function,
            [{location, _}, {interface, mb}, {modality, use}],
            g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated `spawn` call with undefined static function outside mailbox interface scope.

> Should return `error`.
""".
spawn_undef_static_fun_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with undefined static function outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated spawn call with undefined static function outside mailbox
        % scope.
        ?assertMatch(
          {_, _, {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated `spawn` call with defined static function and undefined
mailbox interface outside mailbox interface scope.

> Should return `error`.
""".
spawn_static_fun_undef_mb_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with defined static function and undefined mailbox interface outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated spawn call with defined static function and undefined
        % mailbox interface outside mailbox interface scope.
        ?assertMatch(
          {_, _, {e_undef__mb, {'fun', _, {function, g, 0}}}}, Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated `spawn` call with defined static function and defined mailbox
interface outside mailbox interface scope.

> Should return `ok`.
""".
spawn_static_fun_no_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [g/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> spawn(test, g, []).
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated spawn call with defined static function and defined mailbox interface outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated spawn call with defined static function and defined
        % mailbox interface outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function, _, f, 0,
            [{clause,
              [{location, _}, {type, {type, _, pid, []}}],
              [], [],
              [{call,
                [{location, _}, {interface, mb}, {modality, new}],
                {atom, _, spawn},
                [{atom, _, test}, {atom, _, g}, {nil, _}]}]}]},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated `spawn` call.

> Should return `error`: annotated `spawn` unsupported.
""".
anno_spawn() -> {{
  """
  -module(test).
  -spec f() -> pid().
  f() -> '@as', spawn(test, g, []).
  """,
  [{as, ?as(undef_mb)}]},
  fun(_, Result) ->
    {"Test annotated spawn call",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated spawn call.
        ?assertMatch(
          {_, _, {e_bad__anno_on, {call, _, {atom, 0, spawn}, []}}}, Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc
"""
Tests `spawn` call with dynamic function.

> Should return `error`: dynamic functions unsupported.
""".
spawn_dynamic_fun() -> {{
  """
  -module(test).
  -spec f() -> pid().
  f() -> F = g, spawn(test, F, []).
  """,
  []},
  fun(_, Result) ->
    {"Test spawn call with dynamic function",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Spawn call with dynamic function.
        ?assertMatch({_, _, {e_bad__expr, {var, _, 'F'}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Self call tests.
%%% ----------------------------------------------------------------------------

-doc "`self` call tests.".
self_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun self_no_mb_scope/0,
      fun anno_self_no_mb_scope/0,
      fun self_in_mb_scope/0,
      fun anno_self_in_mb_scope/0,
      fun anno_self_undef_mb_in_mb_scope/0,
      fun bad_anno_self/0
    ]]}.

-doc """
Tests unannotated `self` call outside mailbox interface scope.

> Should return `error`.
""".
self_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> mb().
  f() -> self().
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated self call outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated self call outside mailbox interface scope.
        ?assertMatch({_, _,
          {e_no__mb_scope, {call, _, {atom, _, self}, []}}},
          Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated `self` call outside mailbox interface scope.

> Should return `error`.
""".
anno_self_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> mb().
  f() -> '@as', self().
  """,
  [{as, ?as(undef_mb)}]},
  fun(_, Result) ->
    {"Test annotated self call outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated self call outside mailbox interface scope.
        ?assertMatch(
          {_, _, {e_no__mb_scope, {call, _, {atom, _, self}, []}}}, Error
        ),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated `self` call inside mailbox interface scope.

> Should return `ok`.
""".
self_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> self().
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated self call inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated self call inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{call,
                [{location, _}, {interface, mb}],
                {atom, _, self},
                []}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated `self` call with defined mailbox interface inside mailbox interface scope.

> Should return `ok`.
""".
anno_self_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> mb().
  f() -> '@as', self().
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated self call with defined mailbox interface inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Annotated self call with defined mailbox interface inside mailbox
        % scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _},
                {type, {user_type, _, mb, []}},
                {interface, mb}],
              [], [],
              [{call,
                [{location, _}, {interface, mb}],
                {atom, _, self},
                []}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated `self` call with undefined mailbox interface inside mailbox
scope.

> Should return `error`.
""".
anno_self_undef_mb_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> mb().
  f() -> '@as', self().
  """,
  [{as, ?as(undef_mb)}]},
  fun(_, Result) ->
    {"Test annotated self call with undefined mailbox interface inside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated self call with undefined mailbox interface inside mailbox
        % interface scope.
        ?assertMatch({_, _, {e_undef__mb_scope, {atom, _, undef_mb}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated `self` call with wrong annotation.

> Should return `error`.
""".
bad_anno_self() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> mb().
  f() -> '@expects', self().
  """,
  [{expects, ?expects(mb, "msg")}]},
  fun(_, Result) ->
    {"Test annotated self call with wrong annotation",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated self call with wrong annotation.
        ?assertMatch({_, _,
          {e_bad__anno_on, {call, _, {atom, _, self}, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Function call tests.
%%% ----------------------------------------------------------------------------

-doc "Function call tests.".
call_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun fun_in_mb_scope_to_static_fun_new_in_mb_scope/0,
      fun fun_in_mb_scope_to_static_fun_use_in_mb_scope/0,
      fun fun_in_mb_scope_to_static_fun_no_mb_scope/0,
      fun fun_in_mb_scope_to_undef_static_fun/0,
      fun fun_in_mb_scope_to_static_direct_rec_fun_in_mb_scope/0,
%%      fun fun_in_mb_scope_to_static_mutual_rec_fun_in_mb_scope/0,
      fun fun_no_mb_scope_to_static_fun_in_mb_scope/0,
      fun fun_no_mb_scope_to_static_fun_no_mb_scope/0,
      fun fun_no_mb_scope_undef_static_fun/0,
      fun fun_no_mb_scope_static_direct_rec_fun_no_mb_scope/0,
      fun fun_no_mb_scope_static_mutual_rec_fun_no_mb_scope/0,
      fun fun_dynamic_fun/0,
      fun anno_fun_static_fun/0
    ]]}.


-doc """
Tests unannotated function call inside mailbox interface scope to defined static
function with `-new` modality inside mailbox interface scope.

> Should return `ok`.
""".
fun_in_mb_scope_to_static_fun_new_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb0, [f/1]}).
  -new({mb1, [g/0]}).
  -type mb0() :: pid().
  -type mb1() :: pid().
  -spec f(integer()) -> ok.
  f(X) -> g().
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call inside mailbox interface scope to defined static function with -new modality inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call inside mailbox interface scope to defined
        % static function with -new modality inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb1, {type, _, pid, []}, []}},
          {attribute, _, interface, {mb0, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb0}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb0}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call,
                [{location, _}, {interface, mb1}, {modality, new}],
                {atom, _, g},
                []}]}]},
          {function,
            [{location, _}, {interface, mb1}, {modality, new}],
            g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb1}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated function call inside mailbox interface scope to defined static
function with `-use` modality inside mailbox interface scope.

> Should return `ok`.
""".
fun_in_mb_scope_to_static_fun_use_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -use({mb, [g/0]}).
  -type mb() :: pid().
  -spec f(integer()) -> ok.
  f(X) -> g().
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call inside mailbox interface scope to defined static function with -use modality inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call inside mailbox interface scope to defined
        % static function with -use modality inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call,
                [{location, _}, {interface, mb}, {modality, use}],
                {atom, _, g},
                []}]}]},
          {function,
            [{location, _}, {interface, mb}, {modality, use}],
            g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated function call inside mailbox interface scope to defined static
function outside mailbox interface scope.

> Should return `ok`.
""".
fun_in_mb_scope_to_static_fun_no_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(integer()) -> ok.
  f(X) -> g().
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call inside mailbox interface scope to defined static function outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call inside mailbox interface scope to defined
        % static function outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call, _, {atom, _, g}, []}]}]},
          {function, _, g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated function call inside mailbox interface scope to undefined
static function.

> Should return `error`.
""".
fun_in_mb_scope_to_undef_static_fun() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> g().
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call inside mailbox interface scope to undefined static function",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated function call inside mailbox interface scope to undefined
        % static function.
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated function call inside mailbox interface scope to defined static
direct recursive function inside defined mailbox interface scope.

> Should return `ok`: mailbox modality is overridden with `use`.
""".
fun_in_mb_scope_to_static_direct_rec_fun_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(integer()) -> ok.
  f(X) -> f(X).
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call inside mailbox interface scope to defined static direct recursive function inside defined mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call inside mailbox interface scope to defined
        % static direct recursive function inside defined mailbox interface
        % scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call,
                [{location, _}, {interface, mb}, {modality, use}],
                {atom, _, f},
                [{var, _, 'X'}]}]}]}
        ], Forms)
      end}
  end}.

% Tests unannotated function call inside mailbox interface scope to defined static mutual recursive function inside defined mailbox interface scope. GOOD. Should ?use.
% Should return `ok`: mailbox modality is overridden with `use`.
fun_in_mb_scope_to_static_mutual_rec_fun_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb0, [f/1]}).
  -new({mb1, [g/0]}).
  -type mb0() :: pid().
  -type mb1() :: pid().
  -spec f(integer()) -> ok.
  f(X) -> g(). f/1 = f/1, g/1
  -spec g() -> ok.
  g() -> f(1). g/0 = g/0, f/1
  """,
  []},
  fun(_, Result) ->
    {"Name",
      fun() ->
        ?TRACE("Result = ~p", [Result])
      end}
  end}.


-doc """
Tests unannotated function call outside mailbox interface scope to defined
static function inside mailbox interface scope.

> Should return `ok`.
""".
fun_no_mb_scope_to_static_fun_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [g/0]}).
  -type mb() :: pid().
  -spec f(integer()) -> ok.
  f(X) -> g().
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call outside mailbox interface scope to defined static function inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call outside mailbox interface scope to defined
        % static function inside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function, _, f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call,
                [{location, _}, {interface, mb}, {modality, new}],
                {atom, _, g},
                []}]}]},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated function call outside mailbox interface scope to defined
static function outside mailbox interface scope.

> Should return `ok`.
""".
fun_no_mb_scope_to_static_fun_no_mb_scope() -> {{
  """
  -module(test).
  -spec f(integer()) -> ok.
  f(X) -> g().
  -spec g() -> ok.
  g() -> ok.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call outside mailbox interface scope to defined static function outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call outside mailbox interface scope to defined
        % static function outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {function, _, f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call, _, {atom, _, g}, []}]}]},
          {function, _, g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [], [],
              [{atom, _, ok}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated function call outside mailbox interface scope to undefined
static function.

> Should return `error`.
""".
fun_no_mb_scope_undef_static_fun() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> g().
  """,
  []},
  fun(_, Result) ->
    {"Name",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated function call outside mailbox interface scope to undefined
        % static function.
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated function call outside mailbox interface scope to defined
static direct recursive function outside mailbox interface scope.

> Should return `ok`: mailbox modality is overridden with `use`.
""".
fun_no_mb_scope_static_direct_rec_fun_no_mb_scope() -> {{
  """
  -module(test).
  -spec f(integer()) -> ok.
  f(X) -> f(X).
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call outside mailbox interface scope to defined static direct recursive function outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call outside mailbox interface scope to defined
        % static direct recursive function outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {function, _, f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call, _, {atom, _, f}, [{var, _, 'X'}]}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests unannotated function call outside mailbox interface scope to defined
static mutual recursive function outside mailbox interface scope.

> Should return `ok`: mailbox modality is overridden with `use`.
""".
fun_no_mb_scope_static_mutual_rec_fun_no_mb_scope() -> {{
  """
  -module(test).
  -spec f(integer()) -> ok.
  f(X) -> g().
  -spec g() -> ok.
  g() -> f(5).
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated function call outside mailbox interface scope to defined static mutual recursive function outside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated function call outside mailbox interface scope to defined
        % static mutual recursive function outside mailbox interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {function, _, f, 1,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{call, _, {atom, _, g}, []}]}]},
          {function, _, g, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [], [],
              [{call, _, {atom, _, f}, [{integer, _, 5}]}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests dynamic function call.

> Should return `error`: dynamic functions unsupported.
""".
fun_dynamic_fun() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> F = h, F().
  """,
  []},
  fun(_, Result) ->
    {"Test dynamic function call",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Dynamic function call.
        ?assertMatch({_, _,
          {e_bad__expr, {call, _, {var, _, 'F'}, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated static function call.

> Should return `error`: annotated function call unsupported.
""".
anno_fun_static_fun() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> '@as', f().
  """,
  [{as, ?as(undef_mb)}]},
  fun(_, Result) ->
    {"Test annotated static function call",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated static function call.
        ?assertMatch({_, _,
          {e_bad__anno_on, {call, _, {atom, _, f}, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Receive tests.
%%% ----------------------------------------------------------------------------

receive_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun receive_no_mb_scope/0,
      fun anno_receive_no_mb_scope/0,
      fun receive_in_mb_scope/0,
      fun anno_receive_in_mb_scope/0,
      fun anno_receive_undef_mb_in_mb_scope/0,
      fun bad_anno_receive/0,
      fun expr_receive_no_mb_scope/0,
      fun expr_anno_receive_no_mb_scope/0,
      fun expr_receive_in_mb_scope/0,
      fun expr_anno_receive_in_mb_scope/0,
      fun expr_anno_receive_undef_mb_in_mb_scope/0,
      fun expr_bad_anno_receive/0
    ]]}.

-doc """
Tests unannotated `receive` call outside mailbox interface scope.

> Should return `error`.
""".
receive_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> receive {msg} -> ok end.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated receive call outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated receive call outside mailbox interface scope.
        ?assertMatch({_, _,
          {e_no__mb_scope, {'receive', _, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated `receive` call outside mailbox interface scope.

> Should return `error`.
""".
anno_receive_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> '@expects', receive {msg} -> ok end.
  """,
  [{expects, ?expects(undef_mb, "msg")}]},
  fun(_, Result) ->
    {"Test annotated receive call outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated receive call outside mailbox interface scope.
        ?assertMatch({_, paterl_anno,
          {e_no__mb_scope, {'receive', _, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated `receive` call inside mailbox interface scope.

> Should return `error`.
""".
receive_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> receive {msg} -> ok end.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated receive call inside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated receive call inside mailbox interface scope.
        ?assertMatch({_, paterl_anno,
          {e_exp__anno, {tree, macro, {attr, _, [], none},
            {macro, {atom, _, expects}, []}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated `receive` call with defined mailbox interface inside mailbox
interface scope.

> Should return `ok`.
""".
anno_receive_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@expects', receive {msg} -> ok end.
  """,
  [{expects, ?expects(mb, "msg")}]},
  fun(_, Result) ->
    {"Test annotated receive call with defined mailbox interface inside mailbox interface scope",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Annotated receive call with defined mailbox interface inside mailbox
        % interface scope.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{'receive',
                [{location, _},
                  {interface, mb},
                  {state, "msg"},
                  {type, {atom, _, ok}}],
                [{clause, _,
                  [{tuple, _, [{atom, _, msg}]}],
                  [],
                  [{atom, _, ok}]}]}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated `receive` call with undefined mailbox interface inside mailbox
scope.

> Should return `error`.
""".
anno_receive_undef_mb_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@expects', receive {msg} -> ok end.
  """,
  [{expects, ?expects(undef_mb, "msg")}]},
  fun(_, Result) ->
    {"Test annotated receive call with undefined mailbox interface inside mailbox scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated receive call with undefined mailbox interface inside mailbox
        % scope.
        ?assertMatch({_, _,
          {e_undef__mb_scope, {atom, _, undef_mb}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated `receive` with wrong annotation.

> Should return `error`.
""".
bad_anno_receive() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as', receive {msg} -> ok end.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated receive with wrong annotation",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated receive with wrong annotation.
        ?assertMatch({_, _,
          {e_bad__anno_on, {'receive', _, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests inner expression error in unannotated `receive` call outside mailbox
interface scope.

> Should return `error`.
""".
expr_receive_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> receive {msg} -> g() end.
  """,
  []},
  fun(_, Result) ->
    {"Test inner expression error in unannotated receive call outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_, _]}], []}, Result),
        {error, [{_, [
          Error0 = {_, _, Code0}, Error1 = {_, _, Code1}
        ]}], _Warnings = []} = Result,

        % Inner expression error in unannotated receive call outside mailbox
        % interface scope.
        ?assertMatch({_, _,
          {e_no__mb_scope, {'receive', _, []}}}, Error0),
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error1),
        ?debugMsg(paterl_anno:format_error(Code0)),
        ?debugMsg(paterl_anno:format_error(Code1))
      end}
  end}.

-doc """
Tests inner expression error in annotated `receive` call outside mailbox
interface scope.

> Should return `error`.
""".
expr_anno_receive_no_mb_scope() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> '@expects', receive {msg} -> g() end.
  """,
  [{expects, ?expects(undef_mb, "msg")}]},
  fun(_, Result) ->
    {"Test inner expression error in annotated receive call outside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_, _]}], []}, Result),
        {error, [{_, [
          Error0 = {_, _, Code0}, Error1 = {_, _, Code1}
        ]}], _Warnings = []} = Result,

        % Inner expression error in annotated receive call outside mailbox
        % interface scope.
        ?assertMatch({_, _,
          {e_no__mb_scope, {'receive', _, []}}}, Error0),
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error1),
        ?debugMsg(paterl_anno:format_error(Code0)),
        ?debugMsg(paterl_anno:format_error(Code1))
      end}
  end}.

-doc """
Tests inner expression error in unannotated `receive` call inside mailbox
interface scope.

> Should return `error`.
""".
expr_receive_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> receive {msg} -> g() end.
  """,
  []},
  fun(_, Result) ->
    {"Test inner expression error in unannotated receive call inside mailbox interface scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_, _]}], []}, Result),
        {error, [{_, [
          Error0 = {_, _, Code0}, Error1 = {_, _, Code1}
        ]}], _Warnings = []} = Result,

        % Inner expression error in unannotated receive call inside mailbox
        % interface scope.
        ?assertMatch({_, paterl_anno,
          {e_exp__anno, {tree, macro,
            {attr, _, [], none},
            {macro, {atom, _, expects}, []}}}}, Error0),
        ?assertMatch({_, paterl_anno,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error1),
        ?debugMsg(paterl_anno:format_error(Code0)),
        ?debugMsg(paterl_anno:format_error(Code1))
      end}
  end}.

-doc """
Tests inner expression error in annotated `receive` call with defined mailbox
interface inside mailbox interface scope.

> Should return `error`.
""".
expr_anno_receive_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@expects', receive {msg} -> g() end.
  """,
  [{expects, ?expects(mb, "msg")}]},
  fun(_, Result) ->
    {"Test inner expression error in annotated receive call with defined mailbox interface inside mailbox interface scope.",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Inner expression error in annotated receive call with defined mailbox
        % interface inside mailbox interface scope.
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', 5, {function, g, 0}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests inner expression error in annotated `receive` call with undefined mailbox interface inside mailbox scope.

> Should return `error`.
""".
expr_anno_receive_undef_mb_in_mb_scope() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@expects', receive {msg} -> g() end.
  """,
  [{expects, ?expects(undef_mb, "msg")}]},
  fun(_, Result) ->
    {"Test inner expression error in annotated receive call with undefined mailbox interface inside mailbox scope",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_, _]}], []}, Result),
        {error, [{_, [
          Error0 = {_, _, Code0}, Error1 = {_, _, Code1}
        ]}], _Warnings = []} = Result,

        % Inner expression error in annotated receive call with undefined
        % mailbox interface inside mailbox scope.
        ?assertMatch({_, _,
          {e_undef__mb_scope, {atom, _, undef_mb}}}, Error0),
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error1),
        ?debugMsg(paterl_anno:format_error(Code0)),
        ?debugMsg(paterl_anno:format_error(Code1))
      end}
  end}.

-doc """
Tests inner expression error in annotated `receive` with wrong annotation.

> Should return `error`.
""".
expr_bad_anno_receive() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as', receive {msg} -> g() end.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test inner expression error in annotated receive with wrong annotation",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_, _]}], []}, Result),
        {error, [{_, [
          Error0 = {_, _, Code0}, Error1 = {_, _, Code1}
        ]}], _Warnings = []} = Result,

        % Inner expression error in annotated receive with wrong annotation.
        ?assertMatch({_, _,
          {e_bad__anno_on, {'receive', _, []}}}, Error0),
        ?assertMatch({_, _,
          {e_undef__fun_ref, {'fun', _, {function, g, 0}}}}, Error1),
        ?debugMsg(paterl_anno:format_error(Code0)),
        ?debugMsg(paterl_anno:format_error(Code1))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% If tests.
%%% ----------------------------------------------------------------------------

-doc "`if` tests.".
if_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun 'if'/0,
      fun anno_if/0
    ]]}.

-doc """
Tests unannotated `if`.

> Should return `error`.
""".
'if'() -> {{
  """
  -module(test).
  -spec f() -> ok.
  f() -> if 1 =:= 1 -> ok; true -> ok end.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated if",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated if.
        ?assertMatch([
          {attribute, _, module, test},
          {function, _, f, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}],
              [], [],
              [{'if', _,
                [{clause, _, [],
                  [[{op, _, '=:=', {integer, _, 1}, {integer, _, 1}}]],
                  [{atom, _, ok}]},
                  {clause, _, [],
                    [[{atom, _, true}]],
                    [{atom, _, ok}]}]}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated `if`.

> Should return `error`.
""".
anno_if() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as', if 1 =:= 1 -> ok; true -> ok end.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated if",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated if.
        ?assertMatch({_, _, {e_bad__anno_on, {'if', _, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Match tests.
%%% ----------------------------------------------------------------------------

-doc "Match tests.".
match_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun match_pat_no_var/0,
      fun match_spawn/0,
      fun anno_match_spawn/0,
      fun match_self/0,
      fun anno_match_self/0,
      fun bad_anno_match_self/0,
      fun match_fun/0,
      fun anno_match_fun/0,
      fun match_receive/0,
      fun anno_match_receive/0,
      fun bad_anno_match_receive/0,
      fun match_if/0,
      fun anno_match_if/0,
      fun match_var/0,
      fun anno_match_var/0,
      fun match_bin_op/0,
      fun anno_match_bin_op/0,
      fun match_lit/0,
      fun anno_match_lit/0
    ]]}.

-doc """
Tests match pattern with non-variable.

> Should return `error`.
""".
match_pat_no_var() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> 1.
  f() -> 1 = 1.
  """,
  []},
  fun(_, Result) ->
    {"Test match pattern with non-variable",
      fun() ->

        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Match pattern with non-variable.
        ?assertMatch({_, _,
          {e_bad__expr, {match, _, {integer, _, 1}, {integer, _, 1}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on `spawn` call.

> Should return `ok`.
""".
match_spawn() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> X = spawn(test, f, []).
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on spawn call",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on spawn call.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {type, _, pid, []}}, {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {call,
                  [{location, _}, {interface, mb}, {modality, new}],
                  {atom, _, spawn},
                  [{atom, _, test}, {atom, _, f}, {nil, _}]}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on `spawn` call.

> Should return `error`.
""".
anno_match_spawn() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> '@as', X = spawn(test, f, []).
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on spawn call",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on spawn call.
        ?assertMatch({_, _,
          {e_bad__anno_on, {call, _, {atom, _, spawn}, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on `self` call.

> Should return `ok`.
""".
match_self() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> X = self().
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on self call",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on self call.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {type, _, pid, []}}, {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {call,
                  [{location, _}, {interface, mb}],
                  {atom, _, self},
                  []}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on `self` call.

> Should return `ok`.
""".
anno_match_self() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> '@as', X = self().
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on self call",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Annotated match on self call.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {type, _, pid, []}}, {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {call,
                  [{location, _}, {interface, mb}],
                  {atom, _, self},
                  []}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on `self` call with wrong annotation.

> Should return `error`.
""".
bad_anno_match_self() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> pid().
  f() -> '@expects', X = self().
  """,
  [{expects, ?expects(mb, "msg")}]},
  fun(_, Result) ->
    {"Test annotated match on self call with wrong annotation",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on self call with wrong annotation.
        ?assertMatch({_, _,
          {e_bad__anno_on, {call, _, {atom, _, self}, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on function call.

> Should return `ok`.
""".
match_fun() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> no_return().
  f() -> X = f().
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on function call",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on function call.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _},
                {type, {type, _, no_return, []}},
                {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {call,
                  [{location, _}, {interface, mb}, {modality, use}],
                  {atom, _, f},
                  []}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on function call.

> Should return `error`.
""".
anno_match_fun() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> no_return().
  f() -> '@as', X = f().
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on function call",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on function call.
        ?assertMatch({_, _,
          {e_bad__anno_on, {call, _, {atom, _, f}, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on `receive`.

> Should return `error`.
""".
match_receive() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> X = receive {msg} -> ok end.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on receive",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Unannotated match on receive.
        ?assertMatch({_, _,
          {e_exp__anno, {tree, macro, {attr, _, [], none},
            {macro, {atom, _, expects}, []}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests annotated match on `receive`.

> Should return `ok`.
""".
anno_match_receive() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@expects', X = receive {msg} -> ok end.
  """,
  [{expects, ?expects(mb, "msg")}]},
  fun(_, Result) ->
    {"Test annotated match on receive",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Annotated match on receive.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {'receive',
                  [{location, _},
                    {interface, mb},
                    {state, "msg"},
                    {type, {atom, _, ok}}],
                  [{clause, _,
                    [{tuple, _, [{atom, _, msg}]}],
                    [],
                    [{atom, _, ok}]}]}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on `receive` with wrong annotation.

> Should return `error`.
""".
bad_anno_match_receive() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as', X = receive {msg} -> ok end.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on receive with wrong annotation",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on receive with wrong annotation.
        ?assertMatch({_, _,
          {e_bad__anno_on, {'receive', _, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on `if`.

> Should return `ok`.
""".
match_if() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> X = if 1 =:= 1 -> ok; true -> ok end.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on if",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on if.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _}, {type, {atom, _, ok}}, {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {'if', _,
                  [{clause, _, [],
                    [[{op, _, '=:=', {integer, _, 1}, {integer, _, 1}}]],
                    [{atom, _, ok}]},
                    {clause, _, [], [[{atom, _, true}]], [{atom, _, ok}]}]}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on `if`.

> Should return `error`.
""".
anno_match_if() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> '@as', X = if 1 =:= 1 -> ok; true -> ok end.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on if",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on if.
        ?assertMatch({_, _, {e_bad__anno_on, {'if', _, []}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc"""
Tests unannotated match on variable.

> Should return `ok`.
""".
match_var() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(integer()) -> integer().
  f(Y) -> X = Y.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on variable",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on variable.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _},
                {type, {type, _, integer, []}},
                {interface, mb}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'Y'}],
              [],
              [{match, _, {var, _, 'X'}, {var, _, 'Y'}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on variable.

> Should return `error`.
""".
anno_match_var() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(integer()) -> integer().
  f(Y) -> '@as', X = Y.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on variable",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on variable.
        ?assertMatch({_, _, {e_bad__anno_on, {var, _, 'Y'}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on binary operator.

> Should return `ok`.
""".
match_bin_op() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> integer().
  f() -> X = 1 + 1.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on binary operator",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on binary operator.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause,
              [{location, _},
                {type, {type, _, integer, []}},
                {interface, mb}],
              [], [],
              [{match, _,
                {var, _, 'X'},
                {op, _, '+', {integer, _, 1}, {integer, _, 1}}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on binary operator.

> Should return `error`.
""".
anno_match_bin_op() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> integer().
  f() -> '@as', X = 1 + 1.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on binary operator",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on binary operator.
        ?assertMatch({_, _,
          {e_bad__anno_on, {op, _, '+', {integer, _, 1}, {integer, _, 1}}}},
          Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated match on literal.

> Should return `ok`.
""".
match_lit() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> integer().
  f() -> X = 1.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated match on literal",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated match on literal.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function, [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause, [{location, _},
              {type, {type, _, integer, []}},
              {interface, mb}],
              [], [],
              [{match, _, {var, _, 'X'}, {integer, _, 1}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated match on literal.

> Should return `error`.
""".
anno_match_lit() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> integer().
  f() -> '@as', X = 1.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated match on literal",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated match on literal.
        ?assertMatch({_, _, {e_bad__anno_on, {integer, _, 1}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Binary, variable, and literal expression tests.
%%% ----------------------------------------------------------------------------

-doc "Binary, variable, and literal expression tests.".
expr_test_() ->
  {foreachx, fun startup_mb_anno/1, [
    T() || T <- [
      fun bin_op/0,
      fun anno_bin_op/0,
      fun var/0,
      fun anno_var/0,
      fun lit/0,
      fun anno_lit/0
    ]]}.

-doc """
Tests unannotated binary operator.

> Should return `ok`.
""".
bin_op() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(mb()) -> integer().
  f(X) -> X ! {msg}.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated binary operator",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated binary operator.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, 3, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _},
                {type, {type, _, integer, []}},
                {interface, mb}],
              [{var, [{location, _}, {type, {user_type, _, mb, []}}], 'X'}],
              [],
              [{op, _, '!', {var, _, 'X'}, {tuple, _, [{atom, _, msg}]}}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated binary operator.

> Should return `error`.
""".
anno_bin_op() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(mb()) -> integer().
  f(X) -> '@as', X ! {msg}.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated binary operator",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated binary operator.
        ?assertMatch({_, _,
          {e_bad__anno_on, {op, _, '!', {var, _, 'X'},
            {tuple, _, [{atom, _, msg}]}}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated variable.

> Should return `ok`.
""".
var() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(integer()) -> integer().
  f(X) -> X.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated variable",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated variable.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function,
            [{location, _}, {interface, mb}, {modality, new}],
            f, 1,
            [{clause,
              [{location, _},
                {type, {type, _, integer, []}},
                {interface, mb}],
              [{var, [{location, _}, {type, {type, _, integer, []}}], 'X'}],
              [],
              [{var, _, 'X'}]}]}
        ], Forms)
      end}
  end}.

-doc """
Tests annotated variable.

> Should return `error`.
""".
anno_var() -> {{
  """
  -module(test).
  -new({mb, [f/1]}).
  -type mb() :: pid().
  -spec f(integer()) -> integer().
  f(X) -> '@as', X.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated variable",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated variable.
        ?assertMatch({_, _, {e_bad__anno_on, {var, _, 'X'}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.

-doc """
Tests unannotated literal.

> Should return `ok`.
""".
lit() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> float().
  f() -> 1.5.
  """,
  []},
  fun(_, Result) ->
    {"Test unannotated literal",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, _, []}, Result),
        {ok, Forms, _Warnings = []} = Result,

        % Unannotated literal.
        ?assertMatch([
          {attribute, _, module, test},
          {attribute, _, interface, {mb, {type, _, pid, []}, []}},
          {function, [{location, _}, {interface, mb}, {modality, new}],
            f, 0,
            [{clause, [{location, _},
              {type, {type, _, float, []}},
              {interface, mb}],
              [], [],
              [{float, _, 1.5}]}]}
        ], Forms)

      end}
  end}.

-doc """
Tests annotated literal.

> Should return `error`.
""".
anno_lit() -> {{
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> float().
  f() -> '@as', 1.5.
  """,
  [{as, ?as(mb)}]},
  fun(_, Result) ->
    {"Test annotated literal",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Annotated literal.
        ?assertMatch({_, _, {e_bad__anno_on, {float, 5, 1.5}}}, Error),
        ?debugMsg(paterl_anno:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-spec startup(paterl_test_lib:erl()) -> paterl_syntax:result().
startup(Erl) ->
  % Parse Erlang code into its abstract syntax and extract type information.
  Forms = paterl_test_lib:erl_forms(Erl),
  {ok, TypeInfo, _} = paterl_types:module(Forms),
  ?debugMsg(paterl_test_lib:banner_msg("Setup complete")),
  paterl_anno:module(Forms, TypeInfo).

startup_mb_anno({Erl, ErlSpecs}) ->
  % Parse Erlang code into its abstract syntax and extract type information.
  Forms = paterl_test_lib:expand_erl_forms(Erl, ErlSpecs),
  ?TRACE("Forms = ~p", [Forms]),
  {ok, TypeInfo, _} = paterl_types:module(Forms),
  ?debugMsg(paterl_test_lib:banner_msg("Setup complete")),
  paterl_anno:module(Forms, TypeInfo).