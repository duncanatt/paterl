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
-module(paterl_types_tests).
-moduledoc "`paterl_types` module tests.".
-author("duncan").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("paterl.hrl").


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Extended test.".
-type test_x() :: paterl_test_lib:pair(paterl_test_lib:erl(), paterl_types:result()).


%%% ----------------------------------------------------------------------------
%%% Fixtures.
%%% ----------------------------------------------------------------------------

-doc """
Function definitions and signatures in mailbox interface attributes tests.
""".
mb_interface_fun_sig_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun undef_fun_spec/0,
      fun def_fun_spec/0,
      fun undef_mb_fun_sig/0,
      fun def_mb_fun_sig/0,
      fun non_unique_mb_fun_sig/0
    ]]}.

-doc "Mailbox interface type definition tests.".
mb_interface_type_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun undef_mb_type/0,
      fun def_mb_type/0, % This
      fun mb_inline_msg_type/0,
      fun undef_mb_msg_type/0,
      fun def_mb_msg_type/0,
      fun bad_mb_built_in_type/0,
      fun bad_mb_mb_type/0,
      fun bad_mb_type/0,
      fun no_pid_mb_type/0,
      fun pid_mb_type/0,
      fun pid_mb_simple_msg_type/0,
      fun pid_mb_union_msg_type/0
    ]]}.

-doc "Message type definition tests.".
mb_message_type_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun bad_inline_msg_type_tag/0,
      fun undef_inline_msg_type/0,
      fun def_inline_msg_type/0,
      fun bad_inline_msg_built_in_type/0,
      fun bad_inline_msg_msg_type/0,
      fun inline_msg_mb_type/0,
      fun bad_msg_type/0,
      fun bad_msg_type_tag/0,
      fun undef_msg_type/0,
      fun def_msg_type/0,
      fun bad_msg_built_in_type/0,
      fun bad_msg_msg_type/0,
      fun valid_msg_mb_type/0
    ]]}.

-doc "Mailbox usage tests.".
mb_usage_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun use_non_new_mb/0,
      fun use_new_mb/0,
      fun use_new_mb2/0
    ]]}.


%%% ----------------------------------------------------------------------------
%%% Function definitions and signatures in mailbox interface attributes tests.
%%% ----------------------------------------------------------------------------

-doc "Tests function definition with no corresponding function spec.".
%%-spec undef_fun_spec() -> {Erl :: paterl_test_lib:erl(), fun((Erl0 :: paterl_test_lib:erl(), Result :: any()) -> {Desc :: string(), Test :: fun(() -> any())})}.
-spec undef_fun_spec() -> test_x().
undef_fun_spec() -> {
  """
  -module(test).
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test function definition with no corresponding function spec",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Function definition with no corresponding function spec.
        ?assertMatch(
          {_, _, {e_undef__fun_spec, {'fun', 2, {function, f, 0}}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests function definition with corresponding function spec.".
-spec def_fun_spec() -> test_x().
def_fun_spec() -> {
  """
  -module(test).
  -spec f() -> ok.
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test function definition with corresponding function spec",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{specs = Specs}, _Warnings = []} = Result,

        % Function definition with corresponding function spec.
        ?assertMatch(#{{f, 0} := {spec, _, _}}, Specs)
      end}
  end
}.

-doc """
Tests undefined function signature in `-new` and `-use` mailbox interface
attribute.
""".
-spec undef_mb_fun_sig() -> test_x().
undef_mb_fun_sig() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  """,
  fun(_, Result) ->
    {"Test undefined fun reference in -new and -use mailbox interface attribute",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Undefined function signature in -new and -use mailbox interface
        % attribute.
        ?assertMatch(
          {_, _, {e_undef__fun_ref, {'fun', _, {function, f, 0}}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc """
Tests defined function signature in `-new` and `-use` mailbox interface
attribute.
""".
-spec def_mb_fun_sig() -> test_x().
def_mb_fun_sig() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test defined function signature in -new and -use mailbox interface attribute",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types, specs = Specs}, _Warnings = []} = Result,

        % Defined function signature in -new and -use mailbox interface
        % attribute.
        ?assertMatch(#{mb := {mbox, _, {type, _, pid, []}, []}}, Types),
        ?assertMatch(#{
          {f, 0} := {spec, _, [
            {type, _, 'fun', [{type, _, product, []}, {atom, _, ok}]}
          ]}}, Specs)
      end}
  end}.

-doc """
Tests non-unique function signature association with `-new` or `-use` mailbox
interface attribute.
""".
% TODO: This will eventually be removed, which is why I did not include the positive test.
-spec non_unique_mb_fun_sig() -> test_x().
non_unique_mb_fun_sig() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  -use({mb, [f/0]}).
  """,
  fun(_, Result) ->
    {"Test non-unique function signature association with -new or -use mailbox interface attribute",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Non-unique function signature association with -new or -use mailbox
        % interface attribute.
        ?assertMatch(
          {_, _, {e_mb_sig_not_unique, {'fun', _, {function, f, 0}}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Mailbox interface type definition tests.
%%% ----------------------------------------------------------------------------

-doc "Tests undefined mailbox interface type.".
-spec undef_mb_type() -> test_x().
undef_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  """,
  fun(_, Result) ->
    {"Test undefined mailbox interface type",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Undefined mailbox interface type.
        ?assertMatch({_, _, {e_undef__mb_type, {atom, _, mb}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests defined mailbox interface type.".
-spec def_mb_type() -> test_x().
def_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: pid().
  """,
  fun(_, Result) ->
    {"Test defined mailbox interface type",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types}, _Warnings = []} = Result,

        % Defined mailbox interface type.
        ?assertMatch(#{mb := {mbox, _, {type, _, pid, []}, []}}, Types)
      end}
  end}.

-doc "Tests inline message type in mailbox interface type definition.".
-spec mb_inline_msg_type() -> test_x().
mb_inline_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg}.
  """,
  fun(_, Result) ->
    {"Test inline message type in mailbox interface type definition",
      fun() ->
        % Successful result with one warning.
        ?assertMatch({ok, #t_info{}, [{_, [_]}]}, Result),
        {ok, #t_info{types = Types}, _Warnings = [{_, [_]}]} = Result,

        % Inline message type in mailbox interface type definition.
        ?assertMatch(#{
          mb := {mbox, _, {type, _, tuple, [{atom, _, msg}]}, []}
        }, Types)
      end}
  end}.

-doc "Tests undefined message type in mailbox interface type definition.".
-spec undef_mb_msg_type() -> test_x().
undef_mb_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  """,
  fun(_, Result) ->
    {"Test undefined message type in mailbox interface type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Undefined message type in mailbox interface type definition.
        ?assertMatch({_, _, {e__undef_type, {user_type, _, msg, []}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests defined message type in mailbox interface type definition.".
-spec def_mb_msg_type() -> test_x().
def_mb_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: pid().
  """,
  fun(_, Result) ->
    {"Test defined message type in mailbox interface type definition",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types}, _Warnings = []} = Result,

        % Defined message type in mailbox interface type definition.
        ?assertMatch(#{
          msg := {type, _, {type, _, pid, []}, []},
          mb := {mbox, _, {user_type, _, msg, []}, []}
        }, Types)
      end}
  end}.

-doc "Tests bad built-in type in mailbox interface type definition.".
-spec bad_mb_built_in_type() -> test_x().
bad_mb_built_in_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: integer().
  """,
  fun(_, Result) ->
    {"Test bad built-in type in mailbox interface type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad built-in type in mailbox interface type definition.
        ?assertMatch({_, _, {e_bad__msg_type, {type, _, integer, []}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests bad mailbox interface type in mailbox interface type definition.".
-spec bad_mb_mb_type() -> test_x().
bad_mb_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: mb().
  """,
  fun(_, Result) ->
    {"Test bad mailbox interface type in mailbox interface type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad mailbox interface type in mailbox interface type definition.
        ?assertMatch({_, _, {e_bad__msg_type, {user_type, _, mb, []}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests bad term in mailbox interface type definition.".
-spec bad_mb_type() -> test_x().
bad_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: ok.
  """,
  fun(_, Result) ->
    {"Test bad term in mailbox interface type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad term in mailbox interface type definition.
        ?assertMatch({_, _, {e_bad__msg_type, {atom, _, ok}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests missing Pid type in mailbox interface type definition.".
-spec no_pid_mb_type() -> test_x().
no_pid_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg}.
  """,
  fun(_, Result) ->
    {"Test missing Pid type in mailbox interface type definition",
      fun() ->
        % Successful result with one warning.
        ?assertMatch({ok, #t_info{}, [{_, [_]}]}, Result),
        {ok, #t_info{types = Types}, [{_, [Warning = {_, _, Code}]}]} = Result,

        % Missing Pid type in mailbox interface type definition.
        ?assertMatch(
          #{mb := {mbox, _, {type, _, tuple, [{atom, _, msg}]}, []}}, Types
        ),
        ?assertMatch({_, _, {w_no__pid, {atom, _, mb}}}, Warning),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests present Pid type in mailbox interface type definition.".
-spec pid_mb_type() -> test_x().
pid_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg} | pid().
  """,
  fun(_, Result) ->
    {"Test present Pid type in mailbox interface type definition",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types}, _Warnings = []} = Result,

        % Present Pid type in mailbox interface type definition.
        ?assertMatch(#{
          mb := {mbox, _, {type, _, union, [
            {type, _, tuple, [{atom, _, msg}]}, {type, _, pid, []}
          ]}, []}
        }, Types)
      end}
  end}.

-doc """
Tests present Pid type in second-level simple message type in mailbox interface
type definition.
""".
-spec pid_mb_simple_msg_type() -> test_x().
pid_mb_simple_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg_0} | msg_1().
  -type msg_1() :: pid().
  """,
  fun(_, Result) ->
    {"Test present Pid type in second-level simple message type in mailbox interface type definition",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types}, _Warnings = []} = Result,

        % Present Pid type in second-level simple message type in mailbox
        % interface type definition
        ?assertMatch(#{
          mb := {mbox, _, {type, _, union, [
            {type, _, tuple, [{atom, _, msg_0}]}, {user_type, _, msg_1, []}
          ]}, []},
          msg_1 := {type, _, {type, _, pid, []}, []}
        }, Types)
      end}
  end}.

-doc """
Tests present Pid type in second-level union message type in mailbox interface
type definition.
""".
-spec pid_mb_union_msg_type() -> test_x().
pid_mb_union_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg_0} | msg_1().
  -type msg_1() :: {msg_1} | pid().
  """,
  fun(_, Result) ->
    {"Test present Pid type in second-level union message type in mailbox interface type definition",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types}, _Warnings = []} = Result,

        % Present Pid type in second-level union message type in mailbox
        % interface type definition.
        ?assertMatch(#{
          mb := {mbox, _, {type, _, union, [
            {type, _, tuple, [{atom, _, msg_0}]}, {user_type, _, msg_1, []}
          ]}, []},
          msg_1 := {type, _, {type, _, union, [
            {type, _, tuple, [{atom, _, msg_1}]}, {type, _, pid, []}]
          }, []}
        }, Types)
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Message type definition tests.
%%% ----------------------------------------------------------------------------

-doc "Tests bad tag in inline message type definition.".
-spec bad_inline_msg_type_tag() -> test_x().
bad_inline_msg_type_tag() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {5}.
  """,
  fun(_, Result) ->
    {"Test bad tag in inline message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad tag in inline message type definition.
        ?assertMatch({_, _, {e_bad__msg_tag, {integer, _, 5}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests undefined type in inline message type definition.".
-spec undef_inline_msg_type() -> test_x().
undef_inline_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, undef_type()}.
  """,
  fun(_, Result) ->
    {"Test undefined type in inline message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Undefined type in inline message type definition.
        ?assertMatch(
          {_, _, {e__undef_type, {user_type, _, undef_type, []}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests defined type in inline message type definition.".
-spec def_inline_msg_type() -> test_x().
def_inline_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, integer()}.
  """,
  fun(_, Result) ->
    {"Test defined type in inline message type definition",
      fun() ->
        % Successful result with one warning.
        ?assertMatch({ok, #t_info{}, [{_, [_]}]}, Result),
        {ok, #t_info{types = Types}, _Warnings = [{_, [_]}]} = Result,

        % Defined type in inline message type definition
        ?assertMatch(#{
          mb :=
          {mbox, _, {type, _, tuple, [{atom, _, msg}, {type, _, integer, []}]},
            []}
        }, Types)
      end}
  end}.

-doc "Tests bad built-in type in inline message type definition.".
-spec bad_inline_msg_built_in_type() -> test_x().
bad_inline_msg_built_in_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg, binary()}.
  """,
  fun(_, Result) ->
    {"Test bad built-in type in inline message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad built-in type in inline message type definition.
        ?assertMatch(
          {_, _, {e_bad__msg_elem_type, {type, _, binary, []}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Test bad message type in inline message type definition.".
-spec bad_inline_msg_msg_type() -> test_x().
bad_inline_msg_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg_0, msg_1()}.
  -type msg_1() :: {msg_1}.
  """,
  fun(_, Result) ->
    {"Test bad message type in inline message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [_]} = Result,

        % Bad message type in inline message type definition.
        ?assertMatch(
          {_, _, {e_bad__mb_type, {user_type, _, msg_1, []}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests valid mailbox interface type in inline message type definition.".
-spec inline_msg_mb_type() -> test_x().
inline_msg_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: {msg_0, mb()}.
  """,
  fun(_, Result) ->
    {"Test valid mailbox interface type in inline message type definition",
      fun() ->
        % Successful result with one warning.
        ?assertMatch({ok, #t_info{}, [{_, [_]}]}, Result),
        {ok, #t_info{types = Types}, _Warnings = [{_, [_]}]} = Result,

        % Valid mailbox interface type in inline message type definition.
        ?assertMatch(#{
          mb := {mbox, _,
            {type, _, tuple, [{atom, _, msg_0}, {user_type, _, mb, []}]},
            []}
        }, Types)
      end}
  end}.

-doc "Tests bad message type definition.".
-spec bad_msg_type() -> test_x().
bad_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: ok.
  """,
  fun(_, Result) ->
    {"Test bad message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad message type definition.
        ?assertMatch({_, _, {e_bad__msg_type, {atom, _, ok}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests bad tag in message type definition.".
-spec bad_msg_type_tag() -> test_x().
bad_msg_type_tag() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: {5}.
  """,
  fun(_, Result) ->
    {"Test bad tag in message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad tag in message type definition.
        ?assertMatch({_, _, {e_bad__msg_tag, {integer, _, 5}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests undefined type in message type definition.".
-spec undef_msg_type() -> test_x().
undef_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: {msg, undef_type()}.
  """,
  fun(_, Result) ->
    {"Test undefined type in message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Undefined type in message type definition.
        ?assertMatch(
          {_, _, {e__undef_type, {user_type, _, undef_type, []}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests defined type in message type definition.".
-spec def_msg_type() -> test_x().
def_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: {msg, integer()}.
  """,
  fun(_, Result) ->
    {"Test defined type in message type definition",
      fun() ->
        % Successful result with one warning.
        ?assertMatch({ok, #t_info{}, [{_, [_]}]}, Result),
        {ok, #t_info{types = Types}, _Warnings = [{_, [_]}]} = Result,

        % Defined type in message type definition.
        ?assertMatch(#{
          mb := {mbox, _, {user_type, _, msg, []}, []},
          msg := {type, _, {
            type, _, tuple, [{atom, _, msg}, {type, _, integer, []}]},
            []}
        }, Types)
      end}
  end}.

-doc "Tests bad built-in type in message type definition.".
-spec bad_msg_built_in_type() -> test_x().
bad_msg_built_in_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: {msg, binary()}.
  """,
  fun(_, Result) ->
    {"Test bad built-in type in message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad built-in type in message type definition.
        ?assertMatch(
          {_, _, {e_bad__msg_elem_type, {type, _, binary, []}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests bad mailbox interface type in message type definition.".
-spec bad_msg_msg_type() -> test_x().
bad_msg_msg_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg_0().
  -type msg_0() :: {msg_0, msg_1()}.
  -type msg_1() :: {msg_1}.
  """,
  fun(_, Result) ->
    {"Test bad mailbox interface type in message type definition",
      fun() ->
        % Unsuccessful result with one warning.
        ?assertMatch({error, [{_, [_]}], [{_, [_]}]}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = [{_, [_]}]} = Result,

        % Bad mailbox interface type in message type definition.
        ?assertMatch(
          {_, _, {e_bad__mb_type, {user_type, _, msg_1, []}}}, Error
        ),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests valid mailbox interface type in message type definition.".
-spec valid_msg_mb_type() -> test_x().
valid_msg_mb_type() -> {
  """
  -module(test).
  -new({mb, []}).
  -type mb() :: msg().
  -type msg() :: {msg, mb()}.
  """,
  fun(_, Result) ->
    {"Test valid mailbox interface type in message type definition",
      fun() ->
        % Successful result with one warning.
        ?assertMatch({ok, #t_info{}, [{_, [_]}]}, Result),
        {ok, #t_info{types = Types}, _Warnings = [{_, [_]}]} = Result,

        % Valid mailbox interface type in message type definition.
        ?assertMatch(#{
          mb := {mbox, _, {user_type, _, msg, []}, []},
          msg := {type, _,
            {type, _, tuple, [{atom, _, msg}, {user_type, _, mb, []}]},
            []}
        }, Types)
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% Mailbox usage tests.
%%% ----------------------------------------------------------------------------

-doc "Tests invalid use of non-new mailbox interface.".
-spec use_non_new_mb() -> test_x().
use_non_new_mb() -> {
  """
  -module(test).
  -use({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test invalid use of non-new mailbox interface",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Invalid use of non-new mailbox interface.
        ?assertMatch({_, _, {e_no__mb_new, {atom, _, mb}}}, Error),
        ?debugMsg(paterl_types:format_error(Code))
      end}
  end}.

-doc "Tests valid use of new mailbox interface.".
-spec use_new_mb() -> test_x().
use_new_mb() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> ok.
  """,
  fun(_, Result) ->
    {"Test valid use of new mailbox interface",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, #t_info{}, []}, Result),
        {ok, #t_info{types = Types}, _Warnings = []} = Result,

        % Valid use of new mailbox interface.
        ?assertMatch(#{mb := {mbox, _, {type, _, pid, []}, []}}, Types)
      end}
  end}.

use_new_mb2() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  -use({mb, [f/0, g/0]}).
  -type mb() :: pid().
  -spec f() -> ok.
  f() -> ok.
  -spec g() -> ok.
  g() -> ok.
  """,
  fun(_, Result) ->
    {"Test valid use of new mailbox interface",
      fun() ->
        ?TRACE("Result = ~p", [Result])
        % Successful result with no warnings.
%%        ?assertMatch({ok, #t_info{}, []}, Result),
%%        {ok, #t_info{types = Types}, _Warnings = []} = Result,
%%
%%        % Valid use of new mailbox interface.
%%        ?assertMatch(#{mb := {mbox, _, {type, _, pid, []}, []}}, Types)
      end}
  end}.

%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc "Configures test input.".
-spec startup(paterl_test_lib:erl()) -> paterl_types:result().
startup(Erl) ->
  % Parse Erlang code into its abstract syntax and extract type information.
  paterl_types:table(paterl_test_lib:erl_forms(Erl)).


%%erl_syntax:revert(erl_syntax:type_union([erl_syntax:tuple_type([]), erl_syntax:type_application(erl_syntax:atom(pid), [])]))