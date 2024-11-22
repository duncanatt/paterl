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
-module(paterl_syntax_tests).
-moduledoc "`paterl_syntax` module tests.".
-author("duncan").

%%% Includes.
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("paterl_lib.hrl").

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Extended test.".
-type test_x() :: paterl_test_lib:pair(paterl_test_lib:erl(), paterl_syntax:result()).


%%% ----------------------------------------------------------------------------
%%% Fixtures.
%%% ----------------------------------------------------------------------------

-doc "Mailbox interface attributes well-formedness of tests.".
mb_interface_attribute_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun bad_mb_def/0,
      fun bad_fun_ref_list/0,
      fun valid_mb_def/0,
      fun unused_mb_def/0,
      fun bad_fun_ref_form/0,
      fun bad_fun_ref_arity/0,
      fun valid_fun_ref_arity/0
    ]]}.


%%% ----------------------------------------------------------------------------
%%% Mailbox interface attributes well-formedness tests.
%%% ----------------------------------------------------------------------------

-doc "Tests bad mailbox interface definition attribute.".
-spec bad_mb_def() -> test_x().
bad_mb_def() -> {
  """
  -module(test).
  -new(f/0).
  """,
  fun(_, Result) ->
    {"Test bad mailbox interface definition attribute",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [_], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Bad mailbox interface definition attribute.
        ?assertMatch(
          {_, _, {e_bad__mb_def, {attribute, _, new, {f, 0}}}}, Error
        ),
        ?debugMsg(paterl_syntax:format_error(Code))
      end}
  end}.

-doc "Tests bad fun reference list in mailbox interface definition attribute.".
-spec bad_fun_ref_list() -> test_x().
bad_fun_ref_list() -> {
  """
  -module(test).
  -new({mb, f/0}).
  """,
  fun(_, Result) ->
    {"Test bad fun reference list in mailbox interface definition attribute",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [_], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Bad fun reference list in mailbox interface definition attribute.
        ?assertMatch(
          {_, _, {e_bad__mb_def, {attribute, _, new, {mb, {f, 0}}}}}, Error
        ),
        ?debugMsg(paterl_syntax:format_error(Code))
      end}
  end}.

-doc """
Tests valid fun reference list in mailbox interface definition attribute.
""".
-spec valid_mb_def() -> test_x().
valid_mb_def() -> {
  """
  -module(test).
  -new({mb, [f/0]}).
  """,
  fun(_, Result) ->
    {"Test valid fun reference list in mailbox interface definition attribute",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, [_, _], []}, Result),
        {ok, [Module, New], _Warnings = []} = Result,

        % Valid fun reference list in mailbox interface definition attribute.
        ?assertMatch({attribute, _, module, test}, Module),
        ?assertMatch({attribute, _, new, {mb, [{f, 0}]}}, New)
      end}
  end}.

-doc "Tests valid unused mailbox interface definition attribute.".
-spec unused_mb_def() -> test_x().
unused_mb_def() -> {
  """
  -module(test).
  -new({mb, []}).
  """,
  fun(_, Result) ->
    {"Test valid unused mailbox interface definition attribute",
      fun() ->
        ?TRACE("Result = ~p", [Result]),
        % Successful result with one warning.
        ?assertMatch({ok, [_, _], [{_, [_]}]}, Result),
        {ok, [Module, New], [{_, [Warning = {_, _, Code}]}]} = Result,

        % Valid unused mailbox interface definition attribute.
        ?assertMatch({attribute, _, module, test}, Module),
        ?assertMatch({attribute, _, new, {mb, []}}, New),
        ?assertMatch(
          {_, _,
            {w_no__mb_fun_ref, {attribute, _, new, {mb, []}}}},
          Warning
        ),
        ?debugMsg(paterl_syntax:format_error(Code))
      end}
  end}.

-doc "Test bad fun reference form in mailbox interface definition attribute.".
-spec bad_fun_ref_form() -> test_x().
bad_fun_ref_form() -> {
  """
  -module(test).
  -new({mb, [{f, bad}, {5, bad}]}).
  """,
  fun(_, Result) ->
    {"Test bad fun reference form in mailbox interface definition attribute",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_, _]}], []}, Result),
        {error,
          [{_, [Error0 = {_, _, Code0}, Error1 = {_, _, Code1}]}],
          _Warnings = []} = Result,

        % Bad fun reference form in mailbox interface definition attribute.
        ?assertMatch(
          {_, _, {e_bad__fun_ref, {tuple, _, [{atom, _, f}, {atom, _, bad}]}}},
          Error0
        ),
        ?assertMatch(
          {_, _,
            {e_bad__fun_ref, {tuple, _, [{integer, _, 5}, {atom, _, bad}]}}},
          Error1
        ),
        ?debugMsg(paterl_syntax:format_error(Code0)),
        ?debugMsg(paterl_syntax:format_error(Code1))
      end}
  end}.

-doc "Test bad fun reference arity in mailbox interface definition attribute.".
-spec bad_fun_ref_arity() -> test_x().
bad_fun_ref_arity() -> {
  """
  -module(test).
  -new({mb, [f/100, g/256, h/1]}).
  """,
  fun(_, Result) ->
    {"Test bad fun reference arity in mailbox interface definition attribute",
      fun() ->
        % Unsuccessful result with no warnings.
        ?assertMatch({error, [{_, [_]}], []}, Result),
        {error, [{_, [Error = {_, _, Code}]}], _Warnings = []} = Result,

        % Bad fun reference arity in mailbox interface definition attribute.
        ?assertMatch(
          {_, _, {e_bad__fun_ref, {'fun', _, {function, g, 256}}}}, Error
        ),
        ?debugMsg(paterl_syntax:format_error(Code))
      end}
  end}.

-doc "Test valid fun reference arity in mailbox interface definition attribute.".
-spec valid_fun_ref_arity() -> test_x().
valid_fun_ref_arity() -> {
  """
  -module(test).
  -new({mb, [f/100, g/255, h/1]}).
  """,
  fun(_, Result) ->
    {"Test valid fun reference arity in mailbox interface definition attribute",
      fun() ->
        % Successful result with no warnings.
        ?assertMatch({ok, [_, _], []}, Result),
        {ok, [Module, New], _Warnings = []} = Result,

        % Valid fun reference arity in mailbox interface definition attribute.
        ?assertMatch({attribute, _, module, test}, Module),
        ?assertMatch(
          {attribute, _, new, {mb, [{f, 100}, {g, 255}, {h, 1}]}}, New
        )
      end}
  end}.


%%% ----------------------------------------------------------------------------
%%% API tests.
%%% ----------------------------------------------------------------------------

% API function tests should be written first by the function name followed by
% the test that they describe.
% The description should include the name of the function as the second word:
% e.g. Test <function name> <test description>.

-doc """
Tests [`get_file()`](`paterl_syntax:get_file/1`) found file name in forms.
""".
-spec get_file__found_file_test_() -> paterl_test_lib:test().
get_file__found_file_test_() -> {
  "Tests get_file_name found file name in forms",
  fun() ->
    % Create file attribute.
    FileInfo = erl_syntax:tuple([
      erl_syntax:string("file name"), erl_syntax:integer(0)
    ]),
    Attr = erl_syntax:revert(
      erl_syntax:attribute(erl_syntax:atom(file), [FileInfo])
    ),

    % Found file name in forms.
    ?assertMatch("file name", paterl_syntax:get_file([Attr]))
  end}.

-doc """
Tests [`get_file()`](`paterl_syntax:get_file/1`) missing file name in forms.
""".
-spec get_file__missing_file_test_() -> paterl_test_lib:test().
get_file__missing_file_test_() -> {
  "Test get_file missing file name in forms.",
  fun() ->
    % Create module attribute.
    Attr = erl_syntax:revert(
      erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(test)])
    ),

    % Missing file name in forms.
    ?assertMatch("nofile", paterl_syntax:get_file([Attr]))
  end}.

-doc """
Tests [`get_file()`](`paterl_syntax:get_file/1`) bad forms.
""".
-spec get_file__bad_form_test_() -> paterl_test_lib:test().
get_file__bad_form_test_() -> {
  "Tests get_file bad forms",
  fun() ->
    % Create non-attribute term.
    Term = erl_syntax:revert(erl_syntax:atom(bad)),

    % Bad forms.
    ?assertMatch("nofile", paterl_syntax:get_file([Term]))
  end
}.

-doc """
Tests [`set_anno()`](`paterl_syntax:set_anno/2`) valid abstract syntax tree.
""".
set_anno_valid_tree_test_() -> {
  "Test set_anno valid abstract syntax tree",
  fun() ->
    Tree = erl_syntax:tuple([erl_syntax:atom(atom)]),
    Tree0 = paterl_syntax:set_anno(Tree, erl_anno:new(1)),
    ANNO = erl_syntax:get_pos(Tree0),

    % Valid syntax tree annotation.
    ?assertMatch(1, erl_anno:location(ANNO))
  end
}.


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-spec startup(paterl_test_lib:erl()) -> paterl_syntax:result().
startup(Erl) ->
  % Parse Erlang code into its abstract syntax and extract type information.
  paterl_syntax:module(paterl_test_lib:erl_forms(Erl)).