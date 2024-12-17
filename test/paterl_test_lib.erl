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
-module(paterl_test_lib).
-moduledoc """
Module description
""".
-author("duncan").

%%% Includes.
-include("log.hrl").
-include("paterl_syntax.hrl").

%%% Public API.
-export([erl_forms/1, expand_erl_forms/2]).
-export([banner_msg/1]).

%%% Public types.
-export_type([test/0, pair/2]).
-export_type([erl/0, mb_anno_spec/0, erl_spec/0]).
-compile(export_all).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc """
Test object that carries a
[`Title`](https://www.erlang.org/doc/apps/eunit/chapter.html#titles) and a
[simple test object](https://www.erlang.org/doc/apps/eunit/chapter.html#simple-test-objects)
function.
""".
-type test() :: {Title :: string(), SimpleTestObject :: fun(() -> any())}.

-doc """
Test pair containing an `Input` argument and an extended
[instantiator](https://www.erlang.org/doc/apps/eunit/chapter#fixtures)
function `TestX`.

The [`pair(Input, Result)`](`t:pair/2`) type describes an input pair constructor
for [`foreachx`](https://www.erlang.org/doc/apps/eunit/chapter#fixtures) test
fixtures.
Its use is intended for test modules wanting to concretize
[`pair(Input, Result)`](`t:pair/2`) to a type returned by test functions.
Conventionally, this concrete type should be named `testx()` to reflect its
association with
[`foreachx`](https://www.erlang.org/doc/apps/eunit/chapter#fixtures) test
fixtures.

### Example

The following encodes a test returned by the function `test/0`.
```erlang
test() ->
  Input = % Test input...
  {Input,
  fun(Input, Result) ->
    {"Test title",
      fun() ->
        % Test body.
      end}
  end}.
```

Test [fixtures](https://www.erlang.org/doc/apps/eunit/chapter#fixtures)
are described thus:
```erlang
fixture_test_() ->
  {foreachx, fun startup/1, [
    T() || T <- [
      fun test/0,
    ]]}.
```

The function `startup/1` processes `Input` and returns `Result`. `Input` and
`Result` are available within the extended instantiator function in `test/0`.
See more on [fixtures](https://www.erlang.org/doc/apps/eunit/chapter#fixtures).
""".
-type pair(Input, Result) :: {Input, TestX :: fun((Input, Result) -> test())}.

-doc "Erlang code as text.".
-type erl() :: string().

%%-type mb_spec() :: {assert, Pattern :: string(), MbName :: paterl_syntax:name()}
%%| {new, MbName :: paterl_syntax:name()}
%%| {use, MbName :: paterl_syntax:name()}.

-type mb_anno_spec() :: {Key :: atom(), paterl_syntax:mb_anno()}.

-type erl_spec() :: {erl(), [mb_anno_spec()]}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec erl_forms(erl()) -> erl_syntax:forms().
erl_forms(Erl) ->
  merl:quote(Erl).

-spec expand_erl_forms(erl(), [erl_spec()]) -> erl_syntax:forms().
expand_erl_forms(Erl, ErlSpecs) ->
  % Expand
  ExpErlSpecs = [{Key, expand(Value)} || {Key, Value} <- ErlSpecs],
  if length(ExpErlSpecs) =/= length(ErlSpecs) ->
    error({badarg, ErlSpecs});
    true -> ok
  end,

  % Handles the case when Erl contains multiple expressions that need to be
  % reverted; revert does not work on expression lists but only individual
  % expressions, which is why these need to be reverted separately.
  case merl:qquote(Erl, ExpErlSpecs) of
    Trees when is_list(Trees) ->
      [erl_syntax:revert(Tree) || Tree <- Trees];
    Tree ->
      erl_syntax:revert(Tree)
  end.


banner_msg(Text) ->
  Tail = [$= || _ <- lists:seq(0, 80 - 4 - length(Text))],
  [10, 10, $=, $=, $ , Text, $ , Tail, 10, 10].


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%%-spec expand_assert(string()) -> erl_syntax:forms().
%%expand_assert(Regex) ->
%%  String = merl:quote("\"" ++ Regex ++ "\""),
%%  merl:qquote("{state, '@regex'}", [{regex, String}]).


%%HERE: expand macros for new, use, and assert separately.


-spec expand(paterl_syntax:mb_anno()) -> erl_syntax:syntaxTree().
%%expand({assert, Pattern, MbName}) ->
%%  erl_syntax:tuple([
%%    erl_syntax:atom(state), erl_syntax:string(Pattern), erl_syntax:atom(MbName)
%%  ]);
expand({assert, Pattern}) ->
  erl_syntax:tuple([erl_syntax:atom(state), erl_syntax:string(Pattern)]);
expand({MbMod, MbName}) when MbMod =:= ?MOD_NEW; MbMod =:= ?MOD_USE ->
  erl_syntax:tuple([erl_syntax:atom(MbMod), erl_syntax:atom(MbName)]);
expand({Key = ?ANNO_AS, MbName}) when is_atom(MbName) ->
  erl_syntax:tuple([erl_syntax:atom(Key), erl_syntax:atom(MbName)]);
expand({Key = ?ANNO_EXPECTS, MbName, Pattern})
  when is_atom(MbName), is_list(Pattern) ->
  erl_syntax:tuple([
    erl_syntax:atom(Key), erl_syntax:atom(MbName), erl_syntax:string(Pattern)
  ]);
expand(MbAnno) ->
  error({badarg, MbAnno}).


