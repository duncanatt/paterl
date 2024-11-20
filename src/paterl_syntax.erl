%%
%% %CopyrightBegin%
%%
%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(paterl_syntax).
-moduledoc """
Erlang syntactic subset and mailbox interface well-formedness syntax checks.
""".
-author("duncan").

%%% Includes.
-include("log.hrl").
-include("paterl_lib.hrl").

%%% Public API.
-export([module/1]).
-export([format_error/1]).
-export([get_file/1, set_anno/2, name/2, fun_reference/2]).

%%% Public types.
-export_type([form/0, forms/0, clause/0, expr/0, type/0, tree/0, anno/0]).
-export_type([name/0, fun_ref/0]).
-export_type([result/0]).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Error types.

%% Incorrect mailbox interface definition.
-define(E_BAD__MB_DEF, e_bad__mb_def).

%% Incorrect function reference in mailbox interface definition.
-define(E_BAD__FUN_REF, e_bad__fun_ref).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Abstract form.".
-type form() :: erl_parse:abstract_form().

-doc "Abstract forms.".
-type forms() :: [form()].

-doc "Abstract clause.".
-type clause() :: erl_parse:abstract_clause().

-doc "Abstract expression.".
-type expr() :: erl_parse:abstract_expr().

-doc "Abstract type.".
-type type() :: erl_parse:abstract_type().

-doc "Abstract tree.".
-type tree() :: form() | clause() | expr() | type().

-doc "Abstract syntax annotation".
-type anno() :: erl_anno:anno().

-doc "Generic syntactic object name.".
-type name() :: atom().

-doc "Fun `name/arity` reference.".
-type fun_ref() :: {name(), arity()}.

-doc "Return result.".
-type result() :: {ok, Forms :: forms(), Warnings :: paterl_errors:warnings()} |
{error, Errors :: paterl_errors:errors(), Warnings :: paterl_errors:warnings()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Checks that the Erlang `Forms` are contained within the syntactic subset
accepted by **Pat**.

The function also checks the well-formedness of mailbox interface definitions
encoded as the `-new` and `-use` wild attributes.

### Returns
- `{ok, Forms, Warnings}` if check is successful
- `{error, Errors, Warnings}` if check fails
""".
-spec module(forms()) -> result().
module(Forms) when is_list(Forms) ->

  % Check Erlang syntactic subset.
  Analysis = check_forms(Forms, #analysis{file = get_file(Forms)}),

  % Check well-formedness of mailbox interface definition attributes.
  Analysis0 = check_mb_attribs(Forms, Analysis),
  paterl_lib:return(Analysis0).

-doc """
Extracts the file name from Erlang `Forms`.

### Returns
- file name if `-file()` attribute is found in `Forms`
- "nofile" if `-file()` attribute is missing from `Forms`
""".
-spec get_file(Forms :: forms()) -> string().
get_file([Form | _]) ->
  maybe
    attribute ?= erl_syntax:type(Form),
    {attribute, {file, {File, _}}} ?= erl_syntax_lib:analyze_form(Form),
    File
  else
  % Form is not an attribute or not a file attribute.
    _ -> "nofile"
  end.

-doc """
Sets the annotation of Erlang `Tree`.

The [annotation](`m:erl_anno`) is only set for the root of `Tree`.

### Returns
- updated `Tree`
""".
-spec set_anno(Tree, Anno) -> Tree0
  when
  Tree :: erl_syntax:syntaxTree(),
  Anno :: anno(),
  Tree0 :: tree().
set_anno(Tree, Anno) ->
  erl_syntax:revert(erl_syntax:set_pos(Tree, Anno)).

-doc """
Creates an atomic name abstract syntax representation with the specified
[annotation](`m:erl_anno`).

### Returns.
- atomic name abstract syntax representation
""".
-spec name(Name, Anno) -> Tree0
  when
  Name :: name(),
  Anno :: anno(),
  Tree0 :: tree().
name(Name, Anno) when is_atom(Name) ->
  erl_syntax:revert(erl_syntax:set_pos(erl_syntax:atom(Name), Anno)).

-doc """
Creates an implicit fun reference abstract syntax representation with the
specified [annotation](`m:erl_anno`).

### Returns
- implicit fun reference abstract syntax representation
""".
-spec fun_reference(FunRef, Anno) -> Tree0
  when
  FunRef :: fun_ref(),
  Anno :: anno(),
  Tree0 :: tree().
fun_reference({Name, Arity}, Anno) when is_atom(Name), is_integer(Arity) ->
  erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:implicit_fun(erl_syntax:atom(Name), erl_syntax:integer(Arity)),
      Anno
    )).


%%% ----------------------------------------------------------------------------
%%% Erlang syntactic subset checks.
%%% ----------------------------------------------------------------------------

-doc """
Checks that the Erlang `Forms` are contained within the syntactic subset
accepted by **Pat**.
""".
-spec check_forms(Forms, Analysis) -> Analysis0
  when
  Forms :: forms(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_forms(Forms, Analysis) ->
  Analysis#analysis{status = ok, result = Forms}.


%%% ----------------------------------------------------------------------------
%%% Mailbox interface definition wild attribute checks.
%%% ----------------------------------------------------------------------------

-doc """
Checks the well-formedness of mailbox interface `-new` and `-use` wild
attributes.
""".
-spec check_mb_attribs(Forms, Analysis) -> Analysis0
  when
  Forms :: forms(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_mb_attribs(Forms, Analysis) when is_list(Forms) ->
  lists:foldl(fun check_attr3/2, Analysis, Forms).

-doc """
Checks the well-formedness of mailbox interface `-new` and `-use` wild
attributes, skipping the rest.
""".
-spec check_attr(Attr, Analysis) -> Analysis0
  when
  Attr :: form(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_attr(Attr = {attribute, ANNO, Modality, Value}, Analysis)
  when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
  ?TRACE("Check mailbox interface definition '~s'.", [erl_prettypr:format(Attr)]),

  case Value of
    {MbName, Terms} when is_atom(MbName), is_list(Terms) ->
      % Valid mailbox interface definition wild attribute.
      CheckFunRefFun =
        fun(Term, Analysis0) -> check_fun_ref(Term, ANNO, Analysis0) end,

      % Check well-formedness of function references.
      lists:foldl(CheckFunRefFun, Analysis, Terms);
    _ ->
      % Bad mailbox interface definition attribute.
      ?ERROR("Bad mailbox interface definition '~s'", [
        erl_prettypr:format((Attr))
      ]),
      ?pushError(?E_BAD__MB_DEF, Attr, Analysis)
  end;
check_attr(_Form, Analysis) ->
  ?TRACE("Skip form '~s'.", [erl_prettypr:format(_Form)]),
  Analysis.

check_attr2(Form, Analysis) ->
  case erl_syntax:type(Form) of
    attribute ->
      case erl_syntax_lib:analyze_attribute(Form) of
        {Modality, Value} when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
          case Value of
            {MbName, Terms} when is_atom(MbName), is_list(Terms) ->
              % Valid mailbox interface definition wild attribute.
              ANNO = erl_syntax:get_pos(Form),
              CheckFunRefFun =
                fun(Term, Analysis0) -> check_fun_ref(Term, ANNO, Analysis0) end,

              % Check well-formedness of function references.
              lists:foldl(CheckFunRefFun, Analysis, Terms);
            _ ->
              % Bad mailbox interface definition attribute.
              ?ERROR("Bad mailbox interface definition '~s'", [
                erl_prettypr:format((Form))
              ]),
              ?pushError(?E_BAD__MB_DEF, Form, Analysis)
          end;
        _ ->
          % Other attribute.
          ?TRACE("Skip attribute '~s'.", [erl_prettypr:format(Form)]),
          Analysis
      end;
    _ ->
      % Other form.
      ?TRACE("Skip form '~s'.", [erl_prettypr:format(Form)]),
      Analysis
  end.

-doc """
Checks the well-formedness of mailbox interface `-new` and `-use` wild
attributes, skipping the rest.
""".
-spec check_attr3(Form, Analysis) -> Analysis0
  when
  Form :: form(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_attr3(Form, Analysis) ->
  maybe
    attribute ?= erl_syntax:type(Form),
    {Modality, Value} ?= erl_syntax_lib:analyze_attribute(Form),
    true ?= Modality =:= ?M_NEW orelse Modality =:= ?M_USE,

    ?TRACE("Check mailbox interface definition '~s'.", [erl_prettypr:format(Form)]),
    case Value of
      {MbName, Terms} when is_atom(MbName), is_list(Terms) ->
        % Check well-formedness of function references.
        check_fun_refs(Terms, erl_syntax:get_pos(Form), Analysis);
      _ ->
        % Bad mailbox interface definition attribute.
        ?ERROR("Bad mailbox interface definition '~s'", [erl_prettypr:format((Form))]),
        ?pushError(?E_BAD__MB_DEF, Form, Analysis)
    end
  else
    _ ->
      % Non-mailbox interface definition wild attribute.
      ?TRACE("Skip ~s '~s'.", [erl_syntax:type(Form), erl_prettypr:format(Form)]),
      Analysis
  end.

-doc "Checks the well-formedness of fun references lists.".
-spec check_fun_refs(Terms, Anno, Analysis) -> Analysis0
  when
  Terms :: [term()],
  Anno :: anno(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_fun_refs(Terms, Anno, Analysis) when is_list(Terms) ->
  Fun = fun(Term, Analysis0) -> check_fun_ref(Term, Anno, Analysis0) end,
  lists:foldl(Fun, Analysis, Terms).

-doc "Checks the well-formedness of fun references.".
-spec check_fun_ref({Name, Arity}, Anno, Analysis) -> Analysis0
  when
  Name :: atom(),
  Arity :: non_neg_integer(),
  Anno :: anno(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_fun_ref({Name, Arity}, Anno, Analysis)
  when is_atom(Name), is_integer(Arity) ->
  % Valid function reference structure. Create implicit Erlang fun syntax and
  % check arity.
  Fun = set_anno(
    erl_syntax:implicit_fun(erl_syntax:atom(Name), erl_syntax:integer(Arity)),
    Anno
  ),
  check_implicit_fun(Fun, Analysis);
check_fun_ref(Term, ANNO, Analysis) ->
  % Bad function reference structure.
  Term0 = set_anno(erl_syntax:abstract(Term), ANNO),
  ?ERROR("Bad fun reference '~s',", [erl_prettypr:format(Term0)]),
  ?pushError(?E_BAD__FUN_REF, Term0, Analysis).

-doc "Checks the arity of fun references encoded as implicit funs.".
-spec check_implicit_fun(Fun, Analysis) -> Analysis0
  when
  Fun :: expr(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_implicit_fun(_Fun = {'fun', _, {function, Name, Arity}}, Analysis)
  when is_atom(Name), Arity >= 0, Arity =< 255 ->
  % Valid function signature.
  ?TRACE("Valid fun reference '~s'.", [erl_prettypr:format(_Fun)]),
  Analysis;
check_implicit_fun(Fun = {'fun', _, {function, _, _}}, Analysis) ->
  % Bad function signature.
  ?ERROR("Bad fun reference '~s'.", [erl_prettypr:format(Fun)]),
  ?pushError(?E_BAD__FUN_REF, Fun, Analysis).


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

-doc """
Formats `Detail` to human-readable form.

### Returns
- stringified error as deep list of [`chars()`](`t:io_lib:chars/0`)s
""".
-spec format_error(Detail :: paterl_lib:detail()) -> io_lib:chars().
format_error({?E_BAD__MB_DEF, Node}) ->
  io_lib:format(
    "bad mailbox interface definition '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__FUN_REF, Node}) ->
  io_lib:format(
    "mailbox interface definition has bad fun reference '~s'",
    [erl_prettypr:format(Node)]
  ).