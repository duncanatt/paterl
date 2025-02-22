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
-include("paterl_syntax.hrl").

%%% Public API.
-export([module/1, format_error/1]).
-export([get_file/1, set_anno/2]).
-export([name/2, fun_reference/2, mb_anno/3, mb_anno/1, mb_anno_name/1, mb_anno_args/1, is_mb_anno/1, application/3]).
-export([get_fun_ref/1, get_fun_ref_mfa/1]).

-ifdef(test).
-export([is_atom_value/2, is_mb_anno2/1]).
-endif.

%%% Public types.
-export_type([form/0, forms/0, clause/0, expr/0, type/0, pattern/0, tree/0]).
-export_type([anno/0]).
-export_type([name/0, fun_ref/0, mb_anno/0]).
-export_type([result/0]).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Error types.

%% Mailbox interface definition invalid.
-define(E_BAD__MB_DEF, e_bad__mb_def).

%% Function reference in mailbox interface definition invalid.
-define(E_BAD__FUN_REF, e_bad__fun_ref).

%% Mailbox interface not associated with fun references.
-define(W_NO__MB_FUN_REF, w_no__mb_fun_ref).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "An `m:erl_parse` Abstract form.".
-type form() :: erl_parse:abstract_form().

-doc "Abstract forms.".
-type forms() :: [form()].

-doc "Abstract clause.".
-type clause() :: erl_parse:abstract_clause().

-doc "Abstract expression.".
-type expr() :: erl_parse:abstract_expr().

-doc "Abstract type.".
-type type() :: erl_parse:abstract_type().

-doc "Abstract pattern.".
-type pattern() :: tuple().

-doc "Abstract tree.".
-type tree() :: form() | clause() | expr() | type() | pattern().

-doc "Abstract syntax annotation.".
-type anno() :: erl_anno:anno().

-doc "Generic syntactic object name.".
-type name() :: atom().

-doc "Fun `name/arity` reference.".
-type fun_ref() :: {name(), arity()}.

-doc "Mailbox annotation.".
-type mb_anno() :: {?ANNO_NEW, anno(), MbName :: name()} |
{?ANNO_USE, anno(), MbName :: name()} |
{?ANNO_AS, anno(), MbName :: name()} |
{?ANNO_EXPECTS, anno(), MbName :: name(), Pattern :: string()}.

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
- `{ok, Forms, Warnings}` if syntax check is successful
- `{error, Errors, Warnings}` otherwise
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

[`Anno`](`m:erl_anno`) is only set for the root of `Tree`.

### Returns
- updated `Tree`
""".
-spec set_anno(Tree, Anno) -> Tree0
  when
  Tree :: erl_syntax:syntaxTree() | tree(),
  Anno :: anno(),
  Tree0 :: erl_syntax:syntaxTree() | tree().
set_anno(Tree, Anno) ->
  erl_syntax:revert(erl_syntax:set_pos(Tree, Anno)).

-doc """
Creates an atom name abstract syntax representation with the specified
[`Anno`](`m:erl_anno`).

### Example
`name(Name, Anno)`, where `Name` = `name` and `Anno` = `erl_anno:new(0)`
represents the atom `name`.

### Returns
- atom name abstract syntax representation
""".
-spec name(Name, Anno) -> Tree
  when
  Name :: name(),
  Anno :: anno(),
  Tree :: tree().
name(Name, Anno) when is_atom(Name) ->
  set_anno(erl_syntax:atom(Name), Anno).

-doc """
Creates an implicit fun reference abstract syntax representation with the
specified [annotation](`m:erl_anno`).

### Example
`fun_reference(FunRef, Anno)`, where `FunRef` = `{name, 2}` and `Anno` =
`erl_anno:new(0)` represents the implicit fun reference `fun name/2`.

### Returns
- implicit fun reference abstract syntax representation
""".
-spec fun_reference(FunRef, Anno) -> Tree
  when
  FunRef :: fun_ref(),
  Anno :: anno(),
  Tree :: tree().
fun_reference({Name, Arity}, Anno) when is_atom(Name), is_integer(Arity) ->
  set_anno(
    erl_syntax:implicit_fun(erl_syntax:atom(Name), erl_syntax:integer(Arity)),
    Anno
  ).

-doc """
Creates a mailbox annotation macro abstract syntax representation with the
specified [annotation](`m:erl_anno`).

### Example
- `mb_anno(Name, Args, Anno)`, where `Name` = `'@as'`, `Args` = `[]`, and
`Anno` = `erl_anno:new(0)` represents the macro `?as()`.
- `mb_anno(Name, Args, Anno)`, where `Name` = `'@as'`, `Args` =
`[2, three, "four"]`, and `Anno` = `erl_anno:new(0)` represents the macro
`?as(2, three, "four")`.

### Returns
- mailbox annotation macro abstract syntax representation
""".
-spec mb_anno(Name, Args, Anno) -> Tree
  when
  Name :: ?ANNO_NEW | ?ANNO_USE | ?ANNO_AS | ?ANNO_EXPECTS,
  Args :: [term()],
  Anno :: anno(),
  Tree :: erl_syntax:syntaxTree().
mb_anno(Name, Args, Anno) when is_atom(Name), is_list(Args) ->
  % The macro tree is not revertible since it is not an
  % erl_parse:abstract_expr(), but an erl_syntax:syntaxTree() abstraction.
  % Inner expressions are reverted.
  Name1 = case atom_to_list(Name) of [$@ | Rest] -> Rest; Name0 -> Name0 end,
  set_anno(
    erl_syntax:macro(
      erl_syntax:atom(Name1), [erl_syntax:abstract(Arg) || Arg <- Args]
    ),
    Anno
  ).

-doc """
Creates a mailbox annotation macro abstract syntax representation with the
specified [annotation](`m:erl_anno`).

### Example
- `mb_anno(MbAnno)`, where `MbAnno` = `{'@new', erl_anno:new(0), name}`
represents the macro `?new(name)`.
- `mb_anno(MbAnno)`, where `MbAnno` = `{'@use', erl_anno:new(0), name}`
represents the macro `?use(name)`.
- `mb_anno(MbAnno)`, where `MbAnno` = `{'@as', erl_anno:new(0), name}`
represents the macro `?as(name)`.
- `mb_anno(MbAnno)`, where
`MbAnno` = `{'@expects', erl_anno:new(0), name, "Msg*"}` represents the macro
`?expects(name, "Msg")`.

### Returns
- mailbox annotation macro abstract syntax representation
""".
-spec mb_anno(MbAnno) -> Tree
  when
  MbAnno :: mb_anno(),
  Tree :: erl_syntax:syntaxTree().
mb_anno(MbAnno) when is_tuple(MbAnno), tuple_size(MbAnno) > 1 ->
  [Name | Args] = tuple_to_list(MbAnno), % Should pattern match to a list with at least three args.
%%  [Name, Anno | Args] = tuple_to_list(MbAnno), % Should pattern match to a list with at least three args.
  mb_anno(Name, Args, 0). % TODO: Fix this when we include the actual annotation position in the annotation information.
%%  mb_anno(Name, Args, Anno). % TODO: Fix this when we include the actual annotation position in the annotation information.

-doc """
Returns the concrete arguments of the mailbox annotation abstract syntax
representation.

### Example
- `mb_anno_args(MbAnno)`, where `MbAnno` =
`erl_syntax:tuple([erl_syntax:atom('@as'), erl_syntax:atom(name)])` returns
`[name]`.
- `mb_anno_args(MbAnno)`, where `MbAnno` =
`erl_syntax:tuple([erl_syntax:atom('@expects'), erl_syntax:atom(name),
erl_syntax:string("Msg*")])` returns `[name, "Msg*"]`.

### Returns
- concrete arguments of the mailbox annotation abstract syntax representation
- `{badarg, MbAnno}` when `MbAnno` does not represent a mailbox annotation
""".
-spec mb_anno_args(MbAnno :: paterl_syntax:expr()) -> [term()].
mb_anno_args(MbAnno) ->
  maybe
    true ?= is_mb_anno(MbAnno),
    [_ | Elems] = erl_syntax:tuple_elements(MbAnno),
    [erl_syntax:concrete(Elem) || Elem <- Elems]
  else
    false -> error({badarg, MbAnno})
  end.


-doc """
Returns the concrete name of the mailbox annotation abstract syntax
representation.

### Example
- `mb_anno_name(MbAnno)`, where `MbAnno` =
`erl_syntax:tuple([erl_syntax:atom('@as'), erl_syntax:atom(name)])` returns
`'@as'`.
- `mb_anno_name(MbAnno)`, where `MbAnno` =
`erl_syntax:tuple([erl_syntax:atom('@expects'), erl_syntax:atom(name),
erl_syntax:string("Msg*")])` returns `'@expects'`.

### Returns
- concrete name of the mailbox annotation abstract syntax representation
- `{badarg, MbAnno}` when `MbAnno` does not represent a mailbox annotation
""".
-spec mb_anno_name(MbAnno :: paterl_syntax:expr()) -> ?ANNO_NEW | ?ANNO_USE | ?ANNO_AS | ?ANNO_EXPECTS.
mb_anno_name(MbAnno) ->
  maybe
    true ?= is_mb_anno(MbAnno),
    [Name | _] = erl_syntax:tuple_elements(MbAnno),
    erl_syntax:atom_value(Name)
  else
    false -> error({badarg, MbAnno})
  end.

-doc """
Determines whether the specified abstract syntax representation is a mailbox
annotation macro.

A mailbox annotation macro abstract syntax representation is a tuple whose name
is represents the atoms '@new', '@use', '@as', and '@expects'.

### Returns
- `true` if the specified abstract syntax representation is a mailbox
annotation macro
- `false` otherwise
""".
-spec is_mb_anno(MbAnno :: paterl_syntax:expr()) -> true | false.
is_mb_anno(MbAnno) ->
  % Check annotation is a tuple.
  case erl_syntax:type(MbAnno) of
    tuple ->
      % Check annotation has at least a name.
      case erl_syntax:tuple_elements(MbAnno) of
        [Name | _] ->
          % Check annotation name is an atom.
          case erl_syntax:type(Name) of
            atom ->
              % Check annotation name is legal.
              Name0 = erl_syntax:atom_value(Name),
              Name0 =:= ?ANNO_NEW orelse Name0 =:= ?ANNO_USE orelse
                Name0 =:= ?ANNO_AS orelse Name0 =:= ?ANNO_EXPECTS orelse
                Name0 =:= state;
            _ ->
              false
          end;
        _ ->
          false
      end;
    _ ->
      false
  end.

%%-doc """
%%Creates a mailbox definition wild attribute abstract syntax representation with
%%the specified [annotation](`m:erl_anno`).
%%
%%Name = use,
%%
%%### Returns
%%- mailbox definition wild attribute abstract syntax representation
%%""".
%%-spec mb_definition(Name, Args, Anno) -> Tree
%%  when
%%  Name :: name(),
%%  Args :: [term()],
%%  Anno :: anno(),
%%  Tree :: erl_syntax:syntaxTree().
%%mb_definition(Name, Args, Anno)
%%  when is_atom(Name), is_list(Args) ->
%%  % The empty annotation is not revertible because it is not an
%%  % erl_parse:abstract_expr(), but an erl_syntax:syntaxTree() abstraction.
%%  % Inner expressions are reverted.
%%  set_anno(
%%    erl_syntax:attribute(
%%      erl_syntax:atom(Name), [erl_syntax:abstract(Arg) || Arg <- Args]
%%    ),
%%    Anno
%%  ).

-doc """
Creates a function application abstract syntax representation with the specified
[annotation](`m:erl_anno`).

### Returns
- function application abstract syntax representation
""".
-spec application(Name, Args, Anno) -> Tree
  when
  Name :: name(),
  Args :: [term()],
  Anno :: anno(),
  Tree :: tree().
application(Name, Args, Anno) when is_atom(Name), is_list(Args) ->
  set_anno(
    erl_syntax:application(
      erl_syntax:atom(Name), [erl_syntax:abstract(Arg) || Arg <- Args]
    ),
    Anno
  ).

% MsgType is a singleton type or a type union.

%%-doc """
%%Creates a mailbox interface definition wild attribute abstract syntax
%%representation with the specified [annotation](`m:erl_anno`).
%%
%%### Example
%%
%%The invocation interface(Name, MsgType, Anno), where Name = name, MsgType = erl_syntax:type_application(paterl_syntax:name(pid), []), and Anno = erl_anno:new(0), represents the interface XXX
%%
%%If for instance, name = x, and MsgType = {type, 0 , pid, []}, anno = 0, the result is
%%Name = paterl_syntax:name(mb),
%%MsgType = erl_syntax:type_application(paterl_syntax:name(pid), []),
%%Anno = erl_anno:new(),
%%
%%returns : <enter the stringified representation of what it returns here> do it for the rest of the functions above.
%%
%%
%%### Returns
%%- mailbox interface definition wild attribute abstract syntax representation
%%""".
%%-spec interface(Name, MsgType, Anno) -> Tree
%%  when
%%  Name :: tree(),
%%  MsgType :: type(),
%%  Anno :: anno(),
%%  Tree :: tree().
%%interface(Name, MsgType, Anno) ->
%%  InterfaceDef = erl_syntax:tuple([
%%%%    erl_syntax:atom(Name),
%%    Name,
%%    erl_syntax:abstract(MsgType), % Lifted message type to abstract syntax.
%%    erl_syntax:abstract(_Vars = []) % Lifted type variables to abstract syntax.
%%  ]),
%%  set_anno(erl_syntax:attribute(erl_syntax:atom(interface), [InterfaceDef]), Anno).

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
  % TODO: Sometime in the future.
  % TODO: Do not allow variables in type specs.
  Analysis#analysis{status = ok, result = Forms}.


%%% ----------------------------------------------------------------------------
%%% Mailbox interface definition wild attribute checks.
%%% ----------------------------------------------------------------------------

-doc """
Checks the well-formedness of mailbox interface `-new` and `-use` wild
attributes, skipping the rest.
""".
-spec check_mb_attribs(Forms, Analysis) -> Analysis0
  when
  Forms :: forms(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_mb_attribs(Forms, Analysis) when is_list(Forms) ->
  lists:foldl(fun check_attr/2, Analysis, Forms).

-doc """
Checks the well-formedness of a mailbox interface `-new` and `-use` wild
attribute, skipping it otherwise.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if the mailbox interface wild attribute is well-formed
- `status=error` with details otherwise
""".
-spec check_attr(Form, Analysis) -> Analysis0
  when
  Form :: form(),
  Analysis :: paterl_lib:analysis(),
  Analysis0 :: paterl_lib:analysis().
check_attr(Form, Analysis) ->
  maybe
    attribute ?= erl_syntax:type(Form),
    {Modality, Value} ?= erl_syntax_lib:analyze_attribute(Form),
    true ?= Modality =:= ?MOD_NEW orelse Modality =:= ?MOD_USE,

    ?TRACE("Check mailbox interface definition '~s'.", [erl_prettypr:format(Form)]),
    case Value of
      {MbName, Terms = [_ | _]} when is_atom(MbName) ->
        % Check well-formedness of function references.
        check_fun_refs(Terms, erl_syntax:get_pos(Form), Analysis);
      {MbName, []} when is_atom(MbName) ->
        % Unused mailbox interface definition (i.e. no fun references).
        ?WARN("Unused mailbox interface '~s'.", [erl_prettypr:format(Form)]),
        ?pushWarning(?W_NO__MB_FUN_REF, Form, Analysis);
      _ ->
        % Bad mailbox interface definition attribute.
        ?ERROR("Bad mailbox interface definition '~s'", [erl_prettypr:format(Form)]),
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



-doc """
Checks the well-formedness of fun references.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if the fun reference is well formed
- `status=error` with details otherwise
""".
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
check_fun_ref(Term, Anno, Analysis) ->
  % Bad function reference structure.
  Term0 = set_anno(erl_syntax:abstract(Term), Anno),
  ?ERROR("Bad fun reference '~s',", [erl_prettypr:format(Term0)]),
  ?pushError(?E_BAD__FUN_REF, Term0, Analysis).

-doc """
Checks the arity of a fun reference encoded as implicit funs.

### Returns
a [`paterl_lib:analysis()`](`t:paterl_lib:analysis/0`) with
- `status=ok` if the fun reference has valid arity
- `status=error` with details otherwise
""".
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

get_fun_ref(Call) ->
  maybe
    application ?= erl_syntax:type(Call),
    Operator = erl_syntax:application_operator(Call),

    atom ?= erl_syntax:type(Operator),
    Args = erl_syntax:application_arguments(Call),
    {ok, {erl_syntax:atom_value(Operator), length(Args)}}
  else
    _ ->
      % Not a static call.
      {error, Call};
    module_qualifier ->
      % Remote calls unsupported.
      {error, Call}
  end.

get_fun_ref_mfa([M, F, Args]) ->
  case erl_syntax:type(M) of
    atom ->
      case erl_syntax:type(F) of
        atom ->
          case erl_syntax:type(Args) of
            Type when Type =:= nil; Type =:= list ->
              {ok, erl_syntax:atom_value(M), {erl_syntax:atom_value(F),
                erl_syntax:list_length(Args)}};
            _ ->
              {error, Args}
          end;
        _ ->
          {error, F}
      end;
    _ ->
      {error, M}
  end.


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

-doc """
Formats `Detail` to human-readable form.

See `module/1` for error and warnings codes.

### Returns
- message as a possibly deep [`io_lib:chars()`](`t:io_lib:chars/0`) list
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
  );
format_error({?W_NO__MB_FUN_REF, Node}) ->
  io_lib:format(
    "unused mailbox interface definition '~s'",
    [erl_prettypr:format(Node)]
  ).

-ifdef(test).
is_atom_value(Expr, Value) when ?isAtomValue(Expr, Value) ->
  true;
is_atom_value(_, _) ->
  false.

is_mb_anno2(Expr) when ?isMbAnno(Expr) ->
  true;
is_mb_anno2(_) ->
  false.
-endif.