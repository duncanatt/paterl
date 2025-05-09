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
-module(paterl_trans).
-moduledoc """
Annotated Erlang abstract syntax representation to Pat abstract syntax
representation.
""".
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("paterl_syntax.hrl").

%%% Public API.
-export([module/1]).
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Mailbox variable name.
-define(MB_VAR_NAME, mb).

%% Checks whether the Erlang type is a Pat unit equivalent.
-define(isUnitEqType(Type), (?isType(Type) andalso
  (?litValue(Type) =:= no_return
    orelse ?litValue(Type) =:= any
    orelse ?litValue(Type) =:= none)
)).

%% Map of externally-defined opaque Erlang functions that can be substituted for
%% the concrete data unit value that has the same type as that returned by the
%% corresponding function. This scheme makes it possible to type check files
%% that call external functions without modifying the Pat implementation.
-define(OPAQUE_FUNS, #{
  format => unit,
  uniform => integer,
  system_time => integer,
  sleep => unit
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------
%% TODO: Add typespecs.

-doc """
Translates the specified Erlang abstract syntax representation to its equivalent
Pat abstract syntax representation.

### Returns
- list of Pat forms
""".
-spec module(Forms :: erl_syntax:forms()) -> list().
module(Forms) ->
  forms(Forms).


%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

-doc "Translates annotated Erlang forms.".
forms(Forms) when is_list(Forms) ->
  % Skip untranslatable forms.
  [Form0 || Form <- Forms, (Form0 = form(Form)) =/= undefined].

-doc "Translates an annotated Erlang form.".
form({attribute, _, module, Name}) ->
  % Erlang module attribute.
  pat_syntax:comment("Translated from " ++ atom_to_list(Name) ++ ".erl");
form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  % Pat interface module attribute with message signatures.
  ?TRACE("Translate interface '~s'.", [Name]),
  case type(Type) of
    undefined ->
      % Empty interface.
      pat_syntax:interface_def(Name);
    Type0 ->
      % Non-empty interface.
      pat_syntax:interface_def(Name, Type0)
  end;
form({function, Anno, Name, Arity, Clauses = [_]}) ->
  % Erlang function with one clause.
  ?TRACE("Translate function '~s/~b'.", [Name, Arity]),
  ?TRACE("Anno = ~w", [Anno]),

%%  MbScopes =
%%    case paterl_anno:interfaces(Anno) of
%%      Mbs when is_list(Mbs) -> [MbName || {_, MbName} <- Mbs];
%%      undefined -> undefined
%%    end,
%%  pat_syntax:fun_def(Name, fun_clauses(Clauses, MbScopes));
  pat_syntax:fun_def(Name, fun_clauses(Clauses));
form(_Form) ->
  % Erlang forms without Pat equivalent.
  ?TRACE("Skip form '~s'.", [element(3, _Form)]),
  undefined.

-doc "Translates an Erlang type definition.".
type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. The clause handles the case where a
  % mailbox interface type is just a PID. Mailbox interfaces whose type is just
  % a PID are treated as empty Pat mailbox interface definitions.
  undefined;
type(Type = {type, _, Name, _Vars = []}) when ?isLitType(Type) ->
  % Erlang literal types.
  pat_syntax:lit_type(Name);
type(Type = {type, _, _Name, _Vars = []}) when ?isUnitEqType(Type) ->
  % Erlang special type translated as Pat unit type.
  pat_syntax:lit_type(unit);
type({atom, _, ok}) ->
  % Erlang atom 'ok' translated as Pat unit type.
  % TODO: Will be changed removed once Pat supports atoms.
  pat_syntax:lit_type(unit);
type({user_type, _, Name, _Vars = []}) ->
  % Erlang mailbox type. Mailbox types default to the write capability.
  pat_syntax:mb_type(Name, write);
type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Erlang message signature type.
  pat_syntax:msg_type(Name, type_seq(TypeSeq));
type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Erlang union type.
  pat_syntax:union_type(type_seq(TypeSeq)).

-doc "Translates an Erlang type definition sequence.".
type_seq([]) ->
  [];
type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  % Erlang PID types are not translated and dropped.
  type_seq(TypeSeq);
type_seq([Type | TypeSeq]) ->
  [type(Type) | type_seq(TypeSeq)].


%%% ----------------------------------------------------------------------------
%%% Translation on terms in mailbox context.
%%% ----------------------------------------------------------------------------

-doc """
Generic function that translates a list of Erlang clauses in a mailbox context.
""".
clauses(Fun, Clauses, MbVars) when is_function(Fun, 2), is_list(Clauses) ->
  [Fun(Clause, MbVars) || Clause <- Clauses].

-doc "Translates a list of Erlang case or receive clauses.".
case_clauses(Clauses, MbVars) ->
  ?TRACE("~w Translate case/receive clauses.", [MbVars]),
  clauses(fun case_clause/2, Clauses, MbVars).

-doc "Translates a list of Erlang if clauses.".
if_clauses(Clauses, MbVars) ->
  ?TRACE("~w Translate if clauses.", [MbVars]),
  clauses(fun if_clause/2, Clauses, MbVars).

-doc "Translate an Erlang case of receive clause.".
case_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, MbNameVars) ->
  % Erlang unconstrained case and receive clause.
  ?TRACE("~w Translate case/receive clause.", [MbNameVars]),
%%  MbVar0 = fresh_mb(),
  MbName = paterl_anno:scope(Anno),
%%  MbVar = mb_var(MbName, MbNameVars),

  % Rebind mailbox variable.
  {MbVar0, MbNameVars0} = rebind_mb_var(MbName, MbNameVars),

  Expr = expr(Body, MbNameVars0),
  [MsgPat] = pat_seq(PatSeq),
  pat_syntax:receive_expr(MsgPat, pat_syntax:var(MbVar0), Expr).

-doc "Translate an Erlang if clause.".
if_clause(_Clause = {clause, _, _PatSeq = [], [[GuardTest]], ExprSeq}, MbVars) ->
  % Erlang constrained if clause with exactly one guard and one guard test.
  ?TRACE("~w Translate if clause.", [MbVars]),
  {guard_test(GuardTest), expr(ExprSeq, MbVars)}.

-doc """
Translates an Erlang expression sequence into its equivalent single nested Pat
`let` expression.
""".
expr(ExprSeq, MbVars) ->
  ?TRACE("BEFORE translating singleton expr seq"),
  [Expr] = expr_seq(ExprSeq, MbVars),
  ?TRACE("AFTER translating singleton expr seq"),
  Expr.

-doc """
Translates an Erlang expression sequence into its equivalent Pat expression
sequence.

The returned sequence is a singleton list whose one element consists of the
equivalent single nested Pat `let` expression.
""".
expr_seq([], _) ->
  [];
expr_seq([{call, Anno, {atom, _, self}, _MFArgs = []} | ExprSeq], MbNameVars) -> %TODO: MbVars should be a key val list {mb_name, mb_var}
  % Erlang self function call expression.
  ?TRACE("~w Translate self expression.", [MbNameVars]),

  MbName = paterl_anno:scope(Anno),
  ?TRACE("MbName = ~s", [MbName]),

%%  rebind_mb_vars()

  MbVar = mb_var(MbName, MbNameVars),
  ?TRACE("MbVar = ~p", [MbVar]),
  Var = pat_syntax:var(MbVar),
%%  [pat_syntax:tuple([MbVar0, MbVar0]) | expr_seq(ExprSeq, MbNameVars)];
  Vars = [pat_syntax:var(MbVar) || MbVar <- mb_vars(MbNameVars)],
  [pat_syntax:tuple([Var | Vars]) | expr_seq(ExprSeq, MbNameVars)];
expr_seq([Expr = {call, Anno, {atom, _, Name}, Args} | ExprSeq], MbNameVars) when Name =/= spawn ->
  % Erlang static function call and mailbox-annotated static function call.
  %
  % Dynamic and remote function calls are unsupported.
  %
  % The guard 'Name =/= spawn' is added to force passing calls to 'spawn' to the
  % catch-all clause, which in turn, translates it outside the mailbox context.
  % This makes it correspond to our translation on paper, where 'spawn' is a
  % distinguished operator in the pseudo-Erlang syntax. By contrast, Erlang
  % treats 'spawn' as a regular function call. Such a distinction would not be
  % needed in practice and removing 'Name =/= spawn' yields the same translated
  % output.
  MbNames = paterl_anno:scopes(Anno),
  Call0 =
    case MbNames of
      undefined ->
        % Call to function call outside mailbox context.
        ?TRACE("~w Translate call to ~s/~b.", [MbNameVars, Name, length(Args)]),
        Call = expr([Expr]),

%%        ExprSeq0 = [pat_syntax:var(MbVar) || MbVar <- MbNameVars],
        % Create state-passing tuple.
        Vars0 = [pat_syntax:var(MbVar) || MbVar <- mb_vars(MbNameVars)],
%%        pat_syntax:tuple([Call, pat_syntax:var(MbVars)]);
        pat_syntax:tuple([Call | Vars0]);

      _MbNames ->
        % Call to function call inside mailbox context.
        Modality = paterl_anno:modality(Anno),
        ?TRACE("~w Translate call to '~s/~b' with interfaces '~w' and modality '~s'.", [
          MbNameVars, Name, length(Args), _MbNames, Modality
        ]),

        % TODO
%%        MbVars0 = mb_vars(MbNameVars),
%%        ?TRACE("++MbNames = ~p", [MbNames]),

        case Modality of
          ?MOD_NEW ->
            % Inject new mailbox variables.
            % Create state-passing tuple.
            ?TRACE("[NEW] MbNamesVars = ~w", [MbNameVars]),
            ?TRACE("[NEW] MbNames = ~w", [MbNames]),
            Call = expr([Expr]),
            ?TRACE("After translating call OUTSIDE mailbox scope"),
            ?TRACE("[NEW] MbNamesVars = ~w", [MbNameVars]),
            ?TRACE("[NEW] MbNames = ~w", [MbNames]),
%%            Vars0 = [pat_syntax:var(MbVar0) || MbVar0 <- MbVars0],
%%            Vars0 = [pat_syntax:var(MbVar) || MbVar <- mb_vars(MbNames, MbNameVars)],
            Vars0 = [pat_syntax:var(MbVar) || MbVar <- mb_vars(MbNameVars)],
            ?TRACE("Call and vars translated"),
            pat_syntax:tuple([Call | Vars0]);
          ?MOD_USE ->
            % Thread through existing mailbox variables.
%%            Args0 = [erl_syntax:revert(erl_syntax:atom(MbVar0)) || MbVar0 <- MbVars0],
            ?TRACE("[USE] MbNamesVars = ~w", [MbNameVars]),
            ?TRACE("[USE] MbNames = ~w", [MbNames]),
            Vars0 = [pat_syntax:var(MbVar) || MbVar <- mb_vars(MbNames, MbNameVars)],
            ?TRACE("After translating vars"),
%%            pat_syntax:call_expr(Name, args(Args0 ++ Args))
            X = pat_syntax:call_expr(Name, Vars0 ++ args(Args)),
            ?TRACE("After translating call expression with ?USE modality"),
            X
        end


%%        Modality = paterl_anno:modality(Anno),
%%        ?TRACE("(~s) Translate call to ~s/~b [~s, ~s].", [
%%          Mb, Name, length(Args), _Interfaces, Modality
%%        ]),
%%        case Modality of
%%          new ->
%%            % Inject new mailbox.
%%            Call = expr([Expr]),
%%            pat_syntax:tuple([Call, pat_syntax:var(Mb)]);
%%
%%          use ->
%%            % Thread through existing mailbox.
%%            Args0 = [erl_syntax:revert(erl_syntax:atom(Mb)) | Args],
%%            pat_syntax:call_expr(Name, args(Args0))
%%        end
    end,
  [Call0 | expr_seq(ExprSeq, MbNameVars)];
expr_seq([{match, _, Pat, Expr} | ExprSeq], MbNameVars) ->
  % Erlang match expression.
  ?TRACE("~w Translate match expression.", [MbNameVars]),
  ?TRACE("~n~nMatch expression has pattern ~s", [erl_prettypr:format(Pat)]),

  Expr0 = expr([Expr], MbNameVars),

  ?TRACE("-----After translating expr in match"),

  % The size of tuple to use as a binder an a Pat let expression depends on
  % whether the corresponding Erlang match expression body is a function call or
  % otherwise. If a function call, the size of the tuple used in the Pat let
  % binding must correspond with the one returned by the called function.
  % Otherwise, the size of the tuple corresponds with the size of the state-
  % passing tuple.
  {MbVars0, MbNameVars0} =
    case erl_syntax:type(Expr) of
      application ->
        case paterl_anno:scopes(erl_syntax:get_pos(Expr)) of
          undefined ->
            % Normal function call which returns a singleton value. Binders
            % in let tuple correspond to state-passing tuple.
            ?TRACE("~w Translate match expression with function call outside mailbox context.", [MbNameVars]),
            rebind_mb_vars(MbNameVars);
%%            {ok, ok};
          MbNames ->
            % Function call in mailbox context which returns a tuple. Binders
            % correspond to values in tuple returned by function call.
            % Erlang calls to spawn are treated as normal function calls.
            case erl_syntax:atom_value(erl_syntax:application_operator(Expr)) of
              Op = spawn ->
                % Spawn function call. Binders in let tuple correspond to state-
                % passing tuple
                ?TRACE("~w Translate match expression with '~s' call inside mailbox context.", [MbNameVars, Op]),
                rebind_mb_vars(MbNameVars);
%%                {ok, ok};
              Op ->
                % Any other function call. Binders in let tuple correspond to
                % values in tuple returned by function call.
                ?TRACE("~w Translate match expression with '~s' call inside mailbox context.", [MbNameVars, Op]),
                ?TRACE("Match expression body = ~p", [Expr]),
                ?TRACE("Rebind variables associated with ~w in ~w", [MbNames, MbNameVars]),

                % Check if call is new or use.
                ?TRACE("Called function uses ~s modality.", [paterl_anno:modality(erl_syntax:get_pos(Expr))]),
                case paterl_anno:modality(erl_syntax:get_pos(Expr)) of
                  ?MOD_NEW ->
                    % When modality of call is new, the generated function call
                    % is closed and we need to rebind all variables since these
                    % the state-passing tuple variables are independent of the
                    % ones returned by the call.
                    rebind_mb_vars(MbNameVars);
                  ?MOD_USE ->
                    % When the modality of call is use, the generated function
                    % call is open and we need to rebind only those state-
                    % passing tuple variables corresponding to the ones returned
                    % by the function call and leave the rest of the state-
                    % passing tuple variables in the parent mailbox unchanged.
                    X = rebind_mb_vars(MbNames, MbNameVars),
                    ?TRACE("Let variables to rebind = ~w", [X]),
                    X
                end


%%                rebind_mb_vars(MbNameVars)
%%                {ok, ok}
            end
        end;
      _ ->
        % Any other expression. Binders in let tuple correspond to state-passing
        % tuple.
        ?TRACE("~w Translate match expression with expression.", [MbNameVars]),
        rebind_mb_vars(MbNameVars)
%%        {ok, ok}
    end,




  % Create fresh variable names for each mailbox name in scope to rebind them.
%%  MbVars0 = [{MbName, fresh_mb()} || {MbName, _} <- MbVars],
  % Rebind mailbox variables.
%%  {MbVars0, MbNameVars0} = rebind_mb_vars(MbVars),


  ?TRACE("MbVars = ~w", [MbNameVars]),
  ?TRACE("MbVars0 = ~w", [MbVars0]),
%%  Vars0 = [pat_syntax:var(MbVar0) || {_, MbVar0} <- MbVars0],
  Vars0 = [pat_syntax:var(MbVar0) || MbVar0 <- MbVars0],
%%  Binders = pat_syntax:tuple([pat(Pat), pat_syntax:var(MbVar0)]),
  Binders = pat_syntax:tuple([pat(Pat) | Vars0]),

  % Rest of Erlang expression sequence is translated because Pat let expressions
  % induce nested evaluation context rooted at this top-level let expression.
  % This means that an Erlang expression sequence is always reduced to a
  % singleton list of Pat expressions consisting of one let.
  Body =
    case ExprSeq of
      [] ->
        % Empty expression sequence. Use binders to complete let body.
        Binders;
      ExprSeq ->
        % Non-empty expression sequence.
        expr(ExprSeq, MbNameVars0)
    end,
  [pat_syntax:let_expr(Binders, Expr0, Body)];
expr_seq([{'if', _, [Clause0, Clause1]} | ExprSeq], MbVars) ->
  % Erlang expression sequence with exactly two clauses. This constraint pair
  % corresponds to the 'if' and 'else' branches in Pat.
  ?TRACE("~w Translate if-else expression.", [MbVars]),
  {ExprC, ExprT} = if_clause(Clause0, MbVars), % If.
  {{boolean, _, true}, ExprF} = if_clause(Clause1, MbVars), % Else.
  [pat_syntax:if_expr(ExprC, ExprT, ExprF) | expr_seq(ExprSeq, MbVars)];
expr_seq([{'receive', Anno, Clauses} | ExprSeq], MbNameVars) ->
  % Erlang unconstrained receive expression. Corresponds to a Pat guard
  % expression.
  Pattern = paterl_anno:pattern(Anno),
  MbName = paterl_anno:scope(Anno),

%%  {_, MbNameVars1} = rebind_mb_vars(MbNameVars),

  ?TRACE("~w Translate receive expression guarding on '~s' with '~s'.", [MbNameVars, MbName, Pattern]),
%%  ReceiveClauses = case_clauses(Clauses, MbNameVars1),
  ReceiveClauses = case_clauses(Clauses, MbNameVars),

  % Check mailbox regular expression for emptiness to determine if a Pat empty
  % expression is required.
  ReceiveClauses0 =
    case pat_regex:is_mb_empty(Pattern) of
      true ->
        % Mailbox may be empty. Add Pat empty expression.
        ?TRACE("~w Mailbox '~s' may be empty.", [MbNameVars, MbName]),

        % Rebind mailbox variable.
        {MbVar0, MbNameVars0} = rebind_mb_var(MbName, MbNameVars),

        % BEGIN HACK: Determine unit datum to return based on the return type of the
        % enclosing function.
        {type, _, Type, _} = paterl_anno:type(Anno),
        Unit = get_unit_value(Type),
        ?TRACE("~w HACK: Generating unit value ~p for type ~p", [MbNameVars, Unit, Type]),

        % Create rebound mailbox variables and empty expression.
        Var0 = pat_syntax:var(MbVar0),
        Vars0 = [pat_syntax:var(MbVar0) || MbVar0 <- mb_vars(MbNameVars0)],
        EmptyExpr = pat_syntax:empty_expr(
%%          MbVar0, pat_syntax:tuple([Unit, MbVar0]) %TODO: This cannot be unit but must be the datatype of the return type of the function.
          Var0, pat_syntax:tuple([Unit | Vars0]) %TODO: This cannot be unit but must be the datatype of the return type of the function.
        ),

        % END HACK.
        [EmptyExpr | ReceiveClauses];
      false ->
        % Mailbox cannot be empty.
        ReceiveClauses
    end,

  ?TRACE("~w Generate guard on '~s' with ~b clause(s).", [
    MbNameVars, Pattern, length(ReceiveClauses0)
  ]),

  % Get mailbox variable associated with mailbox name and rebind rest of mailbox
  % variables.
  MbVar = mb_var(MbName, MbNameVars),

  Guard = pat_syntax:guard_expr(pat_syntax:var(MbVar), Pattern, ReceiveClauses0),
  [Guard | expr_seq(ExprSeq, MbNameVars)];
expr_seq([Expr | ExprSeq], MbNameVars) ->
  % Pass thru to out-of-mailbox context translation:
  %
  % 1. Erlang literals and variables.
  % 2. Erlang binary and unary operators.
  % 3. Erlang spawn expression.
  ?TRACE("~w Pass thru ~p expression.", [MbNameVars, Expr]),
  Expr0 = expr([Expr]),
  Vars = [pat_syntax:var(MbVar) || MbVar <- mb_vars(MbNameVars)],
  [pat_syntax:tuple([Expr0 | Vars]) | expr_seq(ExprSeq, MbNameVars)].


%%% ----------------------------------------------------------------------------
%%% Translation on terms outside mailbox context.
%%% ----------------------------------------------------------------------------

-doc """
Generic function that translates a list of Erlang clauses outside a mailbox
context.
""".
clauses(Fun, Clauses) when is_function(Fun, 1), is_list(Clauses) ->
  [Fun(Clause) || Clause <- Clauses].

-doc "Translates a list of Erlang function clauses.".
%%fun_clauses(Clauses, MbScopes) ->
fun_clauses(Clauses) ->
%%  clauses(fun fun_clause/2, Clauses).
  clauses(fun fun_clause/1, Clauses).
%%  [fun_clause(Clause, MbScopes) || Clause <- Clauses].

-doc "Translates a list of Erlang list clauses.".
if_clauses(Clauses) ->
  clauses(fun if_clause/1, Clauses).

-doc "Translates an Erlang function clause.".
%%fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body}, MbScopes) ->
fun_clause({clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Erlang unconstrained function clause or unconstrained mailbox-annotated
  % function clause.

  % Translate function return type.
  RetType = type(paterl_anno:type(Anno)),

  % Determine whether function is mailbox-annotated.
%%  case paterl_anno:interfaces(Anno) of
  case paterl_anno:scopes(Anno) of
%%  case MbScopes of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translate NON mailbox-annotated function clause */~b.", [
        length(PatSeq)
      ]),

      % Translate function parameters and body.
      pat_syntax:fun_clause(params(PatSeq), expr(Body), RetType);

    MbScopes when is_list(MbScopes) ->
      % Mailbox-annotated function.
      ?TRACE("Translate mailbox-annotated function clause */~b.", [
        length(PatSeq)
      ]),

      ?TRACE("---- MBSCOPES: ~w", [MbScopes]),

      % Create mailbox interface names to be injected as the first parameters of
      % the function clause.
      % Mbs are the mailbox variable names, MbTypes, the corresponding types,
      % and Params, the parameters to be injected in the function clause.
%%      {MbVars, MbTypes, Params} =
%%        lists:foldl(
%%          fun(MbName, {Mbs0, MbTypes0, Params0}) ->
%%            Mb = fresh_mb(),
%%            MbType = pat_syntax:mb_type(MbName, read),
%%            {[Mb | Mbs0], [MbType | MbTypes0], [pat_syntax:param(pat_syntax:var(Mb), MbType) | Params0]}
%%          end, {[], [], []}, lists:reverse(MbScopes)
%%        ),
%%      {MbVars, MbTypes, Params} =
%%        lists:foldl(
%%          fun(MbName, {Mbs0, MbTypes0, Params0}) ->
%%            MbVar = fresh_mb(),
%%            MbType = pat_syntax:mb_type(MbName, read),
%%            {[{MbName, MbVar} | Mbs0], [MbType | MbTypes0], [pat_syntax:param(pat_syntax:var(MbVar), MbType) | Params0]}
%%          end, {[], [], []}, MbScopes
%%        ),

%%      MbVar = lists:last(MbVars),

      {MbVars, MbNameVars} = new_mb_vars(MbScopes),
      MbTypes = [pat_syntax:mb_type(MbName, read) || MbName <- MbScopes],
      Params = [pat_syntax:param(pat_syntax:var(MbVar), Type) || {MbVar, Type} <- lists:zip(MbVars, MbTypes)],

%%      ?TRACE("{Mbs, MbTypes, Params} = ~p", [{MbVars, MbTypes, Params}]),
      Params0 = Params ++ params(PatSeq),
      Expr = expr(Body, MbNameVars),
      pat_syntax:fun_clause(Params0, Expr, pat_syntax:product_type([RetType | MbTypes]))
%%      ko;
%%
%%    _Interface ->
%%      % Mailbox-annotated function.
%%      ?TRACE("Translate mailbox-annotated function clause */~b.", [
%%        length(PatSeq)
%%      ]),
%%
%%      % Create mailbox to be injected as first parameter of the
%%      % mailbox-annotated function clause.
%%      Mb = fresh_mb(),
%%      MbType = pat_syntax:mb_type(paterl_anno:interfaces(Anno), read),
%%      Params = [
%%        pat_syntax:param(pat_syntax:var(Mb), MbType) |
%%        params(PatSeq)
%%      ],
%%
%%      Expr = expr(Body, Mb),
%%      pat_syntax:fun_clause(
%%        Params, Expr, pat_syntax:product_type([RetType, MbType])
%%      )
  end.

-doc "Translate an Erlang if clause.".
if_clause({clause, _, _PatSeq = [], [[GuardTest]], ExprSeq}) ->
  % Erlang constrained if clause with exactly one guard and one guard test.
  ?TRACE("Translate if clause."),
  {guard_test(GuardTest), expr(ExprSeq)}.

-doc """
Translates an Erlang expression sequence into its equivalent single nested Pat
`let` expression.
""".
expr(ExprSeq) ->
  [Expr] = expr_seq(ExprSeq),
  Expr.

-doc """
Translates an Erlang expression sequence into its equivalent Pat expression
sequence.

The returned sequence is a singleton list whose one element consists of the
equivalent single nested Pat `let` expression.
""".
expr_seq([]) ->
  [];
expr_seq([{atom, _, ok} | ExprSeq]) ->
  [pat_syntax:unit() | expr_seq(ExprSeq)];
expr_seq([Lit | ExprSeq]) when ?isLit(Lit) ->
  % Erlang literal expressions.
  ?TRACE("Translate literal ~s ~p.", [element(1, Lit), ?litValue(Lit)]),
  [pat_syntax:lit(?litValue(Lit)) | expr_seq(ExprSeq)];
expr_seq([{var, _, Name} | ExprSeq]) ->
  % Erlang variable expression.
  ?TRACE("Translate variable '~s'.", [Name]),
  [pat_syntax:var(Name) | expr_seq(ExprSeq)];
expr_seq([{tuple, _, [_Tag = {atom, _, Name} | Args]} | ExprSeq]) ->
  % Erlang Pat message expression.
  ?TRACE("Translate message with tag '~s'.", [Name]),
  [pat_syntax:msg_expr(Name, args(Args)) | expr_seq(ExprSeq)];
expr_seq([Expr = {call, _, {atom, _, spawn}, _MFArgs = [_, _Fun, _Args]} | ExprSeq]) ->
  % Erlang spawn function call expression.
  ?TRACE("Translate call to spawn ~s/~b.", [element(3, _Fun), erl_syntax:list_length(_Args)]),
  [spawn_expr(Expr) | expr_seq(ExprSeq)];
expr_seq([Expr = {call, Anno, _Fun = {atom, _, Name}, Args} | ExprSeq]) ->
  % Erlang implicitly-qualified local function call (i.e. function name is an
  % atom). Only mailbox-annotated 'new' implicitly-qualified function calls are
  % permitted by the translation.
  %
  % Erlang explicit function calls (i.e. function name is an expression) are
  % unsupported, as are remote function calls.

  % Check whether the function is an externally-defined opaque function that can
  % be substituted for the concrete data unit value of its corresponding return
  % type. This avoids catering for all possible external Erlang functions that
  % the source code uses, replacing them by a concrete value instead.
  case get_fun_return_unit(Expr) of
    undefined ->
      % Unknown externally-defined function that is translated normally.
      case paterl_anno:scopes(Anno) of
        undefined ->
          % Call to function outside mailbox context.
          [pat_syntax:call_expr(Name, args(Args)) | expr_seq(ExprSeq)];

        _MbNames ->
          % Call to function inside mailbox context. Only the new modality is
          % permitted at this point.
          Modality = paterl_anno:modality(Anno),
          ?assertEqual(Modality, new),

          ?TRACE("Translate call to '~s/~b' with interfaces '~w' and modality '~s'.", [
            Name, length(Args), _MbNames, Modality
          ]),
          X = [new_call_expr(Expr) | expr_seq(ExprSeq)],
          ?TRACE("After translating NEW function call"),
          X
      end;

    RetType ->
      % Known externally-defined function that is replaced by a concrete unit
      % value.
      [RetType | expr_seq(ExprSeq)]
  end;
expr_seq([{match, _, Pat, Expr} | ExprSeq]) ->
  % Erlang match expression.
  ?TRACE("Translate match expression."),
  Expr0 = expr([Expr]),

  % Rest of Erlang expression sequence is translated because Pat let expressions
  % induce nested evaluation context rooted at this top-level let expression.
  % This means that an Erlang expression sequence is always reduced to a
  % singleton list of Pat expressions consisting of one let.
  Binders = pat(Pat),
  Body =
    case ExprSeq of
      [] ->
        % Empty expression sequence. Use binders to complete let body.
        Binders;
      ExprSeq ->
        % Non-empty expression sequence.
        expr(ExprSeq)
    end,
  [pat_syntax:let_expr(Binders, Expr0, Body)];
expr_seq([{op, _, Op, Expr0, Expr1} | ExprSeq]) ->
  % Erlang binary operator expression.
  % TODO: Should be changed to values eventually when we have ANF.
  ?TRACE("Translate binary operator expression ~s.", [Op]),
  ExprL = expr([Expr0]),
  ExprR = expr([Expr1]),
  [pat_syntax:op_expr(to_pat_op(Op), ExprL, ExprR) | expr_seq(ExprSeq)];
expr_seq([{op, _, Op, Expr} | ExprSeq]) ->
  % Erlang unary operator expression.
  ?TRACE("Translate unary operator expression ~s.", [Op]),
  Expr0 = expr([Expr]),
  [pat_syntax:op_expr(to_pat_op(Op), Expr0) | expr_seq(ExprSeq)];
expr_seq([{'if', _, [Clause0, Clause1]} | ExprSeq]) ->
  % Erlang expression sequence with exactly two clauses. This constraint pair
  % corresponds to the 'if' and 'else' branches in Pat.
  ?TRACE("Translate if-else expression."),
  {ExprC, ExprT} = if_clause(Clause0), % If.
  {{boolean, _, true}, ExprF} = if_clause(Clause1), % Else.
  [pat_syntax:if_expr(ExprC, ExprT, ExprF) | expr_seq(ExprSeq)];
expr_seq([Expr | _]) ->
  % Erlang unsupported expressions.
  ?ERROR("Unsupported Erlang expression: ~p", [Expr]),
  throw(lists:flatten(
    io_lib:format("Unsupported Erlang expression ~s", [erl_pp:expr(Expr)])
  )).

-doc "Translates an Erlang argument expression sequence.".
args(ExprSeq) ->
  expr_seq(ExprSeq).


%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------

-doc "Translates an Erlang guard sequence.".
guard_seq(GuardSeq) ->
  [guard(Guard) || Guard <- GuardSeq].

-doc "Translates an Erlang guard.".
guard(GuardTests) ->
  [guard_test(GuardTest) || GuardTest <- GuardTests].

-doc "Translates an Erlang guard test.".
guard_test(Lit) when ?isLit(Lit) ->
  % Erlang literal guard tests.
  pat_syntax:lit(?litValue(Lit));
guard_test({var, _, Name}) ->
  % Erlang variable guard test.
  pat_syntax:var(Name);
guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Erlang decidable guard function call guard test.
  pat_syntax:call_expr(Name, guard(GuardTests));
guard_test({op, _, Op, GuardTestL, GuardTestR}) ->
  % Erlang binary operator guard test.
  pat_syntax:op_expr(to_pat_op(Op), guard_test(GuardTestL), guard_test(GuardTestR));
guard_test({op, _, Op, GuardTest}) ->
  % Erlang unary operator guard test.
  pat_syntax:op_expr(to_pat_op(Op), guard_test(GuardTest)).

-doc "Translates an Erlang parameter sequence.".
params(PatSeq) ->
  Translate =
    fun(Pat) ->
      Type = paterl_anno:type(_Anno = element(2, Pat)),
      pat_syntax:param(pat(Pat), type(Type))
    end,
  [Translate(Pat) || Pat <- PatSeq].

-doc "Translates an Erlang pattern sequence.".
pat_seq(PatSeq) ->
  [pat(Pat) || Pat <- PatSeq].

-doc "Translates an Erlang pattern.".
pat(Lit) when ?isLit(Lit) ->
  % Erlang literal patterns.
  pat_syntax:lit(?litValue(Lit));
pat({var, _, Name}) ->
  % Erlang variable pattern.
  pat_syntax:var(Name);
pat({tuple, _, [_Tag = {atom, _, Name} | Args]}) ->
  % Erlang message pattern.
  pat_syntax:msg_pat(Name, pat_seq(Args)).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc """
Creates a call to a Pat function, injects a new mailbox, and frees that mailbox
once it goes out of scope.
""".
new_call_expr({call, Anno, Fun = {atom, _, _}, Args}) ->
  % Only function calls with new mailbox-annotation modality are permitted.
  ?assertEqual(new, paterl_anno:modality(Anno)),

  % Create Erlang syntax of function to be called. The created function must be
  % annotated with the use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
  ),

%%  HERE: make this use multiple lists for mailboxes
  % Create local mailbox IDs.
%%  MbNew = fresh_mb(),
%%  MbCall = fresh_mb(),

  MbNames = paterl_anno:scopes(Anno),
  ?TRACE("MbNames = ~w", [MbNames]),

  {MbVars, MbNameVars} = new_mb_vars(MbNames),
  {MbVars0, MbNameVars0} = new_mb_vars(MbNames),
  ?TRACE("MbNameVars = ~w", [MbNameVars]),


  % Variables used to construct call expression.
%%  MbVarNew = pat_syntax:var(MbNew),
%%  MbVarCall = pat_syntax:var(MbCall),
%%  RetCall = pat_syntax:var(x),
  MbVarsNew = [pat_syntax:var(MbVar) || MbVar <- MbVars],
  MbVarsCall = [pat_syntax:var(MbVar0) || MbVar0 <- MbVars0],
  RetCall = pat_syntax:var(x),

  % Translate function call.
  Call = expr([Expr], MbNameVars),
  ?TRACE("After translating function call with ?NEW modality"),

  % Let with free expression.
%%  LetFree = pat_syntax:let_expr(
%%    pat_syntax:var(y), pat_syntax:free_expr(MbVarCall), RetCall
%%  ),
  LetFree = free_mbs(MbNameVars0, RetCall),
%%  LetFree = pat_syntax:let_expr(
%%    pat_syntax:var(y), free_mbs(MbNameVars0), RetCall
%%  ),

  % Let with call expression.
%%  LetCall = pat_syntax:let_expr(
%%    pat_syntax:tuple([RetCall, MbVarCall]), Call, LetFree
%%  ),
  LetCall = pat_syntax:let_expr(
    pat_syntax:tuple([RetCall | MbVarsCall]), Call, LetFree
  ),

  % Let with new mailbox creation.
%%  Interface = hd(paterl_anno:scopes(Anno)), % TODO: To fix for multiple mailboxes in a recursive loop.
%%  pat_syntax:let_expr(
%%    MbVarNew, pat_syntax:new_expr(pat_syntax:mb_type(Interface)), LetCall
%%  ).
  let_mbs(MbNameVars, LetCall).

-doc """
Creates a spawn of a Pat function, injects a new mailbox, and frees the mailbox
once the function goes out of scope.
""".
spawn_expr({call, Anno, {atom, _, spawn}, _MFArgs = [_, Fun, Args]}) ->
  % Only function calls with new mailbox-annotation modality are permitted.
  ?assertEqual(new, paterl_anno:modality(Anno)),

  % Create Erlang syntax of function to be spawned. The created function must be
  % annotated with the use modality.
  Expr = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, erl_syntax:list_elements(Args)),
      paterl_anno:set_modality(?MOD_USE, Anno))
  ),

  MbNames = paterl_anno:scopes(Anno),
  ?TRACE("MbNames = ~w", [MbNames]),

  {MbVars, MbNameVars} = new_mb_vars(MbNames),
  {MbVars0, MbNameVars0} = new_mb_vars(MbNames),
  ?TRACE("MbNameVars = ~w", [MbNameVars]),

  % Create local mailbox IDs.
%%  MbNew = fresh_mb(),
%%  MbCall = fresh_mb(),

  % Translate function call.
  Call = expr([Expr], MbNameVars),

  % Variables used to construct spawn expression.
%%  MbVarNew = pat_syntax:var(MbNew),
%%  MbVarCall = pat_syntax:var(MbCall),

  MbVarsNew = [pat_syntax:var(MbVar) || MbVar <- MbVars],
  MbVarsCall = [pat_syntax:var(MbVar0) || MbVar0 <- MbVars0],
  RetCall = pat_syntax:var(x),

%%  HERE: figure out the let loop and free loops.
  % Let with call expression to be spawned.
%%  LetCall = pat_syntax:let_expr(
%%    pat_syntax:tuple([RetCall, MbVarCall]),
%%    Call,
%%    pat_syntax:free_expr(MbVarCall)
%%  ),
  LetCall = pat_syntax:let_expr(
    pat_syntax:tuple([RetCall | MbVarsCall]),
    Call,
%%    pat_syntax:free_expr(hd(MbVarsCall))
    free_mbs(MbNameVars0, RetCall)
  ),

  % Let with spawn expression.
%%  LetSpawn = pat_syntax:let_expr(
%%    pat_syntax:var(y), pat_syntax:spawn_expr(LetCall), MbVarNew
%%  ),
  LetSpawn = pat_syntax:let_expr(
    pat_syntax:var(y), pat_syntax:spawn_expr(LetCall), hd(MbVarsNew) % TODO: This should be a tuple once simon fixes lets to accept tuples.
  ),

  % Let with new mailbox creation.
%%  pat_syntax:let_expr(
%%    hd(MbVarsNew),
%%    pat_syntax:new_expr(pat_syntax:mb_type(hd(paterl_anno:scopes(Anno)))), % TODO: we need a recursive one for the length of scopes.
%%    LetSpawn
%%  ).
  let_mbs(MbNameVars, LetSpawn).


let_mbs([], Expr) ->
%%  "expr";
  Expr;
let_mbs([{MbName, MbVar} | MbNameVars], Expr) ->
  LetExpr = let_mbs(MbNameVars, Expr),
  NewExpr = pat_syntax:new_expr(pat_syntax:mb_type(MbName)),
  pat_syntax:let_expr(pat_syntax:var(MbVar), NewExpr, LetExpr).
%%  Let = lists:flatten(let_mbs(MbNameVars, Expr)),
%%  io_lib:format("let ~s = new [~s]~nin~n~s~n", [MbVar, MbName, Let]).

%%free_mbs([{_, MbVar}]) ->
%%  pat_syntax:free_expr(pat_syntax:var(MbVar));
%%free_mbs([{_, MbVar} | MbNameVars]) ->
%%  LetExpr = free_mbs(MbNameVars),
%%  FreeExpr = pat_syntax:free_expr(pat_syntax:var(MbVar)),
%%  pat_syntax:let_expr(pat_syntax:var(y), FreeExpr, LetExpr).

free_mbs([], Expr) ->
  Expr;
free_mbs([{_, MbVar} | MbNameVars], Expr) ->
  LetExpr = free_mbs(MbNameVars, Expr),
  FreeExpr = pat_syntax:free_expr(pat_syntax:var(MbVar)),
  pat_syntax:let_expr(pat_syntax:var(y), FreeExpr, LetExpr).

-doc "Returns a fresh mailbox name.".
fresh_mb() ->
  paterl_tools:fresh_var(?MB_VAR_NAME).

fresh_mbs(N) ->
  [paterl_tools:fresh_var(?MB_VAR_NAME) || _ <- lists:seq(1, N)].

-doc """
Returns the unit data value of the type for the specified externally-defined
opaque Erlang function.
""".
get_fun_return_unit({call, _, _Fun = {atom, _, Name}, _}) ->
  case maps:find(Name, ?OPAQUE_FUNS) of
    {ok, Type} ->
      get_unit_value(Type);
    error ->
      undefined
  end;
get_fun_return_unit(_) ->
  undefined.

-doc "Returns the unit data value of the type.".
get_unit_value(boolean) ->
  pat_syntax:lit(true);
get_unit_value(integer) ->
  pat_syntax:lit(0);
get_unit_value(float) ->
  pat_syntax:lit(0.0);
get_unit_value(string) ->
  pat_syntax:lit("");
get_unit_value(atom) ->
  pat_syntax:lit(atom);
get_unit_value(unit) ->
  pat_syntax:unit();
get_unit_value(_) ->
  pat_syntax:unit().

-doc "Returns the Pat operator equivalent to the specified Erlang operator.".
to_pat_op(Op) when
  Op =:= '+';
  Op =:= '-';
  Op =:= '*';
  Op =:= '/';
  Op =:= '==';
  Op =:= '<';
  Op =:= '>';
  Op =:= '>=';
  Op =:= '!' ->
  Op;
to_pat_op(Op) when Op =:= '=<' ->
  '<='.

replace(_, _, []) ->
  [];
replace(Elem, NewElem, [Elem | T]) ->
  [NewElem | T];
replace(Elem, NewElem, [H | T]) ->
  [H | replace(Elem, NewElem, T)].

keys([]) ->
  [];
keys([{Key, _} | T]) ->
  [Key | keys(T)].

values([]) ->
  [];
values([{_, Value} | T]) ->
  [Value | values(T)].

new_mb_vars(MbNames) when is_list(MbNames), length(MbNames) > 0 ->
  MbNameVars = [{MbName, fresh_mb()} || MbName <- MbNames],
  {mb_vars(MbNameVars), MbNameVars}.


rebind_mb_var(MbName, MbNameVars)
  when is_list(MbNameVars), length(MbNameVars) > 0 ->
  MbVar = fresh_mb(),
  {MbVar, lists:keyreplace(MbName, 1, MbNameVars, {MbName, MbVar})}.

rebind_mb_vars(MbNameVars) when is_list(MbNameVars), length(MbNameVars) > 0 ->
  MbNameVars0 = [{MbName, fresh_mb()} || {MbName, _} <- MbNameVars],
  {mb_vars(MbNameVars0), MbNameVars0}.

rebind_mb_vars(MbNames, MbNameVars)
  when
  is_list(MbNames), length(MbNames) > 0,
  is_list(MbNameVars), length(MbNameVars) > 0 ->
  {MbVars2, MbNameVars2} =
  lists:foldl(
    fun(MbName, {MbVars0, MbNameVars0}) ->
      {MbVar, MbNameVars1} = rebind_mb_var(MbName, MbNameVars0),
      {[MbVar | MbVars0], MbNameVars1}
    end,
    {[], MbNameVars}, MbNames
  ),
  {lists:reverse(MbVars2), MbNameVars2}.

mb_vars(MbNameVars) when is_list(MbNameVars), length(MbNameVars) > 0 ->
  [MbVar || {_, MbVar} <- MbNameVars].

mb_vars(MbNames, MbNameVars)
  when
  is_list(MbNames), length(MbNames) > 0,
  is_list(MbNameVars), length(MbNameVars) > 0 ->
  lists:foldr(
    fun(MbName, MbVars) -> [mb_var(MbName, MbNameVars) | MbVars] end,
    [], MbNames
  ).

mb_var(MbName, MbNameVars) when is_list(MbNameVars), length(MbNameVars) > 0 ->
  case lists:keyfind(MbName, 1, MbNameVars) of
    {MbName, MbVar} -> MbVar;
    false -> error(lists:flatten(io_lib:format("mailbox interface name ~s does not exist in ~w", [MbName, MbNameVars])))
  end.

mb_names(MbNameVars) when is_list(MbNameVars), length(MbNameVars) > 0 ->
  [MbName || {MbName, _} <- MbNameVars].



