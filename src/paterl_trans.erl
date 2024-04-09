%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%% API
-export([]).
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(MB_IDX_START, 0).

-define(SEP_PAT, [$,, $\s]).

-define(SEP_NL, [$\n]).

-spec translate(erl_syntax:forms()) -> list().
translate(Forms) ->
  translate_forms(Forms).




translate_forms([]) ->
  ["\n"];
translate_forms([Form | Forms]) ->
  [translate_form(Form) | translate_forms(Forms)].

%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------



translate_form({attribute, Anno, module, Name}) ->
  ["# Translated from ", atom_to_list(Name), ".erl\n"];
translate_form({attribute, Anno, interface, {Name, Type, _Vars = []}}) ->
  Type0 = translate_type(Type),
  ["interface ", string:titlecase(atom_to_list(Name)), " {\n", Type0, "\n}\n\n"];

translate_form({function, Anno, Name, Arity, Clauses = [_]}) ->
  % Function with one clause.
  Clauses0 = translate_clauses(Clauses),
  ["def ", atom_to_list(Name), Clauses0, "\n"];
translate_form(_) ->
  % Other module attributes without Pat equivalent.
  [].


translate_type({type, _, pid, _Vars = []}) ->
  % PID type is not translated.
  "";
translate_type({type, _, integer, _Vars = []}) ->
  "Int";
translate_type({type, _, string, _Vars = []}) ->
  "String";
translate_type({user_type, _, Name, _Vars = []}) ->
  [string:titlecase(atom_to_list(Name)), "!"];
translate_type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Message signature type.
  Msg = string:join(translate_type_seq(TypeSeq), ","),
  ["\t", string:titlecase(atom_to_list(Name)), "(", Msg, ")"];
translate_type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Union of message types
  string:join(translate_type_seq(TypeSeq), ",\n").

translate_type_seq([]) ->
  [];
translate_type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  translate_type_seq(TypeSeq);
translate_type_seq([Type | TypeSeq]) ->
  [translate_type(Type) | translate_type_seq(TypeSeq)].


%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------

%% @private Translates receive/case and if clauses.
translate_clauses(Clauses, MbCtx) ->
  ?TRACE("(~s) Translating open clauses.", [MbCtx]),
  {X, Y} =
  lists:foldl(
    fun(Clause, {Clauses0, MbCtx0}) ->
      {Clause1, MbCtx1} = translate_clause(Clause, MbCtx0),
      {[Clause1 | Clauses0], MbCtx1}
    end,
    {[], MbCtx}, Clauses
  ),
  ?TRACE("(~s) Translated clauses: ~p", [Y, X]),
  {lists:reverse(X), Y}.

%%Case: {clause,ANNO,[Rep(P)],[],Rep(B)}.
%%Function: {clause,ANNO,Rep(Ps),[],Rep(B)}
%%If: {clause,ANNO,[],Rep(Gs),Rep(B)}

%% @private Translates a receive/case and if clause.
translate_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, MbCtx) ->
  ?TRACE("Translating receive clause: ~p", [_Clause]),
  % Unguarded receive/case clause.
  PatSeq0 = translate_pat_seq(PatSeq),
%%  PatSeq0 = "xxxx",
  {["receive ", PatSeq0, " from mb"], MbCtx};

translate_clause(_Clause = {clause, Anno, _PatSeq = [], GuardSeq = [_], Body}, MbCtx) ->
  % Unguarded if clause.
  ?TRACE("(~p) About to translate if clause: ~p", [MbCtx, _Clause]),
  {Body0, MbCtx0} = translate_expr_seq(Body, MbCtx),
  ?TRACE("(~p) Translated body of if clause: ~p", [MbCtx0, Body0]),
  If =
    case translate_guard_seq(GuardSeq) of
      [["true"]] ->
        % Else.
        ["{\n", Body0, "\n}"];
      GuardSeq0 ->
        % If.
        ["(", GuardSeq0, ") {\n", Body0, "\n}\n"]
    end,
  {If, MbCtx0}.


translate_expr_seq(ExprSeq, MbCtx) ->
  ?TRACE("(~s) Translating open expression sequence.", [MbCtx]),
  lists:foldl(
    fun(Expr, {ExprSeq0, MbCtx0}) ->
      {Expr1, MbCtx1} = translate_expr(Expr, MbCtx0),
      {[Expr1 | ExprSeq0], MbCtx1}
    end,
    {[], MbCtx}, ExprSeq
  ).

%% @private Translates values and expressions.
translate_expr({call, Anno, Fun = {atom, _, Name}, MFArgs}, MbCtx) ->
  % Explicit function call and explicit mailbox-annotated function call.
  % TODO: Need to translate arguments.
  % TODO: We need to use a tuple and let.
  {["TODO: function call"], MbCtx};
translate_expr({match, Anno, Pat, Expr}, MbCtx) ->
  % Match.
  {["let ", translate_pat(Pat), " = ", translate_expr(Expr), "in\n"], MbCtx};
translate_expr({'if', _, Clauses = [_, _]}, MbCtx) ->
  % If else.
  {[Clause0, Clause1], MbCtx0} = translate_clauses(Clauses, "mb99"),
  ?TRACE("Translated clauses: ~p and ~p", [Clause0, Clause1]),
  {["if ", Clause0, "else ", Clause1], MbCtx0};
translate_expr({call, Anno, Self = {atom, _, self}, []}, MbCtx) ->
  % Self.
  {"(x, x)", MbCtx};
translate_expr({'receive', Anno, Clauses}, MbCtx) ->
  % Receive.
  State = paterl_anno:state(Anno),
  {Clauses0, MbCtx0} = translate_clauses(Clauses, "mb999"),
  % TODO: Logic to determine when to free.

  {["guard mb_: ", State, " {\n", Clauses0, "\n}\n"], MbCtx0};
translate_expr(Expr, MbCtx) ->
  % Literal.
  % Variable.
  % Unguarded function call to closed expression.
  % Binary operator.
  % Unary operator.
  % Spawn.
  {translate_expr(Expr), MbCtx}.


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------

%% @private Translates value and expression sequences.
translate_expr_seq(ExprSeq) ->
  ?TRACE("Have to traslate expr seq: ~p", [ExprSeq]),
  Trans = [translate_expr(Expr) || Expr <- ExprSeq],
  ?TRACE("Translated expr_seq: ~p", [Trans]),
  io:format("TRANSLATED: ~s", [(Trans)]),
%%  string:join([translate_expr(Expr) || Expr <- ExprSeq], ?SEP_NL).
  string:join(Trans, ?SEP_NL).

%% @private Translates values and expressions.
translate_expr(Lit)
  when
% Literal.
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  translate_lit(Lit);
translate_expr({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_expr({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
  Msg = string:titlecase(atom_to_list(Name)),
%%  Args = string:join(translate_expr_seq(Payload), ", "),
  Args = translate_expr_seq(Payload),
  [Msg, "(", Args, ")"];
translate_expr({call, Anno, Fun = {atom, _, Name}, MFArgs}) ->
  % Explicit function call and explicit mailbox-annotated function call.
  % TODO: Need to translate arguments.
  % TODO: We need to use a tuple and let.
  ["TODO: function call"];
translate_expr({match, Anno, Pat, Expr}) ->
  % Match.
  ["let ", translate_pat(Pat), " = ", translate_expr(Expr), "in\n"];
translate_expr({op, Anno, Op, Expr0, Expr1}) ->
  % Binary operator.
  Expr2 = translate_expr(Expr0),
  Expr3 = translate_expr(Expr1),
  [Expr2, " ", atom_to_list(Op), " ", Expr3];
translate_expr({op, _, Op, Expr}) ->
  % Unary operator.
  Expr0 = translate_expr(Expr),
  [atom_to_list(Op), Expr0];
translate_expr({'if', _, Clauses = [_, _]}) ->
  % If else.
  [Clause0, Clause1] = translate_clauses(Clauses),
  ["if ", Clause0, "else ", Clause1];
translate_expr({call, _, Operator = {atom, _, spawn}}) ->
  % Spawn.
  ["TODO: spawn"];
translate_expr(Other) ->
  ?ERROR("Cannot translate: ~p", [Other]).

%% @private Translates closed functions and if clauses.
translate_clauses(Clauses) ->
  [translate_clause(Clause) || Clause <- Clauses].

%% @private Translates closed function and if clauses.
%%translate_clause(_Clause = {clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
%%  % Unguarded function clause.
%%  ?TRACE("Translating function clause: ~p", [_Clause]),
%%
%%  % TODO: I need to case split on the type to generate the return type.
%%
%%  RetType = erl_to_pat_type(paterl_anno:type(Anno)),
%%  Args =
%%    case paterl_anno:interface(Anno) of
%%      undefined ->
%%        % Non mailbox-annotated function.
%%        translate_pat_seq(PatSeq);
%%      Interface ->
%%
%%
%%        % Mailbox-annotated function that requires the mailbox variable to be
%%        % injected.
%%        % TODO: The return type must be a pair now.
%%        MbCtx = new_mb(),
%%        MbVar = erl_syntax:variable(MbCtx),
%%        Anno0 = paterl_anno:set_type(Interface, erl_syntax:get_pos(MbVar)),
%%        MbVar0 = erl_syntax:revert(erl_syntax:set_pos(MbVar, Anno0)),
%%        translate_pat_seq([MbVar0 | PatSeq])
%%    end,
%%
%%  Args0 = string:join(Args, ", "),
%%  ?TRACE("About to translate body!"),
%%%%  Body0 = string:join(translate_expr_seq(Body), "\n"),
%%  Body0 = string:join(translate_expr_seq(Body, MbCtx), "\n"),
%%
%%  ["(", Args0, "): ", RetType, " {\n", Body0, "\n}\n"];

translate_clause(_Clause = {clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Unguarded function clause.
  Params = translate_pat_seq(PatSeq),
  RetType = erl_to_pat_type(paterl_anno:type(Anno)),
  case paterl_anno:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating unguarded NON mailbox-annotated function clause: ~p", [_Clause]),

      Body0 = translate_expr_seq(Body),
      [$(, Params, $), $:, $\s, RetType, $\s, ${, $\n, Body0, $\n, $}, $\n];
    Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating unguarded mailbox-annotated function clause: ~p", [_Clause]),

      Interface0 = erl_to_pat_type(Interface),

      % New mailbox.
      MbCtx = new_mb(),
      {Body0, MbCtx0} = translate_expr_seq(Body, MbCtx),
      ?TRACE("Body is: ~p", [lists:flatten(Body0)]),


      [$(, Params, $), $:, $\s, $(, RetType, $\s, $*, $\s, Interface0, $), $\s, ${, $\n, Body0, $\n, $}, $\n]
  end;





translate_clause({clause, _, _PatSeq = [], GuardSeq = [_], Body}) ->
  % Unguarded if clause.
  Body0 = translate_expr_seq(Body),
  case translate_guard_seq(GuardSeq) of
    [["true"]] ->
      % Else.
      ["{\n", Body0, "\n}"];
    GuardSeq0 ->
      % If.
      ["(", GuardSeq0, ") {\n", Body0, "\n}\n"]
  end.

%% @private Translates a guard sequence.
translate_guard_seq(GuardSeq) ->
  [translate_guard(Guard) || Guard <- GuardSeq].

%% @private Translates a guard, which is a sequence of guard tests.
translate_guard(GuardTests) ->
  [translate_guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates a guard test.
translate_guard_test(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  translate_lit(Lit);
translate_guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Function call.
  GuardTests0 = string:join(translate_guard(GuardTests), ","),
  [atom_to_list(Name), "(", GuardTests0, ")"];
translate_guard_test({op, _, Op, GuardTest0, GuardTest1}) ->
  % Binary operator.
  GuardTest2 = translate_guard_test(GuardTest0),
  GuardTest3 = translate_guard_test(GuardTest1),
  [GuardTest2, " ", atom_to_list(Op), " ", GuardTest3];
translate_guard_test({op, _, Op, GuardTest}) ->
  % Unary operator.
  GuardTest0 = translate_guard_test(GuardTest),
  [atom_to_list(Op), GuardTest0];
translate_guard_test({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name)).


%% @private Translates pattern sequences.
translate_pat_seq(PatSeq) ->
  string:join([translate_pat(Pat) || Pat <- PatSeq], ?SEP_PAT).

%% @private Translates patterns.
translate_pat(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  translate_lit(Lit);
translate_pat({var, _, Name}) ->
%%  Type = erl_to_pat_type(paterl_anno:type(Anno)),
%%  [atom_to_list(Name), ": ", Type];
  atom_to_list(Name);
translate_pat({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message pattern.
  Msg = string:titlecase(atom_to_list(Name)),
  Args = translate_pat_seq(Payload),
  [Msg, "(", Args, ")"].

translate_params() ->
  ok. %TODO: Or have a translate_types and then combine with zipwith.

translate_lit({Type = integer, _, Value}) ->
%%  [integer_to_list(Value), ": ", erl_to_pat_type(Type)];
  integer_to_list(Value);
translate_lit({Type = float, _, Value}) ->
%%  [float_to_list(Value), ": ", erl_to_pat_type(Type)];
  float_to_list(Value);
translate_lit({Type = string, _, Value}) ->
%%  [atom_to_list(Value), ": ", erl_to_pat_type(Type)];
  atom_to_list(Value);
translate_lit({Type = atom, _, Value}) ->
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%%make_mb(MbId) ->
%%  "mb" + integer_to_list(MbId).

%%new_mb1(MbId) ->
%%  {"mb" ++ integer_to_list(MbId), MbId + 1}.

new_mb() ->
  "mb" ++ integer_to_list(?MB_IDX_START).

new_mb([$m, $b | IdStr]) ->
  "mb" ++ integer_to_list(list_to_integer(IdStr) + 1).

erl_to_pat_type(integer) ->
  "Int";
erl_to_pat_type(float) ->
  "Float";
erl_to_pat_type(string) ->
  "String";
erl_to_pat_type(none) ->
  "Unit";
erl_to_pat_type(no_return) ->
  "Unit";
erl_to_pat_type(ok) ->
  "Unit";
erl_to_pat_type(any) ->
  "Unit";
erl_to_pat_type(Mb) when is_atom(Mb) ->
  string:titlecase(atom_to_list(Mb)) ++ "!";
erl_to_pat_type(_) ->
  "Unknown".

