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

-spec translate(erl_syntax:forms()) -> list().
translate(Forms) ->
  translate_forms(Forms).


%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------


translate_forms([]) ->
  ["\n"];
translate_forms([Form | Forms]) ->
  [translate_form(Form) | translate_forms(Forms)].

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


%% @private Translates function clauses.
translate_clauses([]) ->
  [];
translate_clauses([Clause | Clauses]) ->
  [translate_clause(Clause) | translate_clauses(Clauses)].

%% @private Translates a function clause.
translate_clause(_Clause = {clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Unguarded function clause.
  ?TRACE("Translating function clause: ~p", [_Clause]),

  RetType = erl_to_pat_type(paterl_anno:type(Anno)),
  Args =
    case paterl_anno:interface(Anno) of
      undefined ->
        % Non mailbox-annotated function.
        translate_pat_seq(PatSeq);
      Interface ->
        % Mailbox-annotated function that requires the mailbox variable to be
        % injected.
        MbVar = erl_syntax:variable(mb0),
        Anno0 = paterl_anno:set_type(Interface, erl_syntax:get_pos(MbVar)),
        MbVar0 = erl_syntax:revert(erl_syntax:set_pos(MbVar, Anno0)),
        translate_pat_seq([MbVar0 | PatSeq])
    end,

  Args0 = string:join(Args, ", "),
  Body0 = string:join(translate_expr_seq(Body), "\n"),

  ["(", Args0, "): ", RetType, " {\n", Body0, "\n}\n"].

%% @private Translates receive/case and if clauses.
translate_clauses([], _) ->
  [];
translate_clauses([Clause | Clauses], VarId) ->
  [translate_clause(Clause, VarId) | translate_clauses(Clauses, VarId)].

%%Case: {clause,ANNO,[Rep(P)],[],Rep(B)}.
%%Function: {clause,ANNO,Rep(Ps),[],Rep(B)}
%%If: {clause,ANNO,[],Rep(Gs),Rep(B)}

%% @private Translates a receive/case and if clause.
translate_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, VarId) ->
  ?TRACE("Translating receive clause: ~p", [_Clause]),
  % Unguarded receive/case clause.
  PatSeq0 = translate_pat_seq(PatSeq),
%%  PatSeq0 = "xxxx",
  ["receive ", PatSeq0, " from mb" ];

translate_clause({clause, Anno, _PatSeq = [], GuardSeq = [_], Body}, VarId) ->
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







translate_pat_seq([]) ->
  [];
translate_pat_seq([Pat | PatSeq]) ->
  [translate_pat(Pat) | translate_pat_seq(PatSeq)].

translate_pat({var, Anno, Name}) ->
  Type = erl_to_pat_type(paterl_anno:type(Anno)),
  [atom_to_list(Name), ": ", Type];
translate_pat({Type = integer, _, Value}) ->
  [integer_to_list(Value), ": ", erl_to_pat_type(Type)];
translate_pat({Type = float, _, Value}) ->
  [float_to_list(Value), ": ", erl_to_pat_type(Type)];
translate_pat({Type = string, _, Value}) ->
  [atom_to_list(Value), ": ", erl_to_pat_type(Type)];
translate_pat({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
%%  HERE: BUILDING THE RECV AND GUARD
  Msg = string:titlecase(atom_to_list(Name)),
  Args = string:join(translate_expr_seq(Payload), ", "),
  [Msg, "(", Args, ")"].

translate_guard_seq([]) ->
  [];
translate_guard_seq([Guard | GuardSeq]) ->
  % A guard is a sequence of guard tests.
  [translate_guard(Guard) | translate_guard_seq(GuardSeq)].

translate_guard([]) ->
  [];
translate_guard([GuardTest | GuardTests]) ->
  [translate_guard_test(GuardTest) | translate_guard(GuardTests)].


translate_guard_test({integer, _, Value}) ->
  % Integer literal.
  integer_to_list(Value);
translate_guard_test({float, _, Value}) ->
  % Float literal.
  float_to_list(Value);
translate_guard_test({string, _, Value}) ->
  % String literal.
  Value;
translate_guard_test({atom, _, Value}) ->
  % Atom literal.
  atom_to_list(Value);
translate_guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Function call.
  GuardTests0 = string:join(translate_guard(GuardTests), ","),
  [atom_to_list(Name), "(", GuardTests0 , ")"];
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






translate_expr_seq([]) ->
  [];
translate_expr_seq([Expr | ExprSeq]) ->
  [translate_expr(Expr) | translate_expr_seq(ExprSeq)].


translate_expr({call, Anno, Operator = {atom, _, spawn}}) ->
  % Spawn.
  ["TODO: spawn"];
translate_expr({call, Anno, Fun = {atom, _, Name}, MFArgs}) ->
  % Explicit function call
  ["TODO: function call"];
translate_expr({call, Anno, Self = {atom, _, self}, []}) ->
  % Self.
  "(x, x)";
translate_expr({'if', Anno, Clauses = [_, _]}) ->
  % If else.
  [Clause0, Clause1] = translate_clauses(Clauses, -1),
  ["if ", Clause0, "else ", Clause1];
translate_expr({match, Anno, Pat, Expr}) ->
  % Match.
  ["let ", translate_pat(Pat), " = ", translate_expr(Expr), "in\n"];
translate_expr({'receive', Anno, Clauses}) ->
  % Receive.
  State = paterl_anno:state(Anno),
  Clauses0 = translate_clauses(Clauses, -1),
  % TODO: Logic to determine when to free.

  ["guard mb_: ", State, " {\n", Clauses0, "\n}\n"];
translate_expr({tuple, Anno, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
  Msg = string:titlecase(atom_to_list(Name)),
  Args = string:join(translate_expr_seq(Payload), ", "),
  [Msg, "(", Args, ")"];
translate_expr({op, Anno, Op, Expr0, Expr1}) ->
  % Binary operator.
  Expr2 = translate_expr(Expr0),
  Expr3 = translate_expr(Expr1),
  [Expr2, " ", atom_to_list(Op), " ", Expr3];
translate_expr({op, Anno, Op, Expr}) ->
  % Unary operator.
  Expr0 = translate_expr(Expr),
  [atom_to_list(Op), Expr0];
translate_expr({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_expr({integer, _, Value}) ->
  % Integer literal.
  integer_to_list(Value);
translate_expr({float, _, Value}) ->
  % Float literal.
  float_to_list(Value);
translate_expr({string, _, Value}) ->
  % String literal.
  Value;
translate_expr({atom, _, Value}) ->
  % Atom literal.
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------




%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

make_mb(MbId) ->
  "mb" + integer_to_list(MbId).

new_mb() ->
  {1, make_mb(0)}.

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

