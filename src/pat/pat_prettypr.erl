%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2024 10:29
%%%-------------------------------------------------------------------
-module(pat_prettypr).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("pat.hrl").


%%% API
-export([module/1]).
%%-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(SEP_CLAUSE, [$\n]).

-define(SEP_TAB, [$\t]).

-define(SEP_PAT, [$,, $\s]).

-define(SEP_PROD, [$\s, $*, $\s]).

-define(SEP_UNION, [$,, $\n]).

-define(SEP_INLINE_EXPR, [$,, $\s]).

%%-define(SEP_BLOCK_EXPR, [$\n]).

%%-define(SEP_NL, [$\n]).

-define(T_BOOLEAN, "Bool").

-define(T_INTEGER, "Int").

-define(T_FLOAT, "Float").

-define(T_STRING, "String").

-define(T_ATOM, "Atom").

-define(T_UNIT, "Unit").

-define(C_WRITE, "!").

-define(C_READ, "?").




module(Forms) when is_list(Forms) ->
  % A Pat module is a set of interface definitions followed by a set of function
  % definitions in that strict order.
  [form(Form) || Form <- Forms].

form({'interface', _, Name, []}) when is_atom(Name) ->
  io_lib:format("interface ~s { }~n", [to_type_name(Name)]);
form({'interface', _, Name, Type}) when is_atom(Name) ->
  io_lib:format("interface ~s {~n~s~n}~n", [to_type_name(Name), type(Type)]);
form({'fun', _, Name, Clauses}) when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
%%  Clauses0 = [fun_clause(Clause) || Clause <- Clauses],
  io_lib:format("def ~s~s", [to_name(Name), fun_clauses(Clauses)]);
form({comment, _, Text}) when is_list(Text) ->
  io_lib:format("# ~s~n", [Text]).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------




lit_type({'type', _, 'boolean'}) ->
  ?T_BOOLEAN;
lit_type({'type', _, 'integer'}) ->
  ?T_INTEGER;
lit_type({'type', _, 'float'}) ->
  ?T_FLOAT;
lit_type({'type', _, 'string'}) ->
  ?T_STRING;
lit_type({'type', _, 'atom'}) ->
  ?T_ATOM;
lit_type({'type', _, 'unit'}) ->
  ?T_UNIT.

type(Type) when ?IS_LIT_TYPE(Type) ->
  % Literal types.
  lit_type(Type);
type({'type', _, Name}) when is_atom(Name) ->
  % Mailbox type without modality.
  to_type_name(Name);
type({'type', _, Name, 'read'}) when is_atom(Name) ->
  % Mailbox type with read modality.
  to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_READ));
type({'type', _, Name, 'write'}) when is_atom(Name) ->
  % Mailbox type with write modality.
  to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_WRITE));
type({'type', _, 'product', Types}) when is_list(Types) ->
  ?TRACE("Types in product are: ~p~n", [Types]),
  % Product type.
  io_lib:format("(~s)", [product_types(Types)]);
type({'type', _, 'union', Types}) when is_list(Types) ->
  % Union type.
  ?TRACE("Types in union are: ~p~n", [Types]),
  io_lib:format("~s", [union_types(Types)]);
type({'type', _, 'msg', Tag, Types}) when is_atom(Tag), is_list(Types) ->
  % Message type.
  io_lib:format("~s(~s)", [to_type_name(Tag), msg_types(Types)]).

types(Types) when is_list(Types) ->
  [type(Type) || Type <- Types].

product_types(Types) when is_list(Types) ->
  string:join(types(Types), ?SEP_PROD).

union_types(Types) when is_list(Types) ->
  string:join(types(Types), ?SEP_UNION).

msg_types(Types) when is_list(Types) ->
  string:join(types(Types), ?SEP_PAT).


%%interface_def({'interface', _, Name, []}) when is_atom(Name) ->
%%  io_lib:format("interface ~s { }~n", [to_type_name(Name)]);
%%interface_def({'interface', _, Name, Type}) when is_atom(Name) ->
%%  io_lib:format("interface ~s {~n~s~n}~n", [to_type_name(Name), Type]).


%%% ----------------------------------------------------------------------------
%%% Functions.
%%% ----------------------------------------------------------------------------

%%fun_def({'fun', _, Name, Clauses}) when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
%%  io_lib:format("def ~s~s", [to_name(Name), seq_nl(Clauses)]).

fun_clause({'fun_clause', _, Params, Expr, RetType}) when is_list(Params) ->
  % Pat function clause.
  ?TRACE("Params are: ~p~n", [params(Params)]),
  io_lib:format("(~s): ~s {~n~s~n}~n", [params(Params), type(RetType), expr(Expr)]).

fun_clauses(Clauses) when is_list(Clauses) ->
  % Pat function clauses.
  string:join([fun_clause(Clause) || Clause <- Clauses], ?SEP_CLAUSE).


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

pat(Var) when element(1, Var) =:= 'var' ->
  % Pat variables.
  var(Var);
pat(Lit)
  when
  element(1, Lit) =:= boolean;
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Pat literals.
  lit(Lit);
pat({'pat', _, 'msg', Tag, PatSeq}) when is_atom(Tag), is_list(PatSeq) ->
%%  ?TRACE("Pretty printing message pattern."),
  io_lib:format("~s(~s)", [to_type_name(Tag), pat_seq(PatSeq)]).


pat_seq(PatSeq) when is_list(PatSeq) ->
  string:join([pat(Pat) || Pat <- PatSeq], ?SEP_PAT).

param({'pat', _, Var, Type}) ->
  ?TRACE("Var is: ~p", [Var]),
  ?TRACE("Type is: ~p", [Type]),
  io_lib:format("~s: ~s", [var(Var), type(Type)]).


%%% ----------------------------------------------------------------------------
%%% Values.
%%% ----------------------------------------------------------------------------

var({'var', _, Name}) when is_atom(Name) ->
  to_name(Name).

lit({'boolean', _, Value}) when is_boolean(Value) ->
  atom_to_list(Value);
lit({'integer', _, Value}) when is_integer(Value) ->
  integer_to_list(Value);
lit({'float', _, Value}) when is_float(Value) ->
  float_to_list(Value);
lit({'string', _, Value}) when is_list(Value) ->
  [$", Value, $"];
lit({'atom', _, Value}) when is_atom(Value) ->
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------


expr(Var) when ?IS_VAR(Var) ->
  % Pat variable.
  var(Var);
expr(Lit) when ?IS_LIT(Lit) ->
%%  when
%%  element(1, Lit) =:= boolean;
%%  element(1, Lit) =:= integer;
%%  element(1, Lit) =:= float;
%%  element(1, Lit) =:= string;
%%  element(1, Lit) =:= atom ->
  % Pat literals.
  lit(Lit);

expr({'tuple', _, ExprSeq}) when is_list(ExprSeq) ->
  % Pat tuple.
  io_lib:format("(~s)", [expr_seq(ExprSeq)]);

expr({'unit', _}) ->
  % Pat unit.
  "()";

expr({'msg', _, Tag, ExprSeq}) when is_atom(Tag), is_list(ExprSeq) ->
  % Pat message.
  io_lib:format("~s(~s)", [to_type_name(Tag), expr_seq(ExprSeq)]);

expr({'op', _, Op, ExprL, ExprR})
  when ?IS_OP(Op) ->
%%  Op =:= '+';
%%  Op =:= '-';
%%  Op =:= '*';
%%  Op =:= '/';
%%  Op =:= '==';
%%  Op =:= '!' ->
  io_lib:format("~s ~s ~s", [expr(ExprL), Op, expr(ExprR)]);
expr({'op', _, Op, Expr})
  when ?IS_OP(Op) ->
%%  when
%%  Op =:= '+';
%%  Op =:= '-';
%%  Op =:= '*';
%%  Op =:= '/';
%%  Op =:= '==';
%%  Op =:= '!' ->
  io_lib:format("~s~s", [Op, expr(Expr)]);


expr({'call', _, Name, ExprSeq}) when is_atom(Name), is_list(ExprSeq) ->
  io_lib:format("~s(~s)", [Name, expr_seq(ExprSeq)]);

expr({'if', _, ExprC, ExprT, ExprF})
  when ?IS_EXPR(ExprC), ?IS_EXPR(ExprT), ?IS_EXPR(ExprF) ->
  io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [expr(ExprC), expr(ExprT), expr(ExprF)]);

expr({'let', _, Binders, Expr0, Expr1}) when ?IS_EXPR(Expr0), ?IS_EXPR(Expr1) ->
%%  io_lib:format("let ~s =~n~s~nin~n~s", [var(Binders), expr(Expr0), expr(Expr1)]);
  io_lib:format("let ~s =~n~s~nin~n~s", [expr(Binders), expr(Expr0), expr(Expr1)]);

expr({'new', _, MbType}) when ?IS_MB_TYPE(MbType) ->
  io_lib:format("new [~s]", [type(MbType)]);

expr({'free', _, Var}) when ?IS_VAR(Var) ->
  io_lib:format("free(~s)", [var(Var)]);

expr({'spawn', _, Expr}) when ?IS_EXPR(Expr)->
  io_lib:format("spawn {~n~s~n}", [expr(Expr)]);

expr({'guard', _, Var, Regex, Clauses}) when ?IS_VAR(Var), is_list(Clauses) ->
  io_lib:format("guard ~s: ~s {~n~s~n}", [var(Var), Regex, case_clauses(Clauses)]);

expr({'empty', _, RebindVar, Expr}) when ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  io_lib:format("empty(~s) ->~n~s", [var(RebindVar), expr(Expr)]);

expr({'receive', _, MsgPat, RebindVar, Expr})
  when ?IS_MSG_PAT(MsgPat), ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  ?TRACE("Pretty-printing receive."),
%%  ?TRACE("Message pattern ~p", [Msg]),
  io_lib:format("receive ~s from ~s ->~n~s", [pat(MsgPat), var(RebindVar), expr(Expr)]);

expr({comment, _, Text}) when is_list(Text) ->
  io_lib:format("# ~s~n", [Text]).

%% @private Generic expression sequence
expr_seq(ExprSeq) when is_list(ExprSeq) ->
  string:join([expr(Expr) || Expr <- ExprSeq], ?SEP_INLINE_EXPR).

%%inline_expr_seq(ExprSeq) when is_list(ExprSeq) ->
%%  string:join(expr_seq(ExprSeq), ?SEP_INLINE_EXPR).

%%block_expr_seq(ExprSeq) when is_list(ExprSeq) ->
%%  string:join(expr_seq(ExprSeq), ?SEP_BLOCK_EXPR).

case_clauses(Clauses) when is_list(Clauses) ->
%%  block_expr_seq(Clauses).
  % Clauses are expressions.
  string:join([expr(Clause) || Clause <- Clauses], ?SEP_CLAUSE).

%%% ----------------------------------------------------------------------------
%%% Utility.
%%% ----------------------------------------------------------------------------

%%seq_comma(Types) when is_list(Types) ->
%%%%  string:join(Types, ?SEP_PAT).
%%%%  [type(Type) || Type <- Types]. %% TODO: Add commas.
%%  string:join(types(Types), ?SEP_PAT).

%%seq_prod(Types) when is_list(Types) ->
%%%%  string:join(Args, ?SEP_PROD).
%%  string:join(types(Types), ?SEP_PROD).

%%seq_union(Types) when is_list(Types) ->
%%%%  [type(Type) || Type <- Types]. %% TODO: Add commas.
%%%%  string:join(Types, ?SEP_UNION).
%%  string:join(types(Types), ?SEP_UNION).

%%seq_nl(Clauses) when is_list(Clauses) ->
%%  string:join(Clauses, ?SEP_CLAUSE).

op(Op) when is_atom(Op) -> %TODO: Make precise with all operators.
  atom_to_list(Op).



params(Params) when is_list(Params) ->
  string:join([param(Param) || Param <- Params], ?SEP_PAT).

to_type_name(Name) when is_atom(Name) ->
  string:titlecase(atom_to_list(Name)).

to_name(Name) when is_atom(Name) ->
  string:lowercase(atom_to_list(Name)).





indent(Lines) ->
  Lines0 = string:split(lists:flatten(Lines), ?SEP_CLAUSE, all),
  io:format("Lines to indent: ~p~n~n~n", [Lines0]),
  indent(Lines0, 0).

indent([], _) ->
  [];

indent([Line = [$i, $n, $t, $e, $r, $f, $a, $c, $e | _] | Lines], _) ->
  io:format("Found interface { (indent ~p).~n", [0]),
  [[10, Line] | indent(Lines, 1)];

indent([Line = [$d, $e, $f | _] | Lines], _) ->
  io:format("Found def { (indent ~p).~n", [0]),
  [[10, Line] | indent(Lines, 1)];

indent([Line = [$} | _] | Lines], Level) ->
  io:format("Found closing } (indent ~p).~n", [Level - 1]),
  [tabs(Level - 1, Line) | indent(Lines, Level - 1)];

indent([Line = [$l, $e, $t | _] | Lines], Level) ->
  io:format("Found let (indent ~p).~n", [Level]),
  [tabs(Level, Line) | indent(Lines, Level + 1)];

indent([Line = [$i, $n | _] | Lines], Level) ->
  io:format("Found in (indent ~p).~n", [Level - 1]),
  [tabs(Level - 1, Line), indent(Lines, Level)];

indent([Line = [$i, $f | _] | Lines], Level) ->
  io:format("Found if (indent ~p).~n", [Level]),
  [tabs(Level, Line) | indent(Lines, Level + 1)];

indent([Line = [$e, $l, $s, $e | _] | Lines], Level) ->
  io:format("Found else (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$s, $p, $a, $w, $n | _] | Lines], Level) ->
  io:format("Found spawn (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$g, $u, $a, $r, $d | _] | Lines], Level) ->
  io:format("Found receive (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$r, $e, $c, $e, $i, $v, $e | _] | Lines], Level) ->
  io:format("Found receive (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line = [$e, $m, $p, $t, $y | _] | Lines], Level) ->
  io:format("Found empty (indent ~p).~n", [Level]),
  [tabs(Level, Line), indent(Lines, Level + 1)];

indent([Line | Lines], Level) ->
  io:format("Found other ~s (indent ~p).~n", [Line, Level]),
  [tabs(Level, Line) | indent(Lines, Level)].

tabs(N, Line) ->
  lists:flatten([?SEP_CLAUSE, lists:duplicate(N, ?SEP_TAB), Line]).