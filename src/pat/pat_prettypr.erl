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
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Types.

-define(T_BOOLEAN, "Bool").

-define(T_INTEGER, "Int").

-define(T_FLOAT, "Float").

-define(T_STRING, "String").

-define(T_ATOM, "Atom").

-define(T_UNIT, "Unit").

-define(C_WRITE, "!").

-define(C_READ, "?").

%% Default indent depth.
-define(IND_STEP, 1).

%%% Code formatting.

%% Code column separator.
-define(SEP_COL, [$\s, $\s]).

%% Pattern separator.
-define(SEP_PAT, [$,, $\s]).

%% Product type separator.
-define(SEP_PROD, [$\s, $*, $\s]).

%% Union type separator.
-define(SEP_UNION, [$,, $\n]).

%% Expression sequence separator.
-define(SEP_EXPR_SEQ, [$,, $\s]).

%% Clause separator.
-define(SEP_CLAUSE, io_lib:nl()).

%% Definition separator.
-define(SEP_FORM, io_lib:nl()).



module(Forms) when is_list(Forms) ->
  % A Pat module is a set of interface definitions followed by a set of function
  % definitions in that strict order.
%%  [[form(Form, 0), ?SEP_FORM] || Form <- Forms].
  lists:join(?SEP_FORM, [form(Form, 0) || Form <- Forms]).

form({'interface', _, Name, []}, Col) when is_atom(Name) ->
  % Interface form.
%%  indent(Col, io_lib:format("interface ~s { }~n", [to_type_name(Name)]));
  format_col("interface ~s { }", [to_type_name(Name)], Col);
form({'interface', _, Name, Type}, Col) when is_atom(Name) ->
%%  indent(Col, io_lib:format("interface ~s {~n~s~n}~n", [to_type_name(Name), type(Type)]));
  [
    format_col("interface ~s {~n", [to_type_name(Name)], Col),
    format("~s~n", [type(Type, Col + 1)]),
    format_col("}", Col)
  ];
form({'fun', _, Name, Clauses}, Col) when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
  % Our implementation expects exactly one function clause.
%%  indent(Col, io_lib:format("def ~s~s", [to_name(Name), fun_clauses(Clauses, Col)]));
  format_col("def ~s~s", [to_name(Name), fun_clauses(Clauses, Col)], Col);
form({comment, _, Text}, Col) when is_list(Text) ->
%%  indent(Col, io_lib:format("# ~s~n", [Text])).
  format_col("# ~s", [Text], Col).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

lit_type({'type', _, 'boolean'}) ->
  % Boolean type.
  ?T_BOOLEAN;
lit_type({'type', _, 'integer'}) ->
  % Integer type
  ?T_INTEGER;
lit_type({'type', _, 'float'}) ->
  % Float type.
  ?T_FLOAT;
lit_type({'type', _, 'string'}) ->
  % String type.
  ?T_STRING;
lit_type({'type', _, 'atom'}) ->
  % Atom type.
  ?T_ATOM;
lit_type({'type', _, 'unit'}) ->
  % Unit type.
  ?T_UNIT.

type(Type, Col) when ?IS_LIT_TYPE(Type) ->
  % Literal types.
  format_col(lit_type(Type), Col);
type({'type', _, Name}, Col) when is_atom(Name) ->
  % Mailbox type without modality.
%%  to_type_name(Name);
  format_col(to_type_name(Name), Col);
type({'type', _, Name, 'read'}, Col) when is_atom(Name) ->
  % Mailbox type with read modality.
  format_col(to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_READ)), Col);
type({'type', _, Name, 'write'}, Col) when is_atom(Name) ->
  % Mailbox type with write modality.
  format_col(to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_WRITE)), Col);
type({'type', _, 'product', Types}, Col) when is_list(Types) ->
%%  ?TRACE("Types in product are: ~p~n", [Types]),
  % Product type.
%%  io_lib:format("(~s)", [product_types(Types)]);
  format("(~s)", [product_types(Types, Col)]);
type({'type', _, 'union', Types}, Col) when is_list(Types) ->
  % Union type.
%%  ?TRACE("Types in union are: ~p~n", [Types]),
%%  io_lib:format("~s", [union_types(Types)]);
%%  format_col("~s", [union_types(Types, 0)], Col);
  format("~s", [union_types(Types, Col)]);
type({'type', _, 'msg', Tag, Types}, Col) when is_atom(Tag), is_list(Types) ->
  % Message type.
%%  io_lib:format("~s(~s)", [to_type_name(Tag), msg_types(Types)]).
  format_col("~s(~s)", [to_type_name(Tag), msg_types(Types, 0)], Col).

%% @private Generic.
types(Types, Col) when is_list(Types) ->
  [type(Type, Col) || Type <- Types].

product_types(Types, Col) when is_list(Types) ->
%%  string:join(types(Types, Col), ?SEP_PROD).
  lists:join(?SEP_PROD, types(Types, Col)).

union_types(Types, Col) when is_list(Types) ->
%%  string:join(types(Types, Col), ?SEP_UNION).
  lists:join(?SEP_UNION, types(Types, Col)).

msg_types(Types, Col) when is_list(Types) ->
%%  string:join(types(Types, Col), ?SEP_PAT).
  lists:join(?SEP_PAT, types(Types, Col)).


%%% ----------------------------------------------------------------------------
%%% Functions.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints a function clause.
fun_clause({'fun_clause', _, Params, Expr, RetType}, N) when is_list(Params)->
  % Function clause.
  io_lib:format("(~s): ~s {~n~s~n}", [params(Params), type(RetType, 0), expr(Expr, N + 1)]).

%% @private Pretty prints a list of function clauses.
fun_clauses(Clauses, N) when is_list(Clauses) ->
  % Function clauses.
%%  string:join([fun_clause(Clause, N) || Clause <- Clauses], ?SEP_CLAUSE).
  lists:join(?SEP_CLAUSE, [fun_clause(Clause, N) || Clause <- Clauses]).


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints patterns.
pat(Var) when ?IS_VAR(Var) ->
  % Variable pattern.
  var(Var);
pat(Lit) when ?IS_LIT(Lit) ->
  % Pat literal patterns.
  lit(Lit);
pat({'pat', _, 'msg', Tag, PatSeq}) when is_atom(Tag), is_list(PatSeq) ->
  % Message pattern.
%%  io_lib:format("~s(~s)", [to_type_name(Tag), pat_seq(PatSeq)]).
  format("~s(~s)", [to_type_name(Tag), pat_seq(PatSeq)]).

%% @private Pretty prints a pattern sequence.
pat_seq(PatSeq) when is_list(PatSeq) ->
  % Pattern sequence.
%%  string:join([pat(Pat) || Pat <- PatSeq], ?SEP_PAT).
%%  [[pat(Pat), ?SEP_PAT] || Pat <- PatSeq].
  lists:join(?SEP_PAT, [pat(Pat) || Pat <- PatSeq]).

%% @private Pretty prints a parameter and its type.
param({'pat', _, Var, Type}) ->
  % Parameter pattern.
%%  io_lib:format("~s: ~s", [var(Var), type(Type, 0)]).
  format("~s: ~s", [var(Var), type(Type, 0)]).


%%% ----------------------------------------------------------------------------
%%% Values.
%%% ----------------------------------------------------------------------------

var({'var', _, Name}) when is_atom(Name) ->
  % Variable value.
  to_name(Name).

lit({'boolean', _, Value}) when is_boolean(Value) ->
  % Boolean value.
  atom_to_list(Value);
lit({'integer', _, Value}) when is_integer(Value) ->
  % Integer value.
  integer_to_list(Value);
lit({'float', _, Value}) when is_float(Value) ->
  % Float value.
  float_to_list(Value);
lit({'string', _, Value}) when is_list(Value) ->
  % String value.
  [$", Value, $"];
lit({'atom', _, Value}) when is_atom(Value) ->
  % Atom value.
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------


expr(Var, Col) when ?IS_VAR(Var) ->
  % Variable expression.
%%  indent(Col, var(Var));
  format_col("~s", [var(Var)], Col);
expr(Lit, Col) when ?IS_LIT(Lit) ->
  % Literal expressions.
%%  indent(Col, lit(Lit));
  format_col("~s", [lit(Lit)], Col);
expr({'tuple', _, ExprSeq}, Col) when is_list(ExprSeq) ->
  % Tuple expression.
%%  indent(Col, io_lib:format("(~s)", [expr_seq(ExprSeq, 0)]));
  format_col("(~s)", [expr_seq(ExprSeq, 0)], Col);
expr({'unit', _}, Col) ->
  % Unit expression.
  format_col("()", Col);
%%  "()";
expr({'msg', _, Tag, ExprSeq}, Col) when is_atom(Tag), is_list(ExprSeq) ->
  % Message expression.
%%  io_lib:format("~s(~s)", [to_type_name(Tag), expr_seq(ExprSeq, 0)]);
  format_col("~s(~s)", [to_type_name(Tag), expr_seq(ExprSeq, 0)], Col);
expr({'op', _, Op, ExprL, ExprR}, Col)
  when ?IS_OP(Op) ->
  % Binary operator expression.
%%  io_lib:format("~s ~s ~s", [expr(ExprL, 0), Op, expr(ExprR, 0)]);
  format_col("~s ~s ~s", [expr(ExprL, 0), Op, expr(ExprR, 0)], Col);
expr({'op', _, Op, Expr}, Col)
  when ?IS_OP(Op) ->
  % Unary operator expression.
%%  io_lib:format("~s~s", [Op, expr(Expr, 0)]);
  format_col("~s~s", [Op, expr(Expr, 0)], Col);
expr({'call', _, Name, ExprSeq}, Col) when is_atom(Name), is_list(ExprSeq) ->
  % Function call expression.
%%  io_lib:format("~s(~s)", [Name, expr_seq(ExprSeq, 0)]);
  format_col("~s(~s)", [Name, expr_seq(ExprSeq, 0)], Col);
expr({'if', _, ExprC, ExprT, ExprF}, Col)
  when ?IS_EXPR(ExprC), ?IS_EXPR(ExprT), ?IS_EXPR(ExprF) ->
  % If expression.
%%  io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [expr(ExprC, 0), expr(ExprT, Col + 1), expr(ExprF, Col + 1)]);
  [
    format_col("if (~s) {~n", [expr(ExprC, 0)], Col),
    format("~s~n", [expr(ExprT, Col + 1)]),
    format_col("}~n", Col),
    format_col("else {~n", Col),
    format("~s~n", [expr(ExprF, Col + 1)]),
    format_col("}", Col)
  ];

expr({'let', _, Binders, Expr0, Expr1}, Col) when ?IS_EXPR(Expr0), ?IS_EXPR(Expr1) ->
  % Let expression.
%%  indent(
%%    N,
%%    io_lib:format("let ~s =~n~s~nin~n~s", [expr(Binders, 0), expr(Expr0, N + 1), expr(Expr1, N + 1)])
%%  );
  [
    format_col("let ~s =~n", [expr(Binders, 0)], Col),
    format("~s~n", [expr(Expr0, Col + 1)]),
    format_col("in~n", Col),
    expr(Expr1, Col + 1)
  ];

expr({'new', _, MbType}, Col) when ?IS_MB_TYPE(MbType) ->
  % New expression.
%%  io_lib:format("new [~s]", [type(MbType)]);
  format_col("new [~s]", [type(MbType, 0)], Col);

expr({'free', _, Var}, Col) when ?IS_VAR(Var) ->
  % Free expression.
  %%  io_lib:format("free(~s)", [var(Var)]);
  format_col("free(~s)", [var(Var)], Col);

expr({'spawn', _, Expr}, Col) when ?IS_EXPR(Expr)->
  % Spawn expression.
%%  io_lib:format("spawn {~n~s~n}", [expr(Expr, Col + 1)]);
  [
    format_col("spawn {~n", Col),
    format("~s~n", [expr(Expr, Col + 1)]),
    format_col("}", Col)
  ];

expr({'guard', _, Var, Regex, Clauses}, Col) when ?IS_VAR(Var), is_list(Clauses) ->
  % Guard expression.
%%  io_lib:format("guard ~s: ~s {~n~s~n}", [var(Var), Regex, case_clauses(Clauses, N)]);
  [
    format_col("guard ~s: ~s {~n", [var(Var), Regex], Col),
    format("~s~n", [case_clauses(Clauses, Col + 1)]),
    format_col("}", Col)
  ];

expr({'empty', _, RebindVar, Expr}, Col) when ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  % Empty expression.
%%  io_lib:format("empty(~s) ->~n~s", [var(RebindVar), expr(Expr, Col + 1)]);
  [
    format_col("empty(~s) ->~n", [var(RebindVar)], Col),
    expr(Expr, Col + 1)
  ];

expr({'receive', _, MsgPat, RebindVar, Expr}, Col)
  when ?IS_MSG_PAT(MsgPat), ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  % Receive expression.
%%  ?TRACE("Pretty-printing receive."),
%%  ?TRACE("Message pattern ~p", [Msg]),
%%  io_lib:format("receive ~s from ~s ->~n~s", [pat(MsgPat), var(RebindVar), expr(Expr, N + 1)]);
  [
    format_col("receive ~s from ~s ->~n", [pat(MsgPat), var(RebindVar)], Col),
    expr(Expr, Col + 1)
  ];

expr({comment, _, Text}, Col) when is_list(Text) ->
%%  io_lib:format("# ~s~n", [Text]).
  format_col("# ~s~n", [Text], Col).

%% @private Generic expression sequence
expr_seq(ExprSeq, Col) when is_list(ExprSeq) ->
  string:join([expr(Expr, Col) || Expr <- ExprSeq], ?SEP_EXPR_SEQ).

%%inline_expr_seq(ExprSeq) when is_list(ExprSeq) ->
%%  string:join(expr_seq(ExprSeq), ?SEP_INLINE_EXPR).

%%block_expr_seq(ExprSeq) when is_list(ExprSeq) ->
%%  string:join(expr_seq(ExprSeq), ?SEP_BLOCK_EXPR).

case_clauses(Clauses, Col) when is_list(Clauses) ->
%%  block_expr_seq(Clauses).
  % Clauses are expressions.
  string:join([expr(Clause, Col) || Clause <- Clauses], ?SEP_CLAUSE).

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

%%op(Op) when is_atom(Op) -> %TODO: Make precise with all operators.
%%  atom_to_list(Op).



params(Params) when is_list(Params) ->
  string:join([param(Param) || Param <- Params], ?SEP_PAT).

to_type_name(Name) when is_atom(Name) ->
  string:titlecase(atom_to_list(Name)).

to_name(Name) when is_atom(Name) ->
  string:lowercase(atom_to_list(Name)).





%%indent(Lines) ->
%%  Lines0 = string:split(lists:flatten(Lines), ?SEP_CLAUSE, all),
%%  io:format("Lines to indent: ~p~n~n~n", [Lines0]),
%%  indent(Lines0, 0).
%%
%%indent([], _) ->
%%  [];
%%
%%indent([Line = [$i, $n, $t, $e, $r, $f, $a, $c, $e | _] | Lines], _) ->
%%  io:format("Found interface { (indent ~p).~n", [0]),
%%  [[10, Line] | indent(Lines, 1)];
%%
%%indent([Line = [$d, $e, $f | _] | Lines], _) ->
%%  io:format("Found def { (indent ~p).~n", [0]),
%%  [[10, Line] | indent(Lines, 1)];
%%
%%indent([Line = [$} | _] | Lines], Level) ->
%%  io:format("Found closing } (indent ~p).~n", [Level - 1]),
%%  [indent(Level - 1, Line) | indent(Lines, Level - 1)];
%%
%%indent([Line = [$l, $e, $t | _] | Lines], Level) ->
%%  io:format("Found let (indent ~p).~n", [Level]),
%%  [indent(Level, Line) | indent(Lines, Level + 1)];
%%
%%indent([Line = [$i, $n | _] | Lines], Level) ->
%%  io:format("Found in (indent ~p).~n", [Level - 1]),
%%  [indent(Level - 1, Line), indent(Lines, Level)];
%%
%%indent([Line = [$i, $f | _] | Lines], Level) ->
%%  io:format("Found if (indent ~p).~n", [Level]),
%%  [indent(Level, Line) | indent(Lines, Level + 1)];
%%
%%indent([Line = [$e, $l, $s, $e | _] | Lines], Level) ->
%%  io:format("Found else (indent ~p).~n", [Level]),
%%  [indent(Level, Line), indent(Lines, Level + 1)];
%%
%%indent([Line = [$s, $p, $a, $w, $n | _] | Lines], Level) ->
%%  io:format("Found spawn (indent ~p).~n", [Level]),
%%  [indent(Level, Line), indent(Lines, Level + 1)];
%%
%%indent([Line = [$g, $u, $a, $r, $d | _] | Lines], Level) ->
%%  io:format("Found receive (indent ~p).~n", [Level]),
%%  [indent(Level, Line), indent(Lines, Level + 1)];
%%
%%indent([Line = [$r, $e, $c, $e, $i, $v, $e | _] | Lines], Level) ->
%%  io:format("Found receive (indent ~p).~n", [Level]),
%%  [indent(Level, Line), indent(Lines, Level + 1)];
%%
%%indent([Line = [$e, $m, $p, $t, $y | _] | Lines], Level) ->
%%  io:format("Found empty (indent ~p).~n", [Level]),
%%  [indent(Level, Line), indent(Lines, Level + 1)];
%%
%%indent([Line | Lines], Level) ->
%%  io:format("Found other ~s (indent ~p).~n", [Line, Level]),
%%  [indent(Level, Line) | indent(Lines, Level)].

%%indent(N, Line) ->
%%  lists:flatten([?SEP_CLAUSE, lists:duplicate(N, ?SEP_TAB), Line]).

indent(N, Line) ->
  lists:flatten([lists:duplicate(N, ?SEP_COL), Line]).

%%fterm(Format, Data, N) ->
%%  Col = lists:duplicate(N, ?SEP_TAB),
%%  [Col, io_lib:format(Format, Data)].

%%fterm(Format, N) when is_integer(N) ->
%%  fterm(Format, [], N);
%%fterm(Format, Data) ->
%%  fterm(Format, Data, 0).

%%fterm(Format) ->
%%  fterm(Format, 0).

format_col(Format, Data, Col)
  when is_list(Format), is_list(Data), is_integer(Col) ->
  [lists:duplicate(Col, ?SEP_COL), io_lib:format(Format, Data)].

format_col(Format, Col) when is_list(Format), is_integer(Col) ->
  format_col(Format, [], Col).

format(Format, Data) when is_list(Format), is_list(Data) ->
  io_lib:format(Format, Data).

format(Format) when is_list(Format) ->
  format(Format, []).

