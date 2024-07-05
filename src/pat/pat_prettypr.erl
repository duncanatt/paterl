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


%%% API.
-export([module/1]).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Pat data type names.

%% Boolean type.
-define(TYP_BOOLEAN, "Bool").

%% Integer type.
-define(TYP_INTEGER, "Int").

%% Float type.
-define(TYP_FLOAT, "Float").

%% String type.
-define(TYP_STRING, "String").

%% Atom type.
-define(TYP_ATOM, "Atom").

%% Unit type.
-define(TYP_UNIT, "Unit").

%% Mailbox write capability.
-define(CAP_WRITE, "!").

%% Mailbox read capability.
-define(CAP_READ, "?").

%%% Code formatting.

%% Default indent depth.
-define(IND_STEP, 1).

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

%% Expression separator.
-define(SEP_EXPR, io_lib:nl()).

%% Block separator.
-define(SEP_BLOCK, io_lib:nl()).

%% Interface and function definition separator.
-define(SEP_FORM, [?SEP_BLOCK, ?SEP_BLOCK]).

%% Clause separator.
-define(SEP_CLAUSE, ?SEP_BLOCK).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

-spec module(list()) -> iolist(). % TODO: Make the list() more specific by defining a type in pat_syntax.
%% @doc Pretty prints a list of Pat form nodes.
module(Forms) when is_list(Forms) ->
  % A Pat module is a set of interface definitions followed by a set of function
  % definitions in that strict order.
  lists:join(?SEP_FORM, [form(Form, 0) || Form <- Forms]).


%%% ----------------------------------------------------------------------------
%%% Forms.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints form nodes.
form({interface, _, Name, []}, Col) when is_atom(Name) ->
  % Interface form.
  format_col("interface ~s { }", [to_type_name(Name)], Col);
form({interface, _, Name, Type}, Col) when is_atom(Name) ->
  [
    format_col("interface ~s {", [to_type_name(Name)], Col),
    ?SEP_EXPR,
    type(Type, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("}", Col)
  ];
form({'fun', _, Name, Clauses}, Col)
  when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
  % Our implementation expects exactly one function clause.
  format_col("def ~s~s", [to_name(Name), fun_clauses(Clauses, Col)], Col);
form({comment, _, Text}, Col) when is_list(Text) ->
  format_col("# ~s", [Text], Col).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints literal type nodes.
lit_type({type, _, boolean}) ->
  % Boolean type.
  ?TYP_BOOLEAN;
lit_type({type, _, integer}) ->
  % Integer type
  ?TYP_INTEGER;
lit_type({type, _, float}) ->
  % Float type.
  ?TYP_FLOAT;
lit_type({type, _, string}) ->
  % String type.
  ?TYP_STRING;
lit_type({type, _, atom}) ->
  % Atom type.
  ?TYP_ATOM;
lit_type({type, _, unit}) ->
  % Unit type.
  ?TYP_UNIT.

%% @private Pretty prints type nodes.
type(Type, Col) when ?IS_LIT_TYPE(Type) ->
  % Literal types.
  format_col(lit_type(Type), Col);
type({type, _, Name}, Col) when is_atom(Name) ->
  % Mailbox type without modality.
  format_col(to_type_name(Name), Col);
type({type, _, Name, read}, Col) when is_atom(Name) ->
  % Mailbox type with read modality.
  format_col(to_type_name(list_to_atom(atom_to_list(Name) ++ ?CAP_READ)), Col);
type({type, _, Name, write}, Col) when is_atom(Name) ->
  % Mailbox type with write modality.
  format_col(to_type_name(list_to_atom(atom_to_list(Name) ++ ?CAP_WRITE)), Col);
type({type, _, product, Types}, Col) when is_list(Types) ->
  % Product type.
  format("(~s)", [product_types(Types, Col)]);
type({type, _, union, Types}, Col) when is_list(Types) ->
  % Union type.
  format("~s", [union_types(Types, Col)]);
type({type, _, msg, Tag, Types}, Col) when is_atom(Tag), is_list(Types) ->
  % Message type.
  format_col("~s(~s)", [to_type_name(Tag), msg_types(Types, 0)], Col).

%% @private Pretty prints a generic type node list.
types(Types, Col) when is_list(Types) ->
  [type(Type, Col) || Type <- Types].

%% @private Pretty prints a type node list as a product.
product_types(Types, Col) when is_list(Types) ->
  lists:join(?SEP_PROD, types(Types, Col)).

%% @private Pretty prints a type node list as a union.
union_types(Types, Col) when is_list(Types) ->
  lists:join(?SEP_UNION, types(Types, Col)).

%% @private Pretty prints a type node list as message arguments.
msg_types(Types, Col) when is_list(Types) ->
  lists:join(?SEP_PAT, types(Types, Col)).


%%% ----------------------------------------------------------------------------
%%% Functions.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints a function clause node.
fun_clause({fun_clause, _, Params, Expr, RetType}, Col) when is_list(Params) ->
  % Function clause.
  [
    format("(~s): ~s {", [params(Params), type(RetType, 0)]),
    ?SEP_EXPR,
    expr(Expr, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("}", Col)
  ].

%% @private Pretty prints a list of function clause nodes.
fun_clauses(Clauses, Col) when is_list(Clauses) ->
  % Function clauses.
  lists:join(?SEP_CLAUSE, [fun_clause(Clause, Col) || Clause <- Clauses]).


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints pattern nodes.
pat(Var) when ?IS_VAR(Var) ->
  % Variable pattern.
  var(Var);
pat(Lit) when ?IS_LIT(Lit) ->
  % Literal patterns.
  lit(Lit);
pat({pat, _, msg, Tag, PatSeq}) when is_atom(Tag), is_list(PatSeq) ->
  % Message pattern.
  format("~s(~s)", [to_type_name(Tag), pat_seq(PatSeq)]).

%% @private Pretty prints a pattern node sequence.
pat_seq(PatSeq) when is_list(PatSeq) ->
  % Pattern sequence.
  lists:join(?SEP_PAT, [pat(Pat) || Pat <- PatSeq]).

%% @private Pretty prints a parameter node and its type.
param({pat, _, Var, Type}) ->
  % Parameter pattern.
  format("~s: ~s", [var(Var), type(Type, 0)]).

%% @private Pretty prints a parameter node sequence.
params(Params) when is_list(Params) ->
  lists:join(?SEP_PAT, [param(Param) || Param <- Params]).


%%% ----------------------------------------------------------------------------
%%% Values.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints a variable node.
var({var, _, Name}) when is_atom(Name) ->
  % Variable value.
  to_name(Name).

%% @private Pretty prints a literal node.
lit({boolean, _, Value}) when is_boolean(Value) ->
  % Boolean value.
  atom_to_list(Value);
lit({integer, _, Value}) when is_integer(Value) ->
  % Integer value.
  integer_to_list(Value);
lit({float, _, Value}) when is_float(Value) ->
  % Float value.
  float_to_list(Value);
lit({string, _, Value}) when is_list(Value) ->
  % String value.
  [$", Value, $"];
lit({atom, _, Value}) when is_atom(Value) ->
  % Atom value.
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Pretty prints an expression node.
expr(Var, Col) when ?IS_VAR(Var) ->
  % Variable expression.
  format_col("~s", [var(Var)], Col);
expr(Lit, Col) when ?IS_LIT(Lit) ->
  % Literal expressions.
  format_col("~s", [lit(Lit)], Col);
expr({tuple, _, ExprSeq}, Col) when is_list(ExprSeq) ->
  % Tuple expression.
  format_col("(~s)", [expr_seq(ExprSeq, 0)], Col);
expr({unit, _}, Col) ->
  % Unit expression.
  format_col("()", Col);
expr({msg, _, Tag, ExprSeq}, Col) when is_atom(Tag), is_list(ExprSeq) ->
  % Message expression.
  format_col("~s(~s)", [to_type_name(Tag), expr_seq(ExprSeq, 0)], Col);
expr({op, _, Op, ExprL, ExprR}, Col)
  when ?IS_OP(Op) ->
  % Binary operator expression.
  format_col("~s ~s ~s", [expr(ExprL, 0), Op, expr(ExprR, 0)], Col);
expr({op, _, Op, Expr}, Col)
  when ?IS_OP(Op) ->
  % Unary operator expression.
  format_col("~s~s", [Op, expr(Expr, 0)], Col);
expr({call, _, Name, ExprSeq}, Col) when is_atom(Name), is_list(ExprSeq) ->
  % Function call expression.
  format_col("~s(~s)", [Name, expr_seq(ExprSeq, 0)], Col);
expr({'if', _, ExprC, ExprT, ExprF}, Col)
  when ?IS_EXPR(ExprC), ?IS_EXPR(ExprT), ?IS_EXPR(ExprF) ->
  % If expression.
  [
    format_col("if (~s) {", [expr(ExprC, 0)], Col),
    ?SEP_EXPR,
    expr(ExprT, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("}", Col),
    ?SEP_EXPR,
    format_col("else {", Col),
    ?SEP_EXPR,
    expr(ExprF, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("}", Col)
  ];
expr({'let', _, Binders, Expr0, Expr1}, Col)
  when ?IS_EXPR(Expr0), ?IS_EXPR(Expr1) ->
  % Let expression.
  [
    format_col("let ~s =", [expr(Binders, 0)], Col),
    ?SEP_EXPR,
    expr(Expr0, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("in", Col),
    ?SEP_EXPR,
    expr(Expr1, Col + ?IND_STEP)
  ];
expr({new, _, MbType}, Col) when ?IS_MB_TYPE(MbType) ->
  % New expression.
  format_col("new [~s]", [type(MbType, 0)], Col);
expr({free, _, Var}, Col) when ?IS_VAR(Var) ->
  % Free expression.
  format_col("free(~s)", [var(Var)], Col);
expr({spawn, _, Expr}, Col) when ?IS_EXPR(Expr) ->
  % Spawn expression.
  [
    format_col("spawn {", Col),
    ?SEP_EXPR,
    expr(Expr, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("}", Col)
  ];
expr({guard, _, Var, Regex, Clauses}, Col) when ?IS_VAR(Var), is_list(Clauses) ->
  % Guard expression.
  [
    format_col("guard ~s: ~s {", [var(Var), Regex], Col),
    ?SEP_EXPR,
    case_clauses(Clauses, Col + ?IND_STEP),
    ?SEP_EXPR,
    format_col("}", Col)
  ];
expr({empty, _, RebindVar, Expr}, Col) when ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  % Empty expression.
  [
    format_col("empty(~s) ->", [var(RebindVar)], Col),
    ?SEP_EXPR,
    expr(Expr, Col + ?IND_STEP)
  ];
expr({'receive', _, MsgPat, RebindVar, Expr}, Col)
  when ?IS_MSG_PAT(MsgPat), ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  % Receive expression.
  [
    format_col("receive ~s from ~s ->", [pat(MsgPat), var(RebindVar)], Col),
    ?SEP_EXPR,
    expr(Expr, Col + ?IND_STEP)
  ].

%% @private Pretty prints an expression sequence.
expr_seq(ExprSeq, Col) when is_list(ExprSeq) ->
  % Expression sequence.
  lists:join(?SEP_EXPR_SEQ, [expr(Expr, Col) || Expr <- ExprSeq]).

%% @private Pretty prints a clause list.
case_clauses(Clauses, Col) when is_list(Clauses) ->
  % Clauses.
  lists:join(?SEP_CLAUSE, [expr(Clause, Col) || Clause <- Clauses]).


%%% ----------------------------------------------------------------------------
%%% Utility.
%%% ----------------------------------------------------------------------------

%% @private Returns the Pat type name from the specified atom.
to_type_name(Name) when is_atom(Name) ->
  string:titlecase(atom_to_list(Name)).

%% @private Returns the Pat variable or function identifier name from the
%% specified atom.
to_name(Name) when is_atom(Name) ->
  string:lowercase(atom_to_list(Name)).

%% @private Prints the data according to the specified format, offset at the
%% given column number.
format_col(Format, Data, Col)
  when is_list(Format), is_list(Data), is_integer(Col) ->
  [lists:duplicate(Col, ?SEP_COL), io_lib:format(Format, Data)].

%% @private Prints the specified format, offset at the given column number.
format_col(Format, Col) when is_list(Format), is_integer(Col) ->
  format_col(Format, [], Col).

%% @private Prints the data according to the specified format.
format(Format, Data) when is_list(Format), is_list(Data) ->
  io_lib:format(Format, Data).

%% @private Prints the specified format.
format(Format) when is_list(Format) ->
  format(Format, []).

