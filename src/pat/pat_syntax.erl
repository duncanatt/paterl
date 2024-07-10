%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2024 18:08
%%%-------------------------------------------------------------------
-module(pat_syntax).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("pat.hrl").

%%% API.
-export([lit_type/1, mb_type/1, mb_type/2, product_type/1, union_type/1, msg_type/2]).
-export([interface_def/1, interface_def/2]).
-export([fun_def/2, fun_clause/3]).
%%-export([msg_pat/2, param/2, var_pat/1, var_pat/2, tuple_pat/1, tuple_pat/2]).
-export([msg_pat/2, param/2]).
-export([var/1, lit/1, tuple/1, unit/0]).
-export([msg_expr/2, op_expr/2, op_expr/3, call_expr/2, if_expr/3, let_expr/3]).
-export([
  new_expr/1, free_expr/1, spawn_expr/1, guard_expr/3, empty_expr/2,
  receive_expr/3
]).
-export([comment/1]).
%%-compile(export_all).


-type anno() :: any().

-type modality() :: read | write.

%%-type lit() :: boolean | integer | float | string | atom | unit.

-type tag() :: atom().

-type forms() :: [form()].

-type form() :: t_interface() | 'fun()' | comment().


%%% Type types.
-type types() :: [type()].

-type type() :: t_lit() | t_mb() | t_product() | t_union() | t_msg().

-type t_lit() :: {type, anno(), boolean | integer | float | string | atom | unit}.

-type t_mb() :: {type, anno(), atom()} | {type, anno(), atom(), modality()}.

-type t_product() :: {type, anno(), product, types()}.

-type t_union() :: {type, anno(), union, types()}.

-type t_msg() :: {type, anno(), msg, atom(), [t_lit()]}.

-type t_interface() :: {t_interface, anno(), atom(), t_union()}.

%%% Function definition types.

-type 'fun'() :: {'fun', anno(), atom(), [fun_clause()]}.

-type fun_clause() :: {fun_clause, anno(), [var_pat()], expr(), type()}.

-type comment() :: {comment, anno(), string()}.

%%% Pattern types.
-type pat() :: var_pat() | var_pat_type() | tuple_pat() | tuple_pat_type().

-type msg_pat() :: {pat, anno(), msg, tag(), [lit()]}.

-type var_pat() :: {pat, anno(), var()}.

-type var_pat_type() :: {pat, anno(), var(), t_lit() | t_mb()}.

-type tuple_pat() :: {pat, anno(), tuple, [expr()]}.

-type tuple_pat_type() :: {pat, anno(), tuple, [expr()], t_product()}.

%%% Value types.
-type val() :: var() | lit() | tuple() | unit().

-type var() :: {var, anno(), atom()}.

-type lit() :: {boolean, anno(), boolean()}
| {integer, anno(), integer()}
| {float, anno(), float()}
| {string, anno(), string()}
| {atom, anno(), atom()}.

-type tuple() :: {tuple, anno(), [expr()]}.

-type unit() :: {unit, anno()}.

%%% Expression types.



-type expr() :: msg() | op() | call() | 'if'() | 'let'() | new() | free()
| spawn() | guard().

-type msg() :: {msg, anno(), tag(), [val()]}.

-type op() :: {op, anno(), atom(), expr(), expr()} | {op, anno(), expr(), expr()}.

-type call() :: {call, anno(), atom(), [expr()]}.

-type 'if'() :: {'if', anno(), expr(), expr(), expr()}.

-type 'let'() :: {'let', anno(), pat(), expr(), expr()}.

-type new() :: {new, anno(), t_mb()}.

-type free() :: {free, anno(), var()}.

-type spawn() :: {spawn, anno(), expr()}.

-type guard() :: {guard, anno(), var(), string(), ['receive'()]}.

-type empty() :: {empty, anno(), var(), expr()}.

-type 'receive'() :: {'receive', anno(), msg_pat(), var(), expr()}.


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Default annotation value.
-define(DEF_ANNO_VAL, 0).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% @doc Returns a literal type node.
lit_type(Name) when Name =:= boolean ->
  % Boolean literal type.
  {type, ?DEF_ANNO_VAL, boolean};
lit_type(Name) when Name =:= integer ->
  % Integer literal type.
  {type, ?DEF_ANNO_VAL, integer};
lit_type(Name) when Name =:= float ->
  % Float literal type.
  {type, ?DEF_ANNO_VAL, float};
lit_type(Name) when Name =:= string ->
  % String literal type.
  {type, ?DEF_ANNO_VAL, string};
lit_type(Name) when Name =:= atom ->
  % Atom literal type.
  {type, ?DEF_ANNO_VAL, atom};
lit_type(Name) when Name =:= 'unit' ->
  % Unit literal type.
  {type, ?DEF_ANNO_VAL, unit}.

%% @doc Returns a modality-less mailbox type node with the specified name.
mb_type(Name) when is_atom(Name) ->
  % Mailbox type without modality.
  {type, ?DEF_ANNO_VAL, Name}.

%% @doc Returns a mailbox type node with the specified name and read or write
%% modality.
mb_type(Name, read) when is_atom(Name) ->
  % Mailbox type with read modality.
  {type, ?DEF_ANNO_VAL, to_type_name(Name), read};
mb_type(Name, write) when is_atom(Name) ->
  % Mailbox type with write modality.
  {type, ?DEF_ANNO_VAL, to_type_name(Name), write}.

%% Examples.
%% pat_syntax:prod_type([pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, read)]).
%% @doc Returns a product type node with the specified type elements.
product_type(Types) when is_list(Types) ->
  % Product type.
  {type, ?DEF_ANNO_VAL, product, Types}.

%% Examples.
%% @doc Returns a union type node with the specified type elements.
union_type(Types) when is_list(Types) ->
  % Union type.
  {type, ?DEF_ANNO_VAL, union, Types}.

%% Examples.
%% pat_syntax:msg_type(tag, [pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, write)]).
%% @doc Returns a message type node with the specified name and payload types.
msg_type(Tag, Types) when is_atom(Tag), is_list(Types) ->
  {type, ?DEF_ANNO_VAL, msg, to_type_name(Tag), Types}.

%% Examples.
%% pat_syntax:iface_def(interface, [pat_syntax:msg_type(tag, [pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, write)])]).
%% @doc Returns an empty interface definition node.
interface_def(Name) when is_atom(Name) ->
  {interface, ?DEF_ANNO_VAL, to_type_name(Name), []}.

%% @doc Returns an interface definition node containing the specified union of
%% message types.
interface_def(Name, Type) when is_atom(Name), ?IS_UNION_TYPE(Type) ->
%%interface_def(Name, Type) when is_atom(Name), ?IS_TYPE(Type) ->
  {interface, ?DEF_ANNO_VAL, to_type_name(Name), Type}.


%%% ----------------------------------------------------------------------------
%%% Functions and misc.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:fun_def(myfun, [pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(x), pat_syntax:lit_type(integer))], pat_syntax:lit(5), pat_syntax:lit_type(integer))]).
%% @doc Returns a function definition node.
fun_def(Name, Clauses) when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
  {'fun', ?DEF_ANNO_VAL, to_name(Name), Clauses}.

%% Params: pairs of name and type.
%% Examples.
%% pat_syntax:fun_clause([], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
%% pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(x), pat_syntax:lit_type(integer))], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
%% @doc Returns a function clause node with the specified parameters, body, and
%% return type.
fun_clause(PatSeq, Expr, RetType) when is_list(PatSeq) ->
  {fun_clause, ?DEF_ANNO_VAL, PatSeq, Expr, RetType}.

%% @doc Returns a comment node.
comment(Text) when is_list(Text) ->
  {comment, ?DEF_ANNO_VAL, Text}.

%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:msg(tag, [pat_syntax:var(x)])
%% @doc Returns a message pattern node with the specified tag and payload
%% patterns.
msg_pat(Tag, PatSeq) when is_atom(Tag), is_list(PatSeq) ->
  {pat, ?DEF_ANNO_VAL, msg, Tag, PatSeq}.

%% Examples.
%% pat_syntax:param(pat_syntax:var(value), pat_syntax:lit_type(integer)).
%% @doc Returns a function parameter node consisting of a variable name and its
%% type.
param(Var, Type) when ?IS_VAR(Var), ?IS_TYPE(Type) ->
  {pat, ?DEF_ANNO_VAL, Var, Type}.

var_pat(Var) when ?IS_VAR(Var) ->
  {pat, ?DEF_ANNO_VAL, Var}.

var_pat(Var, Type) when ?IS_VAR(Var), ?IS_TYPE(Type) ->
  {pat, ?DEF_ANNO_VAL, Var, Type}.

tuple_pat(PatSeq) when is_list(PatSeq) ->
  {pat, ?DEF_ANNO_VAL, tuple, PatSeq}.

tuple_pat(PatSeq, Type) when is_list(PatSeq), ?IS_TYPE(Type) ->
  {pat, ?DEF_ANNO_VAL, tuple, PatSeq, Type}.



%%% ----------------------------------------------------------------------------
%%% Values.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:var(x).
%% @doc Returns a variable node.
var(Name) when is_atom(Name) ->
  {var, ?DEF_ANNO_VAL, to_name(Name)}.

%% Examples.
%% pat_syntax:lit(5).
%% @doc Returns a literal node.
lit(Value) when is_boolean(Value) ->
  {boolean, ?DEF_ANNO_VAL, Value};
lit(Value) when is_integer(Value) ->
  {integer, ?DEF_ANNO_VAL, Value};
lit(Value) when is_float(Value) ->
  {float, ?DEF_ANNO_VAL, Value};
lit(Value) when is_list(Value) ->
  {string, ?DEF_ANNO_VAL, Value};
lit(Value) when is_atom(Value) ->
  {atom, ?DEF_ANNO_VAL, Value}.

%% Examples.
%% pat_syntax:tuple_expr([]).
%% pat_syntax:tuple_expr([pat_syntax:to_var(x), pat_syntax:literal(5), pat_syntax:tuple_expr([])]).
%% @doc Returns a tuple node with the specified expression elements.
tuple(Exprs) when is_list(Exprs) ->
  {tuple, ?DEF_ANNO_VAL, Exprs}.

%% @doc Returns a unit node.
unit() ->
  {unit, ?DEF_ANNO_VAL}.


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:msg(tag, [pat_syntax:var(x)])
%% @doc Returns a message expression node with the specified tag and payload
%% expressions.
msg_expr(Tag, ArgSeq) when is_atom(Tag), is_list(ArgSeq) ->
  {msg, ?DEF_ANNO_VAL, Tag, ArgSeq}.

%% Examples.
%% pat_syntax:op_expr(pat_syntax:op('+'), pat_syntax:var(x), pat_syntax:literal(5)).
%% @doc Returns a binary operator node with the specified operands.
op_expr(Op, ExprL, ExprR) when ?IS_OP(Op), ?IS_EXPR(ExprL), ?IS_EXPR(ExprR) ->
  {op, ?DEF_ANNO_VAL, Op, ExprL, ExprR}.

%% Examples.
%% pat_syntax:op_expr(pat_syntax:op('+'), pat_syntax:var(x)).
%% @doc Returns a unary operator node with the specified operand.
op_expr(Op, Expr) when ?IS_OP(Op), ?IS_EXPR(Expr) ->
  {op, ?DEF_ANNO_VAL, Op, Expr}.

%% Examples.
%% pat_syntax:call_expr(myfun, []).
%% pat_syntax:call_expr(myfun, [pat_syntax:var(x), pat_syntax:lit(5)]).
%% @doc Returns a function call node with the specified name and
%% arguments.
call_expr(Name, Exprs) when is_atom(Name), is_list(Exprs) ->
  {call, ?DEF_ANNO_VAL, Name, Exprs}.

%% Examples.
%% pat_syntax:if_expr(pat_syntax:op_expr(pat_syntax:op('=='), pat_syntax:var(x), pat_syntax:literal(5)), pat_syntax:literal(0), pat_syntax:literal(20)).
%% @doc Returns an if node with the specified condition, true and false
%% expression branches.
if_expr(ExprC, ExprT, ExprF) when ?IS_EXPR(ExprC), ?IS_EXPR(ExprT), ?IS_EXPR(ExprF) ->
  {'if', ?DEF_ANNO_VAL, ExprC, ExprT, ExprF}.

%% Examples.
%% pat_syntax:let_expr(pat_syntax:var(x), pat_syntax:literal(1.2), pat_syntax:unit()).
%% @doc Returns a let node with the specified binders, expression, and body.
let_expr(Pat, Expr0, Expr1) when ?IS_EXPR(Expr0), ?IS_EXPR(Expr1) ->
  {'let', ?DEF_ANNO_VAL, Pat, Expr0, Expr1}.

%% Examples.
%% pat_syntax:new_expr(pat_syntax:mb_type(interface)).
%% @doc Returns a new node with the specified mailbox type name.
new_expr(MbType) when ?IS_MB_TYPE(MbType) ->
  {new, ?DEF_ANNO_VAL, MbType}.

%% Examples.
%% pat_syntax:free_expr(pat_syntax:var(mb)).
%% @doc Returns a free node with the specified variable name.
free_expr(Var) when ?IS_VAR(Var) ->
  {free, ?DEF_ANNO_VAL, Var}.

%% Examples.
%% pat_syntax:spawn_expr(pat_syntax:var(x)).
%% @doc Returns a spawn node with the specified expression.
spawn_expr(Expr) when ?IS_EXPR(Expr) ->
  {spawn, ?DEF_ANNO_VAL, Expr}.

%% Examples.
%% pat_syntax:guard_expr(pat_syntax:var(mb), "Tag*", [pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:var(x)), pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:unit())]).
%% @doc Returns a guard node with the specified variable, regex, and clauses.
guard_expr(Var, Regex, Clauses) when ?IS_VAR(Var), is_list(Clauses) ->
  {guard, ?DEF_ANNO_VAL, Var, Regex, Clauses}.

%% Examples.
%% pat_syntax:empty_expr(pat_syntax:var('mb0'), pat_syntax:var('mb0')).
%% @doc Returns an empty node with the specified rebound variable name and body.
empty_expr(RebindVar, Expr) when ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  {empty, ?DEF_ANNO_VAL, RebindVar, Expr}.

%% Examples.
%% pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:var(x)).
%% @doc Returns a receive node with the specified message pattern, rebound
%% variable name, and body.
receive_expr(MsgPat, RebindVar, Expr) when ?IS_MSG_PAT(MsgPat), ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
  {'receive', ?DEF_ANNO_VAL, MsgPat, RebindVar, Expr}.


%%% ----------------------------------------------------------------------------
%%% Utility.
%%% ----------------------------------------------------------------------------

%% @private Returns the Pat type name from the specified atom.
to_type_name(Name) when is_atom(Name) ->
  list_to_atom(string:titlecase(atom_to_list(Name))).

%% @private Returns the Pat variable or function identifier name from the
%% specified atom.
to_name(Name) when is_atom(Name) ->
  list_to_atom(string:lowercase(atom_to_list(Name))).


%% TODO: These are used only to test the things in the pat.hrl file. Should be
%% TODO: moved to the end of the file or deleted.
type_test(Type) when ?IS_MB_MOD_TYPE(Type) ->
  is_type;
type_test(_) ->
  unexpected_type.

val_test(Term) when ?IS_VAL(Term) ->
  is_val;
val_test(_) ->
  is_not_val.


%% Interface example.
%% interface {
%%   Put(Int),
%%   Get(Int)
%% }
%%
%% PutType = pat_syntax:msg_type(put, [pat_syntax:lit_type(integer)]),
%% GetType = pat_syntax:msg_type(get, [pat_syntax:lit_type(integer)]),
%% IFace = pat_syntax:iface_def(interface, [PutType, GetType]).

%% Function example.
%% def myfun1(value: Int, mode: String): (Int * Interface?) { # Def
%%  let (a, b) = # LetIf
%%    let mb = # LetMb
%%      new [Interface]
%%    in
%%      let y = # LetSpawn
%%        spawn { # Spawn
%%          let (x, mb0) =
%%            myfun2()
%%          in
%%            free(mb0)
%%        }
%%    in
%%      mb
%%  in
%%    if (-1 == y) { # If
%%      (-1, "Bye")
%%    }
%%    else {
%%      guard mb: Put*.Get* { # Guard
%%        receive Put(x) from mb0 -> # ReceivePut
%%          myfun1(x, "Put")
%%        receive Get(x) from mb0 -> # ReceiveGet
%%          let i =
%%            x + 1
%%          in
%%            myfun1(i, "Get")
%%        empty(mb0) -> # Empty
%%          (value, mb0)
%%      }
%%    }
%%}
%%
%% ReceivePut = pat_syntax:receive_expr(pat_syntax:msg_pat(put, [pat_syntax:var(x)]), pat_syntax:var('mb0'), pat_syntax:call_expr(myfun1, [pat_syntax:var(x), pat_syntax:lit("Put")])),
%% ReceiveGet = pat_syntax:receive_expr(pat_syntax:msg_pat(get, [pat_syntax:var(x)]), pat_syntax:var('mb0'), pat_syntax:let_expr(pat_syntax:var(i), pat_syntax:op_expr('+', pat_syntax:var(x), pat_syntax:lit(1)), pat_syntax:call_expr(myfun1, [pat_syntax:var(i), pat_syntax:lit("Get")]))),
%% Empty = pat_syntax:empty_expr(pat_syntax:var('mb0'), pat_syntax:tuple([pat_syntax:var(value), pat_syntax:var('mb0')])),
%% Guard = pat_syntax:guard_expr(pat_syntax:var(mb), "Put*.Get*", [ReceivePut, ReceiveGet, Empty]),
%% If = pat_syntax:if_expr(pat_syntax:op_expr('==', pat_syntax:op_expr('-', pat_syntax:lit(1)), pat_syntax:var(y)), pat_syntax:tuple_expr([pat_syntax:op_expr('-', pat_syntax:lit(1)), pat_syntax:lit("Bye")]), Guard),
%% Spawn = pat_syntax:spawn_expr(pat_syntax:let_expr(pat_syntax:tuple_expr([pat_syntax:var(x), pat_syntax:var('mb0')]), pat_syntax:call_expr('myfun2', []), pat_syntax:free_expr('mb0'))),
%% LetSpawn = pat_syntax:let_expr(pat_syntax:var(y), Spawn, pat_syntax:var(mb)),
%% LetMb = pat_syntax:let_expr(pat_syntax:var(mb), pat_syntax:new_mb_expr(pat_syntax:mb_type(interface)), LetSpawn),
%% LetIf = pat_syntax:let_expr(pat_syntax:tuple_expr([pat_syntax:var(a), pat_syntax:var(b)]), LetMb, If),
%% Clause = pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(value), pat_syntax:lit_type(integer)), pat_syntax:param(pat_syntax:var(mode), pat_syntax:lit_type(string))], LetIf, pat_syntax:prod_type([pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, read)])).
%% Def = pat_syntax:fun_def(myfun1, [{pat_syntax:var(value), pat_syntax:lit_type(integer)}, {pat_syntax:var(mode), pat_syntax:lit_type(string)}], LetIf, pat_syntax:prod_type([pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, read)])).
