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

%%% API
-export([]).
-compile(export_all).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(MB_IDX_START, 0).

-define(SEP_PAT, [$,, $\s]).

-define(SEP_PROD, [$\s, $*, $\s]).

-define(SEP_UNION, [$,, $\n]).

-define(SEP_NL, [$\n]).

-define(T_BOOLEAN, "Bool").

-define(T_INTEGER, "Int").

-define(T_FLOAT, "Float").

-define(T_STRING, "String").

-define(T_ATOM, "Atom").

-define(T_UNIT, "Unit").

-define(C_WRITE, "!").

-define(C_READ, "?").

-define(DEF_ANNO_VAL, 0).

%% Checks whether the term is a type.
%%-define(IS_TYPE(Term), element(1, Term) =:= 'type').


%%-define(IS_TYPE(Term),
%%  element(1, Type) =:= 'type', ?IS_LIT()
%%).

%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

type_test(Type) when ?IS_MB_MOD_TYPE(Type) ->
  is_type;
type_test(_) ->
  unexpected_type.

val_test(Term) when ?IS_VAL(Term) ->
  is_val;
val_test(_) ->
  is_not_val.


lit_type(Name) when Name =:= boolean ->
  % Boolean literal type.
  {'type', ?DEF_ANNO_VAL, 'boolean'};
lit_type(Name) when Name =:= integer ->
  % Integer literal type.
  {'type', ?DEF_ANNO_VAL, 'integer'};
lit_type(Name) when Name =:= float ->
  % Float literal type.
  {'type', ?DEF_ANNO_VAL, 'float'};
lit_type(Name) when Name =:= string ->
  % String literal type.
  {'type', ?DEF_ANNO_VAL, 'string'};
lit_type(Name) when Name =:= atom ->
  % Atom literal type.
  {'type', ?DEF_ANNO_VAL, 'atom'};
lit_type(Name) when Name =:= unit ->
  % Unit literal type.
  {'type', ?DEF_ANNO_VAL, 'unit'}.

mb_type(Name) when is_atom(Name) ->
%%  to_type_name(Name).
  % Mailbox type without modality.
  {'type', ?DEF_ANNO_VAL, Name}.

mb_type(Name, read) when is_atom(Name) ->
%%  to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_READ));
  % Mailbox type with read modality.
  {'type', ?DEF_ANNO_VAL, to_type_name(Name), 'read'};
mb_type(Name, write) when is_atom(Name) ->
%%  to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_WRITE)).
  % Mailbox type with write modality.
  {'type', ?DEF_ANNO_VAL, to_type_name(Name), 'write'}.

%% Examples.
%% pat_syntax:prod_type([pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, read)]).
product_type(Types) when is_list(Types) ->
%%  io_lib:format("(~s)", [seq_prod(Types)]).
  % Product type.
  {'type', ?DEF_ANNO_VAL, 'product', Types}.

%% Examples.
union_type(Types) when is_list(Types) ->
%%  io_lib:format("~s", [seq_union(Types)]).
  % Union type.
  {'type', ?DEF_ANNO_VAL, 'union', Types}.

%% Examples.
%% pat_syntax:msg_type(tag, [pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, write)]).
msg_type(Tag, Types) when is_atom(Tag), is_list(Types) ->
%%  io_lib:format("~s(~s)", [to_type_name(Tag), seq_comma(Types)]).
  {'type', ?DEF_ANNO_VAL, 'msg', to_type_name(Tag), Types}.


%% Examples.
%% pat_syntax:iface_def(interface, [pat_syntax:msg_type(tag, [pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, write)])]).
interface_def(Name) when is_atom(Name) ->
%%  io_lib:format("interface ~s { }~n", [to_type_name(Name)]).
  {'interface', ?DEF_ANNO_VAL, to_type_name(Name), []}.
interface_def(Name, Type) when is_atom(Name), ?IS_TYPE(Type) ->
%%  io_lib:format("interface ~s {~n~s~n}~n", [to_type_name(Name), Type]).
  {'interface', ?DEF_ANNO_VAL, to_type_name(Name), Type}.


%%% ----------------------------------------------------------------------------
%%% Functions.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:fun_def(myfun, [pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(x), pat_syntax:lit_type(integer))], pat_syntax:lit(5), pat_syntax:lit_type(integer))]).
fun_def(Name, Clauses) when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
%%  io_lib:format("def ~s~s", [to_name(Name), seq_nl(Clauses)]).
  {'fun', ?DEF_ANNO_VAL, to_name(Name), Clauses}.

%% Params: pairs of name and type.
%% Examples.
%% pat_syntax:fun_clause([], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
%% pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(x), pat_syntax:lit_type(integer))], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
fun_clause(Params, Expr, RetType) when is_list(Params) ->
%%  io_lib:format("(~s): ~s {~n~s~n}~n", [params(Params), RetType, Expr]).
  {'fun_clause', ?DEF_ANNO_VAL, Params, Expr, RetType}.


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:msg(tag, [pat_syntax:var(x)])
msg_pat(Tag, PatSeq) when is_atom(Tag), is_list(PatSeq) ->
%%  io_lib:format("~s(~s)", [to_type_name(Tag), seq_comma(PatSeq)]).
  {'pat', ?DEF_ANNO_VAL, 'msg', Tag, PatSeq}.

%% Examples.
%% pat_syntax:param(pat_syntax:var(value), pat_syntax:lit_type(integer)).
param(Var, Type) when ?IS_VAR(Var), ?IS_TYPE(Type) ->
%%  {Name, Type}.
  {'pat', ?DEF_ANNO_VAL, Var, Type}.


%%% ----------------------------------------------------------------------------
%%% Values.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:var(x).
var(Name) when is_atom(Name) ->
%%  to_name(Name).
  {'var', ?DEF_ANNO_VAL, to_name(Name)}.

%% Examples.
%% pat_syntax:lit(5).
lit(Value) when is_boolean(Value) ->
%%  atom_to_list(Value);
  {'boolean', ?DEF_ANNO_VAL, Value};
lit(Value) when is_integer(Value) ->
%%  integer_to_list(Value);
  {'integer', ?DEF_ANNO_VAL, Value};
lit(Value) when is_float(Value) ->
%%  float_to_list(Value);
  {'float', ?DEF_ANNO_VAL, Value};
lit(Value) when is_list(Value) ->
%%  [$", Value, $"];
  {'string', ?DEF_ANNO_VAL, Value};
lit(Value) when is_atom(Value) ->
%%  atom_to_list(Value).
  {'atom', ?DEF_ANNO_VAL, Value}.

%% Examples.
%% pat_syntax:tuple_expr([]).
%% pat_syntax:tuple_expr([pat_syntax:to_var(x), pat_syntax:literal(5), pat_syntax:tuple_expr([])]).
tuple(Exprs) when is_list(Exprs) ->
%%  io_lib:format("(~s)", [seq_comma(Exprs)]).
  {'tuple', ?DEF_ANNO_VAL, Exprs}.
unit() ->
%%  "()".
  {'unit', ?DEF_ANNO_VAL}.


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:msg(tag, [pat_syntax:var(x)])
msg_expr(Tag, ArgSeq) when is_atom(Tag), is_list(ArgSeq) ->
%%  io_lib:format("~s(~s)", [to_type_name(Tag), seq_comma(ArgSeq)]).
  {'msg', ?DEF_ANNO_VAL, Tag, ArgSeq}.

%% Examples.
%% pat_syntax:op_expr(pat_syntax:op('+'), pat_syntax:var(x), pat_syntax:literal(5)).
op_expr(Op, ExprL, ExprR) when ?IS_OP(Op), ?IS_EXPR(ExprL), ?IS_EXPR(ExprR) ->
%%  io_lib:format("~s ~s ~s", [ExprL, Op, ExprR]).
  {'op', ?DEF_ANNO_VAL, Op, ExprL, ExprR}.

%% Examples.
%% pat_syntax:op_expr(pat_syntax:op('+'), pat_syntax:var(x)).
op_expr(Op, Expr) when ?IS_OP(Op), ?IS_EXPR(Expr) ->
%%  io_lib:format("~s~s", [Op, Expr]).
  {'op', ?DEF_ANNO_VAL, Op, Expr}.

%% Examples.
%% pat_syntax:call_expr(myfun, []).
%% pat_syntax:call_expr(myfun, [pat_syntax:var(x), pat_syntax:lit(5)]).
call_expr(Name, Exprs) when is_atom(Name), is_list(Exprs) ->
%%  io_lib:format("~s(~s)", [Name, seq_comma(Exprs)]).
  {'call', ?DEF_ANNO_VAL, Name, Exprs}.

%% Examples.
%% pat_syntax:if_expr(pat_syntax:op_expr(pat_syntax:op('=='), pat_syntax:var(x), pat_syntax:literal(5)), pat_syntax:literal(0), pat_syntax:literal(20)).
if_expr(ExprC, ExprT, ExprF) when ?IS_EXPR(ExprC), ?IS_EXPR(ExprT), ?IS_EXPR(ExprF) ->
%%  io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [ExprC, ExprT, ExprF]).
  {'if', ?DEF_ANNO_VAL, ExprC, ExprT, ExprF}.

%% Examples.
%% pat_syntax:let_expr(pat_syntax:var(x), pat_syntax:literal(1.2), pat_syntax:unit()).
let_expr(Binders, Expr0, Expr1) when ?IS_EXPR(Expr0), ?IS_EXPR(Expr1) ->
%%  io_lib:format("let ~s =~n~s~nin~n~s", [Binders, Expr0, Expr1]).
  io:format("+++++++++++++++++++++++++++++++ Creating let expression with body ~p~n~n", [Expr1]),
  {'let', ?DEF_ANNO_VAL, Binders, Expr0, Expr1}.

%% Examples.
%% pat_syntax:new_expr(pat_syntax:mb_type(interface)).
new_expr(MbType) when ?IS_MB_TYPE(MbType) ->
%%  io_lib:format("new [~s]", [MbType]).
  {'new', ?DEF_ANNO_VAL, MbType}.

%% Examples.
%% pat_syntax:free_expr(pat_syntax:var(mb)).
free_expr(Var) when ?IS_VAR(Var) ->
%%  io_lib:format("free(~s)", [Var]).
  {'free', ?DEF_ANNO_VAL, Var}.

%% Examples.
%% pat_syntax:spawn_expr(pat_syntax:var(x)).
spawn_expr(Expr) when ?IS_EXPR(Expr) ->
%%  io_lib:format("spawn {~n~s~n}", [Expr]).
  {'spawn', ?DEF_ANNO_VAL, Expr}.

%% Examples.
%% pat_syntax:guard_expr(pat_syntax:var(mb), "Tag*", [pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:var(x)), pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:unit())]).
guard_expr(Var, Regex, Clauses) when ?IS_VAR(Var), is_list(Clauses) ->
%%  io_lib:format("guard ~s: ~s {~n~s~n}", [Var, Regex, seq_nl(Clauses)]).
  {'guard', ?DEF_ANNO_VAL, Var, Regex, Clauses}.

%% Examples.
%% pat_syntax:empty_expr(pat_syntax:var('mb0'), pat_syntax:var('mb0')).
empty_expr(RebindVar, Expr) when ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
%%  io_lib:format("empty(~s) ->~n~s", [RebindVar, Expr]).
  {'empty', ?DEF_ANNO_VAL, RebindVar, Expr}.

%% Examples.
%% pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:var(x)).
receive_expr(MsgPat, RebindVar, Expr) when ?IS_MSG_PAT(MsgPat), ?IS_VAR(RebindVar), ?IS_EXPR(Expr) ->
%%  io_lib:format("receive ~s from ~s ->~n~s", [Msg, RebindVar, Expr]).
  io:format("-------RECV EXPR"),
  io:format("-------Msg: ~p", [MsgPat]),
  io:format("-------RebindVar: ~p", [MsgPat]),
  io:format("-------Expr: ~p", [Expr]),
  {'receive', ?DEF_ANNO_VAL, MsgPat, RebindVar, Expr}.

%% Others.
comment(Text) when is_list(Text) ->
  {comment, ?DEF_ANNO_VAL, Text}.


%%% COMPOSED EXPRESSIONS TO MOVE TO TRANSLATION.


%% Examples.
%% pat_syntax:call_new_expr(pat_syntax:type('Interface'), pat_syntax:call_expr(pat_syntax:fun_name(myfun), [])).
%%call_new_expr_2(MbType, CallUseExpr) ->
%%  MbVar0 = var('mb\''),
%%  MbVar1 = var(mb),
%%  Ret0 = var(x),
%%
%%  Let0 = let_expr(var(y), free_expr(MbVar0), Ret0),
%%  Let1 = let_expr(tuple_expr([Ret0, MbVar0]), CallUseExpr, Let0),
%%  let_expr(MbVar1 = var(mb), new_mb(MbType), Let1).

%% Examples.
%% pat_syntax:call_new_expr(pat_syntax:mb_type('Interface'), myfun, []).
%% pat_syntax:call_new_expr(pat_syntax:mb_type('Interface'), myfun, [pat_syntax:var(x), pat_syntax:lit(5)]).
%%call_new_expr(MbType, FunName, Exprs) -> %TODO: Move to translation.
%%  MbVar0 = var('mb\''),
%%  MbVar1 = var(mb),
%%  Ret0 = var(x),
%%
%%  Let0 = let_expr(var(y), free_expr(MbVar0), Ret0),
%%  Let1 = let_expr(tuple([Ret0, MbVar0]), call_use_expr(FunName, Exprs, MbVar1), Let0), % Or call_expr.
%%  let_expr(MbVar1 = var(mb), new_expr(MbType), Let1).

%% Examples.
%% pat_syntax:call_use_expr('myfun', [], "Mb0").
%% pat_syntax:call_use_expr('myfun', [pat_syntax:var(x), pat_syntax:lit(5)], "Mb0").
%%call_use_expr(FunName, Exprs, MbCtx) when is_list(Exprs) -> % TODO: Move to translation.
%%  call_expr(FunName, [MbCtx | Exprs]).

%% Examples.
%% pat_syntax:spawn_expr(pat_syntax:mb_type('Interface'), myfun, []).
%% pat_syntax:spawn_expr(pat_syntax:mb_type('Interface'), myfun, [pat_syntax:var(x), pat_syntax:lit(5)]).
%%spawn_expr(MbType, FunName, Exprs) -> % TODO: Move to translation.
%%  MbVar0 = var('mb\''),
%%  MbVar1 = var(mb),
%%  Ret0 = var(x),
%%
%%  Let0 = let_expr(tuple([Ret0, MbVar0]), call_use_expr(FunName, Exprs, MbVar1), free_expr(MbVar0)),
%%  Let1 = let_expr(var(y), spawn_expr(Let0), MbVar1),
%%  let_expr(MbVar1, new_expr(MbType), Let1).

%% Examples.
%% pat_syntax:fun_def(myfun, [], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
%% pat_syntax:fun_def(myfun, [{pat_syntax:var(x), pat_syntax:lit_type(string)}], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
%%fun_def(FunName, Params, Expr, RetType) when is_list(Params) ->
%%  io_lib:format("def ~s(~s): ~s {~n~s~n}", [FunName, params(Params), RetType, Expr]).

%% Example: pat_syntax:fun_def(pat_syntax:mb_type('Interface', read), pat_syntax:fun_name(myfun), [pat_syntax:param(pat_syntax:var('X'), pat_syntax:lit_type(integer))], pat_syntax:var(x), pat_syntax:lit_type(string)).
%%fun_def(MbType, FunName, Params, Body, RetType) when is_list(Params) -> % TODO: Move to translation.
%%  Param = param(var('mb0'), MbType),
%%  RetType0 = prod_type([RetType, MbType]),
%%  fun_def(FunName, [Param | Params], Body, RetType0).

%%%% Examples.
%%self_expr(MbCtx) -> % TODO: Move to translation.
%%  io_lib:format("(~s, ~s)", [MbCtx, MbCtx]).

%%% ----------------------------------------------------------------------------
%%% Utility.
%%% ----------------------------------------------------------------------------

%%seq_comma(Args) when is_list(Args) ->
%%  string:join(Args, ?SEP_PAT).
%%
%%seq_prod(Args) when is_list(Args) ->
%%  string:join(Args, ?SEP_PROD).
%%
%%seq_union(Args) when is_list(Args) ->
%%  string:join(Args, ?SEP_UNION).
%%
%%seq_nl(Clauses) when is_list(Clauses) ->
%%  string:join(Clauses, ?SEP_NL).
%%
%%op(Op) when is_atom(Op) -> %TODO: Make precise with all operators.
%%  atom_to_list(Op).


%%params(Params) when is_list(Params) ->
%%  string:join([io_lib:format("~s: ~s", [Var, Type]) || {Var, Type} <- Params], ?SEP_PAT).

to_type_name(Name) when is_atom(Name) ->
  list_to_atom(string:titlecase(atom_to_list(Name))).

to_name(Name) when is_atom(Name) ->
  list_to_atom(string:lowercase(atom_to_list(Name))).


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
