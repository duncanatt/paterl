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

%% API
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

%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

lit_type(Name) when Name =:= boolean ->
  ?T_BOOLEAN;
lit_type(Name) when Name =:= integer ->
  ?T_INTEGER;
lit_type(Name) when Name =:= float ->
  ?T_FLOAT;
lit_type(Name) when Name =:= string ->
  ?T_STRING;
lit_type(Name) when Name =:= atom ->
  ?T_ATOM;
lit_type(Name) when Name =:= unit ->
  ?T_UNIT.

mb_type(Name) when is_atom(Name) ->
  to_type_name(Name).

mb_type(Name, read) when is_atom(Name) ->
  to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_READ));
mb_type(Name, write) when is_atom(Name) ->
  to_type_name(list_to_atom(atom_to_list(Name) ++ ?C_WRITE)).

%% Examples.
%% pat_syntax:prod_type([pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, read)]).
prod_type(Types) when is_list(Types) ->
  io_lib:format("(~s)", [seq_prod(Types)]).

%% Examples.
union_type(Types) when is_list(Types) ->
  io_lib:format("~s", [seq_union(Types)]).

%% Examples.
%% pat_syntax:msg_type(tag, [pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, write)]).
msg_type(Tag, Types) when is_atom(Tag), is_list(Types) ->
  io_lib:format("~s(~s)", [to_type_name(Tag), seq_comma(Types)]).

%% Examples.
%% pat_syntax:iface_def(interface, [pat_syntax:msg_type(tag, [pat_syntax:lit_type(integer), pat_syntax:mb_type(interface, write)])]).
iface_def(Name) when is_atom(Name) ->
  io_lib:format("interface ~s { }~n", [to_type_name(Name)]).
iface_def(Name, Type) when is_atom(Name) ->
  io_lib:format("interface ~s {~n~s~n}~n", [to_type_name(Name), Type]).

%% Examples.
%% pat_syntax:param(pat_syntax:var(value), pat_syntax:lit_type(integer)).
param(Name, Type)  ->
  {Name, Type}.


%%% ----------------------------------------------------------------------------
%%% Functions.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:fun_def(myfun, [pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(x), pat_syntax:lit_type(integer))], pat_syntax:lit(5), pat_syntax:lit_type(integer))]).
fun_def(Name, Clauses) when is_atom(Name), is_list(Clauses), length(Clauses) =:= 1 ->
  io_lib:format("def ~s~s", [to_name(Name), seq_nl(Clauses)]).

%% Params: pairs of name and type.
%% Examples.
%% pat_syntax:fun_clause([], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
%% pat_syntax:fun_clause([pat_syntax:param(pat_syntax:var(x), pat_syntax:lit_type(integer))], pat_syntax:lit(5), pat_syntax:lit_type(integer)).
fun_clause(Params, Expr, RetType) when is_list(Params) ->
  io_lib:format("(~s): ~s {~n~s~n}~n", [params(Params), RetType, Expr]).


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:msg(tag, [pat_syntax:var(x)])
msg_pat(Tag, PatSeq) when is_atom(Tag), is_list(PatSeq) ->
  io_lib:format("~s(~s)", [to_type_name(Tag), seq_comma(PatSeq)]).


%%% ----------------------------------------------------------------------------
%%% Values.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:var(x).
var(Name) when is_atom(Name) ->
  to_name(Name).

%% Examples.
%% pat_syntax:lit(5).
lit(Value) when is_boolean(Value) ->
  atom_to_list(Value);
lit(Value) when is_integer(Value) ->
  integer_to_list(Value);
lit(Value) when is_float(Value) ->
  float_to_list(Value);
lit(Value) when is_list(Value) ->
  [$", Value, $"];
lit(Value) when is_atom(Value) ->
  atom_to_list(Value).

%% Examples.
%% pat_syntax:tuple_expr([]).
%% pat_syntax:tuple_expr([pat_syntax:to_var(x), pat_syntax:literal(5), pat_syntax:tuple_expr([])]).
tuple(Exprs) when is_list(Exprs) ->
  io_lib:format("(~s)", [seq_comma(Exprs)]).

unit() ->
  "()".

%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% Examples.
%% pat_syntax:msg(tag, [pat_syntax:var(x)])
msg_expr(Tag, ArgSeq) when is_atom(Tag), is_list(ArgSeq) ->
  io_lib:format("~s(~s)", [to_type_name(Tag), seq_comma(ArgSeq)]).

%% Examples.
%% pat_syntax:op_expr(pat_syntax:op('+'), pat_syntax:var(x), pat_syntax:literal(5)).
op_expr(Op, ExprL, ExprR) ->
  io_lib:format("~s ~s ~s", [ExprL, Op, ExprR]).

%% Examples.
%% pat_syntax:op_expr(pat_syntax:op('+'), pat_syntax:var(x)).
op_expr(Op, Expr) ->
  io_lib:format("~s~s", [Op, Expr]).

%% Examples.
%% pat_syntax:call_expr(myfun, []).
%% pat_syntax:call_expr(myfun, [pat_syntax:var(x), pat_syntax:lit(5)]).
call_expr(Name, Exprs) when is_list(Exprs) ->
  io_lib:format("~s(~s)", [Name, seq_comma(Exprs)]).

%% Examples.
%% pat_syntax:if_expr(pat_syntax:op_expr(pat_syntax:op('=='), pat_syntax:var(x), pat_syntax:literal(5)), pat_syntax:literal(0), pat_syntax:literal(20)).
if_expr(ExprC, ExprT, ExprF) ->
  io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [ExprC, ExprT, ExprF]).

%% Examples.
%% pat_syntax:let_expr(pat_syntax:var(x), pat_syntax:literal(1.2), pat_syntax:unit()).
let_expr(Binders, Expr0, Expr1) ->
  io_lib:format("let ~s =~n~s~nin~n~s", [Binders, Expr0, Expr1]).

%% Examples.
%% pat_syntax:new_expr(pat_syntax:mb_type(interface)).
new_expr(MbType) ->
  io_lib:format("new [~s]", [MbType]).

%% Examples.
%% pat_syntax:free_expr(pat_syntax:var(mb)).
free_expr(Var) ->
  io_lib:format("free(~s)", [Var]).

%% Examples.
%% pat_syntax:spawn_expr(pat_syntax:var(x)).
spawn_expr(Expr) ->
  io_lib:format("spawn {~n~s~n}", [Expr]).

%% Examples.
%% pat_syntax:guard_expr(pat_syntax:var(mb), "Tag*", [pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:var(x)), pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:unit())]).
guard_expr(Var, Regex, Clauses) when is_list(Clauses) ->
  io_lib:format("guard ~s: ~s {~n~s~n}", [Var, Regex, seq_nl(Clauses)]).

%% Examples.
%% pat_syntax:empty_expr(pat_syntax:var('mb0'), pat_syntax:var('mb0')).
empty_expr(RebindVar, Expr) ->
  io_lib:format("empty(~s) ->~n~s", [RebindVar, Expr]).

%% Examples.
%% pat_syntax:receive_expr(pat_syntax:msg(tag, [pat_syntax:var(x)]), pat_syntax:var(mb), pat_syntax:var(x)).
receive_expr(Msg, RebindVar, Expr) ->
  io_lib:format("receive ~s from ~s ->~n~s", [Msg, RebindVar, Expr]).



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
call_new_expr(MbType, FunName, Exprs) -> %TODO: Move to translation.
  MbVar0 = var('mb\''),
  MbVar1 = var(mb),
  Ret0 = var(x),

  Let0 = let_expr(var(y), free_expr(MbVar0), Ret0),
  Let1 = let_expr(tuple([Ret0, MbVar0]), call_use_expr(FunName, Exprs, MbVar1), Let0), % Or call_expr.
  let_expr(MbVar1 = var(mb), new_expr(MbType), Let1).

%% Examples.
%% pat_syntax:call_use_expr('myfun', [], "Mb0").
%% pat_syntax:call_use_expr('myfun', [pat_syntax:var(x), pat_syntax:lit(5)], "Mb0").
call_use_expr(FunName, Exprs, MbCtx) when is_list(Exprs) -> % TODO: Move to translation.
  call_expr(FunName, [MbCtx | Exprs]).

%% Examples.
%% pat_syntax:spawn_expr(pat_syntax:mb_type('Interface'), myfun, []).
%% pat_syntax:spawn_expr(pat_syntax:mb_type('Interface'), myfun, [pat_syntax:var(x), pat_syntax:lit(5)]).
spawn_expr(MbType, FunName, Exprs) -> % TODO: Move to translation.
  MbVar0 = var('mb\''),
  MbVar1 = var(mb),
  Ret0 = var(x),

  Let0 = let_expr(tuple([Ret0, MbVar0]), call_use_expr(FunName, Exprs, MbVar1), free_expr(MbVar0)),
  Let1 = let_expr(var(y), spawn_expr(Let0), MbVar1),
  let_expr(MbVar1, new_expr(MbType), Let1).

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

%% Examples.
self_expr(MbCtx) -> % TODO: Move to translation.
  io_lib:format("(~s, ~s)", [MbCtx, MbCtx]).

%%% ----------------------------------------------------------------------------
%%% Utility.
%%% ----------------------------------------------------------------------------

seq_comma(Args) when is_list(Args) ->
  string:join(Args, ?SEP_PAT).

seq_prod(Args) when is_list(Args) ->
  string:join(Args, ?SEP_PROD).

seq_union(Args) when is_list(Args) ->
  string:join(Args, ?SEP_UNION).

seq_nl(Clauses) when is_list(Clauses) ->
  string:join(Clauses, ?SEP_NL).

op(Op) when is_atom(Op) -> %TODO: Make precise with all operators.
  atom_to_list(Op).



params(Params) when is_list(Params) ->
  string:join([io_lib:format("~s: ~s", [Var, Type]) || {Var, Type} <- Params], ?SEP_PAT).

to_type_name(Name) when is_atom(Name) ->
  string:titlecase(atom_to_list(Name)).

to_name(Name) when is_atom(Name) ->
  string:lowercase(atom_to_list(Name)).




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
