%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Aug 2024 18:37
%%%-------------------------------------------------------------------
-author("duncan").

%%% Guard macros.

%% Retrieves the syntactic category of the specified Erlang AST node.
-define(synCat(Expr), element(1, Expr)).

%% Retrieves the annotation of the specified Erlang AST node.
-define(anno(Expr), element(2, Expr)).

%% Retrieves the literal value of the specified Erlang AST literal expression.
-define(litValue(Expr), element(3, Expr)).

%% Determines whether the specified Erlang AST node represents an integer
%% expression.
-define(isInteger(Expr), ?synCat(Expr) =:= integer).

%% Determines whether the specified Erlang AST node represents a
%% float expression.
-define(isFloat(Expr), ?synCat(Expr) =:= float).

%% Determine whether the specified Erlang AST node represents a string
%% expression.
-define(isString(Expr), ?synCat(Expr) =:= string).

%% Determines whether the specified Erlang AST node represents an atom expression.
-define(isAtom(Expr), ?synCat(Expr) =:= atom).

%% Determines whether the specified Erlang AST node represents a literal
%% expression.
-define(isLit(Expr), ?isInteger(Expr)
  orelse ?isFloat(Expr)
  orelse ?isString(Expr)
  orelse ?isAtom(Expr)
).

%% Determines whether the specified Erlang AST node represents a variable
%% expression.
-define(isVar(Expr), ?synCat(Expr) =:= var).

%% Determines whether the specified Erlang AST node represents a tuple
%% expression.
-define(isTuple(Expr), ?synCat(Expr) =:= tuple).

%% Determines whether the specified Erlang AST node represents a value
%% expression.
-define(isVal(Expr), ?isLit(Expr) andalso ?isVar(Expr) andalso ?isTuple(Expr)).

%% Determines whether the specified Erlang AST node represents a message (tagged
%% tuple) expression.
-define(isMsg(Expr), ?isTuple(Expr)
  andalso length(element(3, Expr)) >= 1
  andalso ?isAtom(hd(element(3, Expr)))
).

%% Determines whether the specified Erlang AST node represents a receive
%% expression.
-define(isReceive(Expr), ?synCat(Expr) =:= 'receive').

%% Determines whether the specified Erlang AST node represents an operator
%% expression.
-define(isOp(Expr), ?synCat(Expr) =:= op).

%% Determines whether the specified Erlang AST node represents an if expression.
-define(isIf(Expr), ?synCat(Expr) =:= 'if').

%% Determines whether the specified Erlang AST node represents a case
%% expression.
-define(isCase(Expr), ?synCat(Expr) =:= 'case').

%% Determines whether the specified Erlang AST node represents a match
%% expression.
-define(isMatch(Expr), ?synCat(Expr) =:= match).

%% Determines whether the specified Erlang AST node represents a local function
%% call expression.
-define(isCall(Expr), ?synCat(Expr) =:= call).

%% Determines whether the specified Erlang AST node represents an implicit
%% function call (function name is an atom).
-define(isImplicitCall(Expr), ?isCall(Expr)
  andalso ?isAtom(element(3, Expr))
  andalso is_list(element(4, Expr))
).

%% Determines whether the specified Erlang AST node represents an explicit
%% function call (function name is an expression).
-define(isExplicitCall(Expr), ?isCall(Expr)
  andalso not(?isAtom(element(3, Expr)))
  andalso is_list(element(4, Expr))
).

%% Determines whether the specified Erlang AST node represents a mailbox
%% annotation expression.
-define(isMbAnno(Expr), ?isTuple(Expr)
  andalso length(element(3, Expr)) =:= 2
  andalso ?isAtom(hd(element(3, Expr)))
  andalso (?litValue(hd(element(3, Expr))) =:= new
    orelse ?litValue(hd(element(3, Expr))) =:= use
    orelse ?litValue(hd(element(3, Expr))) =:= state
  )
).

