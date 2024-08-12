%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Aug 2024 18:37
%%%-------------------------------------------------------------------
-author("duncan").


%%% ----------------------------------------------------------------------------
%%% General guard macros.
%%% ----------------------------------------------------------------------------

%% Retrieves the syntactic category of the specified Erlang abstract syntax
%% node.
-define(synCat(Term), element(1, Term)).

%% Retrieves the annotation of the specified Erlang abstract syntax node.
-define(anno(Term), element(2, Term)).

%% Retrieves the literal value of the specified Erlang abstract syntax node
%% literal term. May be undefined in cases.
-define(litValue(Term), element(3, Term)).


%%% ----------------------------------------------------------------------------
%%% Type guard macros.
%%% ----------------------------------------------------------------------------

%% Determines whether the specified Erlang abstract syntax node represents a
%% type annotation.
-define(isType(Type), ?synCat(Type) =:= type).

%% Determines whether the specified Erlang abstract syntax node represents a
%% literal type annotation.
-define(isLitType(Type), (?isBooleanType(Type)
    orelse ?isIntegerType(Type)
    orelse ?isFloatType(Type)
    orelse ?isStringType(Type)
    orelse ?isAtomType(Type)
)).
%%-define(isLitType(Type), (?isType(Type)
%%  andalso (?litValue(Type) =:= boolean
%%  orelse ?litValue(Type) =:= integer
%%  orelse ?litValue(Type) =:= float
%%  orelse ?litValue(Type) =:= string
%%  orelse ?litValue(Type) =:= atom
%%  ))).

%% Determines whether the specified Erlang abstract syntax node represents a
%% Boolean type annotation.
-define(isBooleanType(Type), (?isType(Type)
  andalso ?litValue(Type) =:= boolean)
).

%% Determines whether the specified Erlang abstract syntax node represents an
%% integer type annotation.
-define(isIntegerType(Type), (?isType(Type)
  andalso ?litValue(Type) =:= integer)
).

%% Determines whether the specified Erlang abstract syntax node represents a
%% float type annotation.
-define(isFloatType(Type), (?isType(Type) andalso ?litValue(Type) =:= float)).

%% Determines whether the specified Erlang abstract syntax node represents a
%% string type annotation.
-define(isStringType(Type), (?isType(Type) andalso ?litValue(Type) =:= string)).

%% Determines whether the specified Erlang abstract syntax node represents an
%% atom type annotation.
-define(isAtomType(Type), (?isType(Type) andalso ?litValue(Type) =:= atom)).


%%% ----------------------------------------------------------------------------
%%% Expression guard macros.
%%% ----------------------------------------------------------------------------

%% Determines whether the specified Erlang abstract syntax node represents a
%% Boolean expression. Incorrectly defined. If used, definition should be that
%% ?synCat(Expr) =:= atom andalso (value is true or false).
%%-define(isBoolean(Expr), ?synCat(Expr) =:= boolean).

%% Determines whether the specified Erlang abstract syntax node represents an
%% integer expression.
-define(isInteger(Expr), ?synCat(Expr) =:= integer).

%% Determines whether the specified Erlang abstract syntax node represents a
%% float expression.
-define(isFloat(Expr), ?synCat(Expr) =:= float).

%% Determine whether the specified Erlang abstract syntax node represents a
%% string expression.
-define(isString(Expr), ?synCat(Expr) =:= string).

%% Determines whether the specified Erlang abstract syntax node represents an
%% atom expression.
-define(isAtom(Expr), ?synCat(Expr) =:= atom).

%% Determines whether the specified Erlang abstract syntax node represents a
%% literal expression.
-define(isLit(Expr), (?isInteger(Expr)
  orelse ?isFloat(Expr)
  orelse ?isString(Expr)
  orelse ?isAtom(Expr))
).

%% Determines whether the specified Erlang abstract syntax node represents a
%% variable expression.
-define(isVar(Expr), ?synCat(Expr) =:= var).

%% Determines whether the specified Erlang abstract syntax node represents a
%% tuple expression.
-define(isTuple(Expr), ?synCat(Expr) =:= tuple).

%% Determines whether the specified Erlang abstract syntax node represents a
%% value expression.
-define(isVal(Expr), (?isLit(Expr)
  andalso ?isVar(Expr) andalso ?isTuple(Expr)
)).

%% Determines whether the specified Erlang abstract syntax node represents a
%% message (tagged tuple) expression.
-define(isMsg(Expr), (?isTuple(Expr)
  andalso length(element(3, Expr)) >= 1
  andalso ?isAtom(hd(element(3, Expr)))
)).

%% Determines whether the specified Erlang abstract syntax node represents a
%% receive expression.
-define(isReceive(Expr), ?synCat(Expr) =:= 'receive').

%% Determines whether the specified Erlang abstract syntax node represents an
%% operator expression.
-define(isOp(Expr), ?synCat(Expr) =:= op).

%% Determines whether the specified Erlang abstract syntax node represents an
%% if expression.
-define(isIf(Expr), ?synCat(Expr) =:= 'if').

%% Determines whether the specified Erlang abstract syntax node represents a
%% case expression.
-define(isCase(Expr), ?synCat(Expr) =:= 'case').

%% Determines whether the specified Erlang abstract syntax node represents a
%% match expression.
-define(isMatch(Expr), ?synCat(Expr) =:= match).

%% Determines whether the specified Erlang abstract syntax node represents a
%% local function call expression.
-define(isCall(Expr), ?synCat(Expr) =:= call).

%% Determines whether the specified Erlang abstract syntax node represents an
%% implicit function call (i.e., function name is an atom).
-define(isImplicitCall(Expr), ?isCall(Expr)
  andalso ?isAtom(element(3, Expr))
  andalso is_list(element(4, Expr))
).

%% Determines whether the specified Erlang abstract syntax node represents an
%% explicit function call (i.e., function name is an expression).
-define(isExplicitCall(Expr), (?isCall(Expr)
  andalso not(?isAtom(element(3, Expr)))
  andalso is_list(element(4, Expr))
)).

%% Determines whether the specified Erlang abstract syntax node represents a
%% mailbox annotation expression.
-define(isMbAnno(Expr), (?isTuple(Expr)
  andalso length(element(3, Expr)) =:= 2
%%  andalso ?isAtom(hd(element(3, Expr)))
  andalso (?litValue(hd(element(3, Expr))) =:= new
    orelse ?litValue(hd(element(3, Expr))) =:= use
    orelse ?litValue(hd(element(3, Expr))) =:= state
  )
)).


