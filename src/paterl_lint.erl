%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2024 11:10
%%%-------------------------------------------------------------------
-module(paterl_lint).
-moduledoc "Erlang syntactic subset linter.".
-author("duncan").

%%% Includes.
-include("log.hrl").
-include("paterl_lib.hrl").
-include("paterl_syntax.hrl").

%%% Public API.
-export([module/1, format_error/1]).
-compile(export_all).

%%% Error types.

%% Unsupported form.
-define(E_BAD__FORM, e_bad__form).

%% Unsupported literal.
-define(E_BAD__LIT, e_bad__lit).

%% Unsupported pattern.
-define(E_BAD__PAT, e_bad__pat).

%% Unsupported expression.
-define(E_BAD__EXPR, e_bad__expr).

%% Unsupported argument.
-define(E_BAD__ARG, e_bad__arg).

%% Unsupported message element.
-define(E_BAD__MSG_ELEM, e_bad__msg_elem).

%% Unsupported clause.
-define(E_BAD__CLAUSE, e_bad__clause).

%% Unsupported guard.
-define(E_BAD__GUARD, e_bad__guard).

%% Unsupported guard test.
-define(E_BAD__GUARD_TEST, e_bad__guard_test).

%% Unsupported type.
-define(E_BAD__TYPE, e_bad__type).


%%% ----------------------------------------------------------------------------
%%% Module declarations and forms.
%%% ----------------------------------------------------------------------------

%TODO: Add the erlang linter inside the module function.

%% @doc Module declaration.
module(Forms) when is_list(Forms) ->
  % Lint Erlang syntax.
  case erl_lint:module(Forms) of
    {ok, Warnings0} ->
      % Lint Erlang syntactic subset.
      Analysis0 = lint_forms(Forms, #analysis{file = paterl_syntax:get_file(Forms)}),

      % Lint mailbox interface definition wild attributes.
      Analysis1 = Analysis0,

      % Prepend Erlang linter warnings with rest of analysis.
      case paterl_lib:return(Analysis1) of
        {ok, Warnings1} ->
          % Successful analysis with possible warnings.
          {ok, Warnings0 ++ Warnings1};
        {error, Error1, Warnings1} ->
          % Unsuccessful analysis.
          {error, Error1, Warnings0 ++ Warnings1}
      end;
    Error = {error, _, _} ->
      Error
  end.


lint_forms(Forms, Analysis) when is_list(Forms) ->
  lists:foldl(fun lint_form/2, Analysis, Forms).

%% @private Forms.
lint_form(_Form = {attribute, _, export, FunRefs}, Analysis)
  when is_list(FunRefs) ->
  % Export attribute.
  ?TRACE("Lint '~s'.", [erl_prettypr:format(_Form)]),
  lint_fun_refs(FunRefs, Analysis);
lint_form(_Form = {attribute, _, import, {Mod, FunRefs}}, Analysis)
  when is_atom(Mod), is_list(FunRefs) ->
  % Import attribute.
  ?TRACE("Lint '~s'.", [erl_prettypr:format(_Form)]),
  lint_fun_refs(FunRefs, Analysis);
lint_form(_Form = {attribute, _, module, Mod}, Analysis) when is_atom(Mod) ->
  % Module attribute.
  ?TRACE("Lint '~s'.", [erl_prettypr:format(_Form)]),
  Analysis;
lint_form(_Form = {attribute, _, file, {_File, _Line}}, Analysis) ->
  % File attribute.
  ?TRACE("Lint '~s'.", [erl_prettypr:format(_Form)]),
  Analysis;
lint_form({function, Anno, Name, Arity, Clauses}, Analysis)
  when
  is_atom(Name),
  is_integer(Arity),
  is_list(Clauses), length(Clauses) =:= 1 ->
  % Function declaration with one clause.
  ?TRACE("Lint '~s'.", [
    erl_prettypr:format(paterl_syntax:fun_reference({Name, Arity}, Anno))
  ]),
  lint_clauses(Clauses, Analysis);
lint_form(_Form = {attribute, _, spec, {{Name, Arity}, TypeSeq}}, Analysis)
  when
  is_atom(Name),
  is_integer(Arity),
  is_list(TypeSeq), length(TypeSeq) =:= 1 ->
  % Function specification with local function and one clause.
  ?TRACE("Lint spec '~s'.", [erl_prettypr:format(_Form)]),
  lint_type_seq(TypeSeq, Analysis);
lint_form(_Form = {attribute, _, type, {_Name, Type, TypeVars}}, Analysis) ->
  % Type declaration.
  ?TRACE("Lint type +++ '~s'.", [erl_prettypr:format(_Form)]),
  ?TRACE("=== TYPE DECL = ~p", [_Form]),
%%  lint_type_seq(TypeVars, Analysis);
  lint_type(Type, Analysis);
lint_form(_Form = {attribute, _, _A, _T}, Analysis) ->
  % Wild attribute.
  ?TRACE("Lint '~s'.", [erl_prettypr:format(_Form)]),
  Analysis;
lint_form({error, _}, Analysis) ->
  % epp or erl_parse error.
  Analysis;
lint_form({warning, _}, Analysis) ->
  % epp or erl_parse warning.
  Analysis;
lint_form({eof, _}, Analysis) ->
  % EOF marker.
  Analysis;
lint_form(Form, Analysis) ->
  % Rest of forms are unsupported.
  ?ERROR("Unsupported form '~s'.", [erl_prettypr:format(Form)]),
  ?pushError(?E_BAD__FORM, Form, Analysis).

%% @private Function references.
%%lint_fun_refs([], Analysis) ->
%%  Analysis;
%%lint_fun_refs([FunRef | FunRefs], Analysis) ->
%%  Analysis0 = lint_fun_ref(FunRef, Analysis),
%%  lint_fun_refs(FunRefs, Analysis0).

lint_fun_refs(FunRefs, Analysis) when is_list(FunRefs) ->
  lists:foldl(fun lint_fun_ref/2, Analysis, FunRefs).

lint_fun_ref(_FunRef = {_Name, _Arity}, Analysis) ->
  Analysis.


%%% ----------------------------------------------------------------------------
%%% Atomic literals.
%%% ----------------------------------------------------------------------------

%% @private Atomic literals.
lint_lit({atom, _, Value}, Analysis) when is_atom(Value) ->
  % Atom.
  Analysis;
lint_lit({float, _, Value}, Analysis) when is_float(Value) ->
  % Float.
  Analysis;
lint_lit({integer, _, Value}, Analysis) when is_integer(Value) ->
  % Integer.
  Analysis;
lint_lit({string, _, Value}, Analysis) when is_list(Value) ->
  % String.
  Analysis;
lint_lit(Lit, Analysis) ->
  % Rest of literals are unsupported.
  ?ERROR("Unsupported literal '~s'.", [erl_prettypr:format(Lit)]),
  ?pushError(?E_BAD__LIT, Lit, Analysis).


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

%%lint_pat_seq([], Analysis) ->
%%  Analysis;
%%lint_pat_seq([Pat | PatSeq], Analysis) ->
%%  Analysis0 = lint_pat(Pat, Analysis),
%%  lint_pat_seq(PatSeq, Analysis0).
lint_pat_seq(PatSeq, Analysis) when is_list(PatSeq) ->
  lists:foldl(fun lint_pat/2, Analysis, PatSeq).


%%lint_pat(Pat, Analysis) when
%%  element(1, Pat) =:= atom;
%%  element(1, Pat) =:= char;
%%  element(1, Pat) =:= float;
%%  element(1, Pat) =:= integer;
%%  element(1, Pat) =:= string ->
%%  % Literal pattern.
%%  lint_lit(Pat, Analysis);
%%lint_pat({match, _, _, _}, Analysis) ->
%%  % Compound pattern.
%%  Analysis;
%%lint_pat({op, _, Op, Pat0, Pat1}, Analysis) ->
%%  % Binary operator pattern.
%%  Analysis0 = lint_pat(Pat0, Analysis),
%%  lint_pat(Pat1, Analysis0);
%%lint_pat({op, _, Op, Pat0}, Analysis) ->
%%  % Unary operator pattern.
%%  lint_pat(Pat0, Analysis);

%%lint_pat(_Pat = {tuple, _, PatSeq}, Analysis) when is_list(PatSeq) ->
%%  % Tuple pattern.
%%  ?TRACE("Lint pattern '~s'.", [erl_prettypr:format(_Pat)]),
%%  lint_msg_elems(PatSeq, Analysis);

lint_pat(_Expr = {tuple, _, MsgElemsPats = [{atom, _, Tag} | _]}, Analysis)
  when is_atom(Tag) ->
  % Message expression.
  ?TRACE("Lint message pattern '~s'.", [erl_prettypr:format(_Expr)]),
  lint_msg_elems_pats(MsgElemsPats, Analysis);
lint_pat(_Pat = {var, _, Name}, Analysis) when is_atom(Name) ->
  % Variable pattern.
  ?TRACE("Lint variable pattern '~s'.", [erl_prettypr:format(_Pat)]),
  Analysis;
lint_pat(Pat, Analysis) ->
  % Rest of patterns unsupported.
  ?ERROR("Unsupported pattern '~s'.", [erl_prettypr:format(Pat)]),
  ?pushError(?E_BAD__PAT, Pat, Analysis).


lint_msg_elems_pats([{atom, _, Tag} | ElemSeq], Analysis) when is_atom(Tag) ->
  % Message tuple expression.
  lint_msg_elem_pat_seq(ElemSeq, Analysis).

lint_msg_elem_pat_seq(ElemSeq, Analysis) when is_list(ElemSeq) ->
  lists:foldl(fun lint_msg_elem_pat/2, Analysis, ElemSeq).

%%lint_msg_elem_pat(Arg, Analysis) when
%%  element(1, Arg) =:= char;
%%  element(1, Arg) =:= float;
%%  element(1, Arg) =:= integer;
%%  element(1, Arg) =:= string ->
%%  % Literal expression.
%%  lint_lit(Arg, Analysis);
lint_msg_elem_pat(_Pat = {var, _, Name}, Analysis) when is_atom(Name) ->
  % Variable expression.
  ?TRACE("Lint variable pattern '~s'.", [erl_prettypr:format(_Pat)]),
  Analysis;
lint_msg_elem_pat(Elem, Analysis) ->
  % Other message elements unsupported.
  ?ERROR("Unsupported message element '~s'.", [erl_prettypr:format(Elem)]),
  ?pushError(?E_BAD__MSG_ELEM, Elem, Analysis).


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Body.
%%lint_expr_seq([], Analysis) ->
%%  Analysis;
%%lint_expr_seq([Expr | ExprSeq], Analysis) ->
%%  Analysis0 = lint_expr(Expr, Analysis),
%%  lint_expr_seq(ExprSeq, Analysis0).

lint_expr_seq(ExprSeq, Analysis) when is_list(ExprSeq) ->
  lists:foldl(fun lint_expr/2, Analysis, ExprSeq).


%% @private Expressions.
lint_expr(Expr, Analysis) when
  element(1, Expr) =:= atom;
  element(1, Expr) =:= char;
  element(1, Expr) =:= float;
  element(1, Expr) =:= integer;
  element(1, Expr) =:= string ->
  % Literal expression.
  lint_lit(Expr, Analysis);
lint_expr(_Expr = {call, _, {atom, _, self}, []}, Analysis) ->
  % Call to self expression.
  ?TRACE("Lint self expression '~s'.", [erl_prettypr:format(_Expr)]),
  Analysis;
lint_expr(_Expr = {call, _, _Operator = {atom, _, spawn}, Args}, Analysis)
  when is_list(Args), length(Args) =:= 3 ->
  % Call to spawn expression.
  ?TRACE("Lint spawn expression '~s'.", [erl_prettypr:format(_Expr)]),
  lint_args(Args, Analysis);
lint_expr(_Expr = {call, _, _Operator = {atom, _, Name}, Args}, Analysis)
  when
  is_atom(Name), is_list(Args) ->
  % Internal function call expression.
  ?TRACE("Lint call expression '~s'.", [erl_prettypr:format(_Expr)]),
  lint_args(Args, Analysis);
lint_expr({'if', _, Clauses}, Analysis) when is_list(Clauses) ->
  % If expression.
  ?TRACE("Lint if expression '~s'.", [erl_prettypr:format(erl_syntax:if_expr([]))]),
  lint_clauses(Clauses, Analysis);
lint_expr(_Expr = {match, _, Pat, Expr}, Analysis) ->
  % Match expression.
  ?TRACE("Lint match expression '~s'.", [erl_prettypr:format(_Expr)]),
  Analysis0 = lint_pat(Pat, Analysis),
  lint_expr(Expr, Analysis0);
lint_expr(_Expr = {op, _, Op, Expr0, Expr1}, Analysis) ->
  % Binary operator expression.
  ?TRACE("Lint binary expression '~s'.", [erl_prettypr:format(_Expr)]),
  Analysis0 = lint_expr(Expr0, Analysis),
  lint_expr(Expr1, Analysis0);
lint_expr(_Expr = {op, _, Op, Expr}, Analysis) ->
  % Unary operator expression.
  ?TRACE("Lint unary expression '~s'.", [erl_prettypr:format(_Expr)]),
  lint_expr(Expr, Analysis);
lint_expr({'receive', _, Clauses}, Analysis) when is_list(Clauses) ->
  % Receive expression.
  ?TRACE("Lint receive expression '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),
  lint_clauses(Clauses, Analysis);
lint_expr({'receive', Anno, Clauses, Expr, Body}, Analysis) when is_list(Clauses) ->
  % Receive after expression.
  ?TRACE("Lint receive expression '~s'.", [erl_prettypr:format(erl_syntax:receive_expr([]))]),
  Analysis0 = lint_clauses(Clauses, Analysis),
  Analysis1 = lint_expr(Expr, Analysis0),
  lint_expr_seq(Body, Analysis1);
lint_expr(_Expr = {tuple, _Anno, MbAnnoParams = [{atom, _, MbAnno} | _]}, Analysis)
  when MbAnno =:= ?ANNO_EXPECTS; MbAnno =:= ?ANNO_AS ->
  % Mailbox annotation expression.
  ?TRACE("Lint mailbox annotation '~s'.", [
    erl_prettypr:format(
      paterl_syntax:mb_anno(
        paterl_syntax:mb_anno_name(_Expr),
        paterl_syntax:mb_anno_args(_Expr),
        _Anno
      ))
  ]),
  lint_mb_anno_params(MbAnnoParams, Analysis);
lint_expr(_Expr = {tuple, _, MsgElems = [{atom, _, Tag} | _]}, Analysis)
  when is_atom(Tag) ->
  % Message expression.
  ?TRACE("Lint message expression '~s'.", [erl_prettypr:format(_Expr)]),
  lint_msg_elems(MsgElems, Analysis);
lint_expr(_Expr = {var, _, Name}, Analysis) when is_atom(Name) ->
  % Variable expression.
  ?TRACE("Lint variable expression '~s'.", [erl_prettypr:format(_Expr)]),
  Analysis;
lint_expr(Expr, Analysis) ->
  % Rest of expressions unsupported.
  ?ERROR("Unsupported expression '~s'.", [erl_prettypr:format(Expr)]),
  ?pushError(?E_BAD__EXPR, Expr, Analysis).


%%lint_args([], Analysis) ->
%%  Analysis;
%%lint_args([Arg | Args], Analysis) ->
%%  Analysis0 = lint_arg(Arg, Analysis),
%%  lint_args(Args, Analysis0).

lint_args(Args, Analysis) when is_list(Args) ->
  lists:foldl(fun lint_arg/2, Analysis, Args).


lint_arg(Arg, Analysis) when
  element(1, Arg) =:= atom;
  element(1, Arg) =:= char;
  element(1, Arg) =:= float;
  element(1, Arg) =:= integer;
  element(1, Arg) =:= string ->
  % Literal expression.
  lint_lit(Arg, Analysis);
lint_arg({cons, _, ExprH, ExprT}, Analysis) ->
  % Cons skeleton expression.
  Analysis0 = lint_arg(ExprH, Analysis),
  lint_arg(ExprT, Analysis0);
lint_arg({nil, _}, Analysis) ->
  % Nil expression.
  Analysis;
% TODO: Comment out to match the syntax on paper.
lint_arg({op, _, Op, Expr0, Expr1}, Analysis) ->
  % Binary operator expression.
  Analysis0 = lint_arg(Expr0, Analysis),
  lint_arg(Expr1, Analysis0);
lint_arg({op, _, Op, Expr}, Analysis) ->
  % Unary operator expression.
  lint_arg(Expr, Analysis);
%TODO: End comment out
lint_arg({var, _, Name}, Analysis) when is_atom(Name) ->
  % Variable expression.
  Analysis;
lint_arg(Arg, Analysis) ->
  % Other arguments unsupported.
  ?ERROR("Unsupported argument '~s'.", [erl_prettypr:format(Arg)]),
  ?pushError(?E_BAD__ARG, Arg, Analysis).

lint_mb_anno_params([{atom, _, _MbAnno = ?ANNO_EXPECTS}, {atom, _, MbName}, {string, _, Pattern}], Analysis)
  when is_atom(MbName), is_list(Pattern) ->
  Analysis;
lint_mb_anno_params([{atom, _, ?ANNO_EXPECTS}, {string, _, Pattern}], Analysis)
  when is_list(Pattern) ->
  Analysis;
lint_mb_anno_params([{atom, _, ?ANNO_AS}, {atom, _, MbName}], Analysis)
  when is_atom(MbName) ->
  Analysis.


lint_msg_elems([{atom, _, Tag} | ElemSeq], Analysis) when is_atom(Tag) ->
  % Message tuple expression.
  lint_msg_elem_seq(ElemSeq, Analysis).

lint_msg_elem_seq(ElemSeq, Analysis) when is_list(ElemSeq) ->
  lists:foldl(fun lint_msg_elem/2, Analysis, ElemSeq).

lint_msg_elem(Arg, Analysis) when
  element(1, Arg) =:= char;
  element(1, Arg) =:= float;
  element(1, Arg) =:= integer;
  element(1, Arg) =:= string ->
  % Literal expression.
  lint_lit(Arg, Analysis);
lint_msg_elem({op, _, Op, Expr0, Expr1}, Analysis) ->
  % Binary operator expression.
  Analysis0 = lint_arg(Expr0, Analysis),
  lint_msg_elem(Expr1, Analysis0);
lint_msg_elem({op, _, Op, Expr}, Analysis) ->
  % Unary operator expression.
  lint_msg_elem(Expr, Analysis);
lint_msg_elem({var, _, Name}, Analysis) when is_atom(Name) ->
  % Variable expression.
  Analysis;
lint_msg_elem(Elem, Analysis) ->
  % Other message elements unsupported.
  ?ERROR("Unsupported message element '~s'.", [erl_prettypr:format(Elem)]),
  ?pushError(?E_BAD__MSG_ELEM, Elem, Analysis).


%%% ----------------------------------------------------------------------------
%%% Clauses.
%%% ----------------------------------------------------------------------------

%% @private Clauses.
%%lint_clauses([], Analysis) ->
%%  Analysis;
%%lint_clauses([Clause | Clauses], Analysis) ->
%%  Analysis0 = lint_clause(Clause, Analysis),
%%  lint_clauses(Clauses, Analysis0).

lint_clauses(Clauses, Analysis) when is_list(Clauses) ->
  lists:foldl(fun lint_clause/2, Analysis, Clauses).

%% @private Function, if clauses, case, and catch clauses.
%%{clause,ANNO,[Rep(P)],[],Rep(B)}
%%lint_clause(_Clause = {clause, _, [Pat], [], Body}, Analysis) when is_list(Body) ->
%%  % Case clause and function clause.
%%  ?TRACE("Lint clause '~s'.", [erl_prettypr:format(_Clause)]),
%%  Analysis0 = lint_pat(Pat, Analysis),
%%  lint_expr_seq(Body, Analysis0);
%%% {clause,ANNO,Rep(Ps),[],Rep(B)}
lint_clause(_Clause = {clause, _, PatSeq, [], Body}, Analysis)
  when
  is_list(PatSeq), is_list(Body) ->
  % Case clause and function clause.
  ?TRACE("Lint clause '~s'.", [erl_prettypr:format(_Clause)]),
  Analysis0 = lint_pat_seq(PatSeq, Analysis),
  lint_expr_seq(Body, Analysis0);
lint_clause(_Clause = {clause, _, [], GuardSeq = [_], Body}, Analysis)
  when
  is_list(Body) ->
  % If clause with exactly two clauses.
  ?TRACE("Lint if clause '~s'.", [erl_prettypr:format(_Clause)]),
  Analysis0 = lint_guard_seq(GuardSeq, Analysis),
  lint_expr_seq(Body, Analysis0);
lint_clause(Clause, Analysis) ->
  % Rest of clauses unsupported.
  ?ERROR("Unsupported clause '~s'.", [erl_prettypr:format(Clause)]),
  ?ERROR("Unsupported clause '~p'.", [Clause]),
  ?pushError(?E_BAD__CLAUSE, Clause, Analysis).


%%% ----------------------------------------------------------------------------
%%% Guards.
%%% ----------------------------------------------------------------------------

%% @private Guard sequence.
%%lint_guard_seq([], Analysis) ->
%%  Analysis;
%%lint_guard_seq([Guard | GuardSeq], Analysis) ->
%%  Analysis0 = lint_guard(Guard, Analysis),
%%  lint_guard_seq(GuardSeq, Analysis0).

lint_guard_seq(GuardSeq, Analysis) when is_list(GuardSeq) ->
  lists:foldl(fun lint_guard/2, Analysis, GuardSeq).

%% @private Guard.
lint_guard([], Analysis) ->
  Analysis;
lint_guard([GuardTest | GuardTests], Analysis) ->
  Analysis0 = lint_guard_test(GuardTest, Analysis),
  lint_guard(GuardTests, Analysis0).

%% @private Guard tests.
lint_guard_test(Test, Analysis)
  when
  element(1, Test) =:= atom, element(3, Test) =:= true;
  element(1, Test) =:= char;
  element(1, Test) =:= float;
  element(1, Test) =:= integer;
  element(1, Test) =:= string ->
  % Literal guard test.
  ?TRACE("Lint PRIM guard test '~s'.", [erl_prettypr:format(Test)]),
  ?TRACE("Lint PRIM guard test '~p'.", [Test]),
  lint_lit(Test, Analysis);
%%lint_guard_test({call, _, Name, Guard}, Analysis) when is_list(Guard) ->
%%  % Internal function call guard test.
%%  Analysis0 = lint_lit(Name, Analysis),
%%  lint_guard(Guard, Analysis0);
lint_guard_test(_GuardTest = {op, _, Op, GuardTest0, GuardTest1}, Analysis) ->
  % Binary operator guard test.
  ?TRACE("Lint guard test '~s'.", [erl_prettypr:format(_GuardTest)]),
  Analysis0 = lint_guard_test(GuardTest0, Analysis),
  lint_guard_test(GuardTest1, Analysis0);
%%lint_guard_test({op, _, Op, GuardTest}, Analysis) ->
%%  % Unary operator guard test.
%%  lint_guard_test(GuardTest, Analysis);
%%lint_guard_test({tuple, _, Guard}, Analysis) when is_list(Guard) ->
%%  % Tuple skeleton guard test.
%%  lint_guard(Guard, Analysis);
lint_guard_test({var, _, Name}, Analysis) when is_atom(Name) ->
  % Variable guard test.
  Analysis;
lint_guard_test(GuardTest, Analysis) ->
  % Other guard tests unsupported.
  ?ERROR("Unsupproted guard test '~s'.", [erl_prettypr:format(GuardTest)]),
  ?pushError(?E_BAD__GUARD_TEST, GuardTest, Analysis).


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

%% TODO: Check for no type variables allowed.

%%lint_type_seq([], Analysis) ->
%%  Analysis;
%%lint_type_seq([Type | TypeSeq], Analysis) ->
%%  Analysis0 = lint_type(Type, Analysis),
%%  lint_type_seq(TypeSeq, Analysis0).

lint_type_seq(TypeSeq, Analysis) when is_list(TypeSeq) ->
  ?TRACE("Lint type seq!! (~p)", [length(TypeSeq)]),
  lists:foldl(fun lint_type/2, Analysis, TypeSeq).

lint_type(Type, Analysis)
  when
  element(1, Type) =:= atom ->
%%  element(1, Type) =:= char;
%%  element(1, Type) =:= float;
%%  element(1, Type) =:= integer;
%%  element(1, Type) =:= string ->
  % Singleton (actual) type.
  lint_lit(Type, Analysis);
lint_type(_Type = {type, _, Name, []}, Analysis)
  when
  Name =:= ok;
  Name =:= pid;
  Name =:= any;
  Name =:= no_return;
  Name =:= boolean;
  Name =:= float;
  Name =:= integer;
  Name =:= string ->
  % Literal and built-in types.
  ?TRACE("Lint builtin type = ~p", [_Type]),
%%  lint_type_seq(TypeVar, Analysis);
  Analysis;

%%                 {type, Anno0, 'fun', [{type, Anno1, product, TypeSeq}, Type]}
lint_type(_Type = {type, _, 'fun', [{type, _, product, TypeSeq}, RetType]}, Analysis) ->
  % Fun type.
  ?TRACE("Lint type '~s'.", [erl_prettypr:format(_Type)]),
  Analysis0 = lint_type_seq(TypeSeq, Analysis),
  lint_type(RetType, Analysis0);

lint_type(_Type = {type, _, tuple, MsgElemsTypes = [{atom, _, Tag} | _]}, Analysis)
  when is_atom(Tag) ->
  % Message expression.
  ?TRACE("Lint message type '~s'.", [erl_prettypr:format(_Type)]),
  lint_msg_elems_types(MsgElemsTypes, Analysis);

%%lint_type(_Type = {type, _, tuple, TypeSeq}, Analysis) when is_list(TypeSeq) ->
%%  % Tuple type.
%%  % TODO: Check that it is a message type.
%%  ?TRACE("-----> ~p", [_Type]),
%%  lint_type_seq(TypeSeq, Analysis);
%%  Analysis;

%%lint_expr(_Expr = {tuple, _, MsgElems = [{atom, _, Tag} | _]}, Analysis)
%%  when is_atom(Tag) ->
%%  % Message expression.
%%  ?TRACE("Lint message '~s'.", [erl_prettypr:format(_Expr)]),
%%  lint_msg_elems(MsgElems, Analysis);

lint_type({type, _, union, TypeSeq}, Analysis) when is_list(TypeSeq) ->
  % Type union.
  ?TRACE("Lint type union"),
  lint_type_seq(TypeSeq, Analysis);
lint_type({user_type, _, Name, TypeSeq}, Analysis) when is_atom(Name), is_list(TypeSeq) ->
  % User-defined type.
  ?TRACE("Lint user type"),
  lint_type_seq(TypeSeq, Analysis);
lint_type(Type, Analysis) ->
  % Other types unsupported.
  ?ERROR("Unsupported type '~s'.", [erl_prettypr:format(Type)]),
  ?pushError(?E_BAD__TYPE, Type, Analysis).

lint_msg_elems_types([{atom, _, Tag} | ElemSeq], Analysis) when is_atom(Tag) ->
  % Message tuple expression.
  lint_msg_elem_type_seq(ElemSeq, Analysis).

lint_msg_elem_type_seq(ElemSeq, Analysis) when is_list(ElemSeq) ->
  lists:foldl(fun lint_msg_elem_type/2, Analysis, ElemSeq).

%%lint_msg_elem_type(Arg, Analysis) when
%%  element(1, Arg) =:= char;
%%  element(1, Arg) =:= float;
%%  element(1, Arg) =:= integer;
%%  element(1, Arg) =:= string ->
%%  % Literal expression.
%%  lint_lit(Arg, Analysis);

lint_msg_elem_type(_Type = {type, _, Name, []}, Analysis)
  when
  Name =:= float;
  Name =:= integer;
  Name =:= string ->
  % Literal and built-in types.
  ?TRACE("Lint builtin type = ~p", [_Type]),
  Analysis;
lint_msg_elem_type(_Type = {user_type, _, Name, []}, Analysis) when is_atom(Name) ->
  % User-defined type.
  ?TRACE("Lint user type '~s'.", [erl_prettypr:format(_Type)]),
  Analysis;
lint_msg_elem_type(Elem, Analysis) ->
  % Other message elements unsupported.
  ?ERROR("Unsupported message element '~s'.", [erl_prettypr:format(Elem)]),
  ?pushError(?E_BAD__MSG_ELEM, Elem, Analysis).


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

-doc """
Formats `Detail` to human-readable form.

See `module/1` for error and warnings codes.

### Returns
- message as a possibly deep [`io_lib:chars()`](`t:io_lib:chars/0`) list
""".
format_error({?E_BAD__FORM, Node}) ->
  io_lib:format(
    "unsupported form '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__LIT, Node}) ->
  io_lib:format(
    "unsupported literal '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__PAT, Node}) ->
  io_lib:format(
    "unsupported pattern '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__EXPR, Node}) ->
  io_lib:format(
    "unsupported expression '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__ARG, Node}) ->
  io_lib:format(
    "unsupported argument '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MSG_ELEM, Node}) ->
  io_lib:format(
    "unsupported message element '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__CLAUSE, Node}) ->
  io_lib:format(
    "unsupported clause '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__GUARD, Node}) ->
  io_lib:format(
    "unsupported guard '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__GUARD_TEST, Node}) ->
  io_lib:format(
    "unsupported guard test '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__TYPE, Node}) ->
  io_lib:format(
    "unsupported type '~s'",
    [erl_prettypr:format(Node)]
  ).


format(Node) ->
  Anno = erl_syntax:get_pos(Node),
  io_lib:format("~p: ~s", [Anno, erl_prettypr:format(Node)]).