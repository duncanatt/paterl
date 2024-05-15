%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans_2).
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

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(MB_IDX_START, 0).

-define(SEP_PAT, [$,, $\s]).

-define(SEP_NL, [$\n]).

-define(T_INTEGER, "Int").

-define(T_FLOAT, "Float").

-define(T_STRING, "String").

-define(T_UNIT, "Unit").

-define(C_WRITE, "!").

-define(C_READ, "?").

-spec translate(erl_syntax:forms()) -> list().
translate(Forms) ->

  Type = erl_syntax:revert(
    erl_syntax:type_application(erl_syntax:atom(any), [])
  ),

  Call =
    erl_syntax:set_pos(
      erl_syntax:application(erl_syntax:atom(main), []),
      paterl_anno:set_modality(new, paterl_anno:set_interface(main_mb, erl_anno:new(0)))
    ),

  Clause =
    erl_syntax:revert(
      erl_syntax:set_pos(
        erl_syntax:clause([], [Call]),
        paterl_anno:set_type(Type, erl_anno:new(0))

      )),

  Fun = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:function(erl_syntax:atom('main\''), [Clause]),
      paterl_anno:set_type(Type, erl_anno:new(0))
    )),

  translate_forms(Forms ++ [Fun]).


translate_forms(Forms) ->
  [translate_form(Form) || Form <- Forms].

%%% ----------------------------------------------------------------------------
%%% Translation on forms and types.
%%% ----------------------------------------------------------------------------

translate_form({attribute, _, module, Name}) ->
  io_lib:format("# Translated from ~s.erl~n", [Name]);
translate_form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  Type0 = translate_type(Type),
  io_lib:format("interface ~s { ~s }~n~n", [make_type_name(Name), Type0]);

translate_form({function, _, Name, Arity, Clauses = [_]}) ->
  % Function with one clause.
  ?TRACE("Translating function ~s/~b.", [Name, Arity]),
  Clauses0 = translate_fun_clauses(Clauses),
  io_lib:format("def ~s ~s~n", [make_fun_name(Name), Clauses0]);
translate_form(_) ->
  % Other Erlang module attributes without Pat equivalent.
  "".

translate_type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. This handles the case where the a mailbox
  % interface type is just a PID.
  "";
translate_type({type, _, integer, _Vars = []}) ->
  % Integer type.
  ?T_INTEGER;
translate_type({type, _, float, _Vars = []}) ->
  % Float type.
  ?T_FLOAT;
translate_type({type, _, string, _Vars = []}) ->
  % String type.
  ?T_STRING;
translate_type({type, _, Name, _Vars = []})
  when Name =:= no_return; Name =:= any; Name =:= none ->
  % Unit type.
  ?T_UNIT;
translate_type({atom, _, ok}) ->
  % Atom ok translated as unit type.
  ?T_UNIT;
translate_type({user_type, _, Name, _Vars = []}) ->
  % Mailbox type. Mailbox types default to the write capability.
  make_type_name(Name) ++ ?C_WRITE;
translate_type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Message signature type.
  Params = string:join(translate_type_seq(TypeSeq), ?SEP_PAT),
  io_lib:format("~s(~s)", [make_type_name(Name), Params]);
translate_type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Union of message types
  string:join(translate_type_seq(TypeSeq), ?SEP_PAT).

translate_type_seq([]) ->
  [];
translate_type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  % Erlang PIDs are not translated and eaten.
  translate_type_seq(TypeSeq);
translate_type_seq([Type | TypeSeq]) ->
  [translate_type(Type) | translate_type_seq(TypeSeq)].

make_type_name(Type) when is_atom(Type) ->
  string:titlecase(atom_to_list(Type)).

make_fun_name(Fun) when is_atom(Fun) ->
  atom_to_list(Fun).


%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------

%% @private Translates receive/case and if clauses.
%%translate_clauses(Clauses, MbCtx) ->
%%  {Clauses0, MbCtx0} =
%%    lists:foldl(
%%      fun(Clause, {Clauses0, MbCtx0}) ->
%%        {Clause1, MbCtx1} = translate_clause(Clause, MbCtx0),
%%        {[Clause1 | Clauses0], MbCtx1}
%%      end,
%%      {[], MbCtx}, Clauses
%%    ),
%%  {lists:reverse(Clauses0), MbCtx0}.

translate_o_clauses(Fun, Clauses, MbCtx) when is_function(Fun, 2), is_list(Clauses) ->
  {Clauses0, MbCtx0} =
    lists:foldl(
      fun(Clause, {Clauses0, MbCtx0}) ->
        {Clause1, MbCtx1} = Fun(Clause, MbCtx0),
        {[Clause1 | Clauses0], MbCtx1}
      end,
      {[], MbCtx}, Clauses
    ),
  {lists:reverse(Clauses0), MbCtx0}.

translate_case_clauses(Clauses, MbCtx) ->
  translate_o_clauses(fun translate_case_clause/2, Clauses, MbCtx).

translate_if_clauses(Clauses, MbCtx) ->
  translate_o_clauses(fun translate_if_clause/2, Clauses, MbCtx).

%% @private Translates a receive/case and if clause.
translate_case_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, MbCtx) ->
  % Unguarded receive or case clause.
  MbCtx0 = new_mb(MbCtx),

  PatSeq0 = translate_pat_seq(PatSeq),
  {Body0, MbCtx1} = translate_body(Body, MbCtx0),
  Expr1 = io_lib:format("receive ~s from ~s ->~n~s", [
    PatSeq0, MbCtx0, Body0
  ]),

  {Expr1, MbCtx1}.

translate_if_clause(_Clause = {clause, Anno, _PatSeq = [], GuardSeq = [_], Body}, MbCtx) ->
  % Unguarded if clause.
  ?TRACE("(~p) About to translate if clause: ~p", [MbCtx, _Clause]),
  {Body0, MbCtx0} = translate_body(Body, MbCtx),
  ?TRACE("(~p) Translated body of if clause: ~p", [MbCtx0, Body0]),

  Expr1 =
    case translate_guard_seq(GuardSeq) of
      [["true"]] ->
        % Default Erlang if guard equates to else branch in Pat.
%%        io_lib:format("{~n~s~n}", [Body0]);
        io_lib:format("~s", [Body0]); %% TODO: Consider removing format call.
      GuardSeq0 ->
        % If branch.
%%        io_lib:format("(~s) {~n~s~n}~n", [GuardSeq0, Body0])
        {io_lib:format("~s", [GuardSeq0]), Body0} %% TODO: Consider removing format call.
    end,

  {Expr1, MbCtx0}.


translate_body(ExprSeq, MbCtx) ->
  {ExprSeq0, MbCtx0} = translate_expr_seq(ExprSeq, MbCtx),
  {string:join(ExprSeq0, ?SEP_NL), MbCtx0}.

%%translate_args(ExprSeq, MbCtx) -> % TODO: Not needed and for completeness.
%%  {ExprSeq0, MbCtx0} = translate_expr_seq(ExprSeq, MbCtx),
%%  {string:join(ExprSeq0, ?SEP_PAT), MbCtx0}.

%%translate_expr_seq(ExprSeq, MbCtx) ->
%%  ?TRACE("(~s) Translating open expression sequence.", [MbCtx]),
%%  {ExprSeq0, MbCtx0} =
%%    lists:foldl(
%%      fun(Expr, {ExprSeq0, MbCtx0}) ->
%%        {Expr1, MbCtx1} = translate_expr(Expr, MbCtx0),
%%        {[Expr1 | ExprSeq0], MbCtx1}
%%      end,
%%      {[], MbCtx}, ExprSeq
%%    ),
%%  {lists:reverse(ExprSeq0), MbCtx0}.



translate_expr_seq([], MbCtx) ->
  {[], MbCtx};
translate_expr_seq([Expr | ExprSeq], MbCtx) ->
  {Expr0, Rest, MbCtx0} = translate_expr(Expr, ExprSeq, MbCtx),
  {Exprs0, MbCtx1} = translate_expr_seq(Rest, MbCtx0),
  {[Expr0 | Exprs0], MbCtx1}.


%%TODO: REMOVE THE HACK AND INTEGRATE WITH REST.
%%TODO: FIX THE MATCH BUG.
%%TODO: INTEGRATE THE ERL_SYNTAX MODULE.
%%TODO: ADD THE PASS THAT CONVERTS CERTAIN EXPRESSIONS TO MATCH EXPRESSIONS.
%%hack_it(Expr = {call, Anno, Spawn = {atom, _, spawn}, _MFArgs = [_, Fun, Args]}, MbCtx) ->
%%  % Spawn.
%%%%  Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),
%%  Interface = make_type_name(paterl_anno:interface(Anno)),
%%
%%  Args0 = erl_syntax:list_elements(Args),
%%  Expr0 = erl_syntax:revert(
%%    erl_syntax:set_pos(
%%      erl_syntax:application(Fun, Args0), paterl_anno:set_modality(use, Anno))
%%  ),
%%
%%  MbCtx0 = new_mb(MbCtx),
%%  MbCtx1 = new_mb(MbCtx0),
%%  MbCtx2 = new_mb(MbCtx1),
%%
%%  {Expr1, [], MbCtx1} = translate_expr(Expr0, [], MbCtx1),
%%
%%  Expr2 =
%%    lists:flatten(
%%      io_lib:format("spawn {~nlet (x, ~s) = ~s in free(~s); x~n}", [
%%        MbCtx2, Expr1, MbCtx2
%%      ])
%%    ),
%%  Expr3 = io_lib:format("let ~s =~nnew[~s]~nin~nlet y =~n~s~nin~n~s", [
%%    MbCtx1, Interface, Expr2, MbCtx1
%%  ]),
%%  Expr4 = io_lib:format("(~s, ~s)", [Expr3, MbCtx]),
%%  {Expr4, MbCtx};
%%hack_it({call, Anno, Fun = {atom, _, Name}, Args}, MbCtx) ->
%%  % Explicit internal function call and explicit internal mailbox-annotated
%%  % function call.
%%  case paterl_anno:modality(Anno) of
%%    undefined ->
%%      % Call to closed function call outside mailbox context.
%%      Args0 = translate_args(Args),
%%      ?TRACE("Translated function arguments: ~p", [lists:flatten(Args0)]),
%%      io_lib:format("ERROR!(~s(~s), ~s)", [Name, Args0, MbCtx]);
%%    new ->
%%      % Only the new interface modality is expected at this point.
%%%%      Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),
%%      Interface = make_type_name(paterl_anno:interface(Anno)),
%%
%%      Expr0 = erl_syntax:revert(
%%        erl_syntax:set_pos(
%%          erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
%%      ),
%%
%%      MbCtx0 = new_mb(MbCtx),
%%      MbCtx1 = new_mb(MbCtx0),
%%      MbCtx2 = new_mb(MbCtx1),
%%
%%      {Expr1, [], MbCtx1} = translate_expr(Expr0, [], MbCtx1),
%%
%%      Expr2 =
%%        lists:flatten(
%%          io_lib:format("let (x, ~s) =~n~s~nin~nlet y =~nfree(~s)~nin~nx", [
%%            MbCtx2, Expr1, MbCtx2
%%          ])
%%        ),
%%
%%      Expr3 = io_lib:format("let ~s =~nnew[~s]~nin~n~s", [
%%        MbCtx1, Interface, Expr2
%%      ]),
%%      Expr4 = io_lib:format("(~s, ~s)", [Expr3, MbCtx]),
%%      {Expr4, MbCtx2}
%%  end.


%% @private Translates values and expressions.
translate_expr({call, _, {atom, _, self}, _MFArgs = []}, ExprSeq, MbCtx) ->
  % Self.
  Expr0 = io_lib:format("(~s, ~s)", [MbCtx, MbCtx]),
  {Expr0, ExprSeq, MbCtx};

%%% TODO: This is currently a hack until I discuss with Simon the let expression scoping in when as a tuple element issue.
%%translate_expr(Expr = {call, _, {atom, _, spawn}, _MFArgs = [_Mod, _Fun, _Args]}, ExprSeq, MbCtx) ->
%%  % Spawn.
%%  {Expr0, MbCtx0} = hack_it(Expr, MbCtx),
%%  {Expr0, ExprSeq, MbCtx0};

translate_expr(Expr = {call, Anno, Fun = {atom, _, Name}, Args}, ExprSeq, MbCtx) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.

  Expr1 =
    case paterl_anno:interface(Anno) of
      undefined ->
        io:format("-------------------------------------------------------> We are translating function in context but it is closed~n", []),
        % Call to closed function call outside mailbox context.
        {PatExpr, []} = translate_c_expr(Expr, []),
        io_lib:format("(~s, ~s)", [PatExpr, MbCtx]);

      Interface ->
        % Call to an open function call inside mailbox context.
        case paterl_anno:modality(Anno) of
          new ->
            io:format("-------------------------------------------------------> We are translating a NEW function in context~n", []),
            % Inject new mailbox.
            {Expr2, []} = translate_c_expr(Expr, []),
            io_lib:format("(~s, ~s)", [Expr2, MbCtx]); % Re-enable this line and delete the one below once fixed.
%%            % TODO: This is a hack until the issue is fixed from Pat's end.
%%            {Expr2, _} = hack_it(Expr, MbCtx),
%%            Expr2;

          use ->
            io:format("-------------------------------------------------------> We are translating a USE function in context ~p~n", [Expr]),
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(MbCtx)) | Args],
            ?TRACE("Args: ~p", [translate_args(Args0)]),
            io_lib:format("~s(~s)", [
              Name, translate_args(Args0)
            ])
        end
    end,
  {Expr1, ExprSeq, MbCtx};



translate_expr({match, _, Pat, Expr}, ExprSeq, MbCtx) ->
  % Match.
  {PatExpr0, [], MbCtx0} = translate_expr(Expr, [], MbCtx),
  MbCtx1 = new_mb(MbCtx0),
%%  {PatExpr1, MbCtx2} = translate_body(ExprSeq, MbCtx1),

  LetPat = io_lib:format("(~s, ~s)", [translate_pat(Pat), MbCtx1]),
  {Cont, MbCtx3} =
  case translate_body(ExprSeq, MbCtx1) of
    {[], MbCtx1} ->
      {LetPat, MbCtx1};
    {PatExpr1, MbCtx2} ->
      {PatExpr1, MbCtx2}
  end,

  PatExpr2 = io_lib:format(
    "let ~s =~n~s~nin~n~s", [LetPat, PatExpr0, Cont]
  ),
  {PatExpr2, [], MbCtx3};



translate_expr({'if', _, Clauses = [Clause0, Clause1]}, ExprSeq, MbCtx) ->
  % If else.
%%  {[Clause0, Clause1], MbCtx0} = translate_if_clauses(Clauses, MbCtx),
%%  {{Cond, PatExpr0}, MbCtx0} = translate_if_clause(Clause0, MbCtx), % If
%%  {PatExpr1, MbCtx1} = translate_if_clause(Clause1, MbCtx0), % Else

  {{Cond, PatExpr0}, MbCtx0} = translate_if_clause(Clause0, MbCtx), % If
  {PatExpr1, MbCtx1} = translate_if_clause(Clause1, MbCtx), % Else

  ?TRACE("Translated clauses: ~p and ~p", [PatExpr0, PatExpr1]),

%%  Expr1 = io_lib:format("if ~selse ~s", [
%%    Clause0, Clause1
%%  ]),
  PatExpr2 = io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [Cond, PatExpr0, PatExpr1]),
  MbCtx2 = new_mb(MbCtx1),
%%  {Expr1, MbCtx1};
  {PatExpr2, ExprSeq, MbCtx2};

%%{Cond, PatExpr0} = translate_if_clause(Clause0), % If
%%PatExpr1 = translate_if_clause(Clause1), % Else
%%?TRACE("If: ~p", [PatExpr0]),
%%?TRACE("Else: ~p", [PatExpr1]),
%%PatExpr2 = io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [Cond, PatExpr0, PatExpr1]),
%%{PatExpr2, ExprSeq};


translate_expr({'receive', Anno, Clauses}, ExprSeq, MbCtx) ->
  % Receive.
  State = paterl_anno:state(Anno),

  {Clauses0, MbCtx0} = translate_case_clauses(Clauses, MbCtx),

  Clauses1 =
    case is_mb_empty(State) of
      true ->
        MbCtx1 = new_mb(MbCtx),
        [io_lib:format("empty(~s) ->~n((), ~s)~n", [MbCtx1, MbCtx1]) | Clauses0];
      false ->
        Clauses0
    end,

  Expr0 = io_lib:format("guard ~s: ~s {~n~s~n}", [
    MbCtx, State, Clauses1
  ]),
  {Expr0, ExprSeq, MbCtx0};


%%translate_expr(Expr, ExprSeq, MbCtx)
%%  when
%%%%  element(1, Expr) =:= integer;
%%%%  element(1, Expr) =:= float;
%%%%  element(1, Expr) =:= string;
%%%%  element(1, Expr) =:= atom;
%%%%  element(1, Expr) =:= var;
%%  element(1, Expr) =:= op ->
%%  % Literal.
%%  % Variable.
%%  % Unguarded function call to closed expression.
%%  % Binary operator.
%%  % Unary operator.
%%  % Spawn.
%%  {PatExpr0, []} = translate_c_expr(Expr, []),
%%  MbCtx0 = new_mb(MbCtx),
%%
%%
%%
%%  {PatExpr2, MbCtx1} = translate_body(ExprSeq, MbCtx0),
%%
%%
%%  PatExpr2 = io_lib:format(
%%  "let (~s, ~s) =~n~s~nin~n~s", ["tx", MbCtx0, PatExpr0, PatExpr2]
%%  ),
%%%%  {PatExpr2, [], MbCtx2};
%%  {PatExpr2, [], MbCtx1};

%%translate_expr(Expr, ExprSeq, MbCtx)
%%  when element(1, Expr) =:= opdd ->
%%  % Binary operator.
%%  % Unary operator.
%%  {PatExpr0, []} = translate_c_expr(Expr, []),
%%  PatExpr1 = io_lib:format("(~s, ~s)", [PatExpr0, MbCtx]),
%%
%%  MbCtx1 = new_mb(MbCtx),
%%  {PatExpr2, MbCtx2} = translate_body(ExprSeq, MbCtx1),
%%%%  PatExpr2 = io_lib:format("(~s, ~s)", ["tx", MbCtx1]),
%%
%%  PatExpr3 = io_lib:format(
%%    "let (~s, ~s) =~n~s~nin~n~s", ["tx", MbCtx1, PatExpr1, PatExpr2]
%%  ),
%%
%%%%  {PatExpr1, ExprSeq, MbCtx};
%%  {PatExpr3, [], MbCtx2};

%%PatExpr2 = io_lib:format(
%%"let (~s, ~s) =~n~s~nin~n~s", [translate_pat(Pat), MbCtx1, PatExpr0, PatExpr1]
%%),
%%{PatExpr2, [], MbCtx2};

translate_expr(Expr, ExprSeq, MbCtx) ->
  % Literal.
  % Variable.
  % Unguarded function call to closed expression.
  % Binary operator.
  % Unary operator.
  % Spawn.
  ?TRACE("Expr: ~p", [Expr]),
  {PatExpr0, []} = translate_c_expr(Expr, []),
  ?TRACE("PatExpr0: ~p", [PatExpr0]),
  % TODO: LET DESUGARING HERE. I need to insert an Erlang match expression.
  % TODO: Also remove redundant hack_it code to tighten the translation.
  PatExpr1 = io_lib:format("(~s, ~s)", [PatExpr0, MbCtx]),
  {PatExpr1, ExprSeq, MbCtx}.


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------

%% @private Translates closed functions and if clauses.
translate_fun_clauses(Clauses) ->
%%  [translate_fun_clause(Clause) || Clause <- Clauses].
  translate_c_clauses(Clauses, fun translate_fun_clause/1).

translate_if_clauses(Clauses) ->
%%  [translate_if_clause(Clause) || Clause <- Clauses].
  translate_c_clauses(Clauses, fun translate_if_clause/1).

translate_c_clauses(Clauses, Fun) when is_list(Clauses), is_function(Fun, 1) ->
  [Fun(Clause) || Clause <- Clauses].

%% @private Translates closed function and if clauses.
translate_fun_clause(_Clause = {clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Mailbox-annotated or non mailbox-annotated unguarded function clause.

  % Translate function return type.
%%  RetType = erl_to_pat_type(paterl_anno:type(Anno)),
%%  RetType = erl_to_pat_type(paterl_anno:type(Anno)),
  RetType = translate_type(paterl_anno:type(Anno)),

  % Determine whether function is mailbox-annotated or non mailbox-annotated.
  case paterl_anno:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating NON mailbox-annotated guarded function clause */~b.",
        [length(PatSeq)]
      ),

      % Translate function parameters and body.
      Params = translate_params(PatSeq),
      Body0 = translate_body(Body),

      io_lib:format("(~s): ~s {~n~s~n}~n",
        [Params, RetType, Body0]
      );
    Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating mailbox-annotated guarded function clause */~b.",
        [length(PatSeq)]
      ),

      % Create mailbox that is injected as the first parameter of the mailbox-
      % annotated function.
%%      reset_mb(),
      MbCtx = new_mb(),
      MbType = io_lib:format("~s?",
%%        [string:titlecase(atom_to_list(paterl_anno:interface(Anno)))]
        [make_type_name(paterl_anno:interface(Anno))]
      ),


%%      MbType = make_recv_type(Interface),
%%      MbPat = erl_syntax:revert(
%%        erl_syntax:set_pos(
%%          erl_syntax:variable(MbCtx),
%%          paterl_anno:set_type(Interface, Anno)
%%        )),

%%      MbType = erl_syntax:revert(erl_syntax:user_type_application(erl_syntax:atom(Interface), [])),
%%      MbPat = erl_syntax:revert(
%%        erl_syntax:set_pos(
%%          erl_syntax:variable(MbCtx),
%%          paterl_anno:set_read(paterl_anno:read(Anno), paterl_anno:set_type(MbType, Anno))
%%        )),

      Params =
        case PatSeq of
          [] ->
            io_lib:format("~s: ~s", [MbCtx, MbType]);
          PatSeq ->
            io_lib:format("~s: ~s, ~s", [MbCtx, MbType, translate_params(PatSeq)])
        end,

%%      MbParam = translate_params([MbPat]),


      % Translate function parameters and body.
%%      Params = io_lib:format("~s", [translate_params(PatSeq)]),

%%      Params = "",

%%      ?TRACE(">>> MbParam: ~p", [MbParam]),
%%      ?TRACE(">>> MbParam flattened: ~p", [lists:flatten(MbParam)]),
%%      ?TRACE(">>> Params: ~p", [translate_params(PatSeq)]),
%%      ?TRACE(">>> Params flattened: ~p", [lists:flatten(translate_params(PatSeq))]),
%%      ?TRACE(">>> Flattened: ~p", [[MbParam | Params]]),

%%      Params0 = string:join([MbParam, Params], ?SEP_PAT),

      {Body0, _} = translate_body(Body, MbCtx),

      io_lib:format("(~s): (~s * ~s) {~n~s~n}~n",
%%        [Params, RetType, Interface, Body0]
        [Params, RetType, MbType, Body0]
      )
  end.

translate_if_clause({clause, _, _PatSeq = [], GuardSeq = [_], Body}) ->
  % Guarded if clause.
  Body0 = translate_body(Body),

  % Determine whether clause is the default catch-all true clause.
  case translate_guard_seq(GuardSeq) of
    [["true"]] ->
      % Default Erlang if guard equates to else branch in Pat.
      io_lib:format("~s", [Body0]); %% TODO: Consider removing format call.
    GuardSeq0 ->
      % If branch.
      {io_lib:format("~s", [GuardSeq0]), Body0} %% TODO: Consider removing format call.
%%      io_lib:format("(~s) ~s", [GuardSeq0, Body0])
  end.

translate_body(ExprSeq) ->
  string:join(translate_c_expr_seq(ExprSeq), ?SEP_NL).
translate_args(ExprSeq) ->
  string:join(translate_c_expr_seq(ExprSeq), ?SEP_PAT).

%% @private Translates value and expression sequences.
%%translate_expr_seq(ExprSeq) ->
%%  [translate_expr(Expr) || Expr <- ExprSeq].

translate_c_expr_seq([]) ->
  [];
translate_c_expr_seq([Expr | ExprSeq]) ->
  {Expr0, Rest} = translate_c_expr(Expr, ExprSeq),
  [Expr0 | translate_c_expr_seq(Rest)].

%% @private Translates values and expressions.
translate_c_expr(Lit, ExprSeq)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  {translate_lit(Lit), ExprSeq};
translate_c_expr({var, _, Name}, ExprSeq) ->
  % Variable.
  PatExpr = string:lowercase(atom_to_list(Name)),
  ?TRACE("PatExpr [var]: ~p", [PatExpr]),
  {PatExpr, ExprSeq};
translate_c_expr({tuple, _, [_Tag = {atom, _, Name} | Payload]}, ExprSeq) ->
  % Message.
  Tag = make_type_name(Name),
  Args = translate_args(Payload),
  PatExpr = io_lib:format("~s(~s)", [Tag, Args]),
  {PatExpr, ExprSeq};
translate_c_expr({call, Anno, {atom, _, spawn}, _MFArgs = [_, Fun, Args]}, ExprSeq) ->
  % Spawn.
  Interface = make_type_name(paterl_anno:interface(Anno)),

  Args0 = erl_syntax:list_elements(Args),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args0), paterl_anno:set_modality(use, Anno))
  ),

  MbCtx0 = new_mb(),
  MbCtx1 = new_mb(MbCtx0),

  {PatExpr1, [], _} = translate_expr(Expr0, [], MbCtx0),

  PatExpr2 =
    lists:flatten(
      io_lib:format("spawn {~nlet (x, ~s) = ~s in free(~s); x~n}", [
        MbCtx1, PatExpr1, MbCtx1
      ])
    ),
  PatExpr3 = io_lib:format("let ~s =~nnew[~s]~nin~nlet y =~n~s~nin ~s", [
    MbCtx0, Interface, PatExpr2, MbCtx0
  ]),
  {PatExpr3, ExprSeq};
translate_c_expr({call, _, {atom, _, format}, _}, ExprSeq) ->
  % Format calls.
  {"()", ExprSeq};
translate_c_expr({call, Anno, Fun = {atom, _, Name}, Args}, ExprSeq) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.

  PatExpr =
    case paterl_anno:modality(Anno) of
      undefined ->
        % Call to closed function call outside mailbox context.
        Args0 = translate_args(Args),
        ?TRACE("Translated function arguments: ~p", [lists:flatten(Args0)]),
        io_lib:format("~s(~s)", [Name, Args0]);
      new ->
        % Only the new interface modality is expected at this point.
        Interface = make_type_name(paterl_anno:interface(Anno)),

        MbCtx0 = new_mb(),
        MbCtx1 = new_mb(MbCtx0),

        Expr0 = erl_syntax:revert(
          erl_syntax:set_pos(
            erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
        ),

        {PatExpr1, [], _} = translate_expr(Expr0, [], MbCtx0),

        PatExpr2 =
          lists:flatten(
            io_lib:format("let (x, ~s) =~n~s~nin~nlet y =~nfree(~s)~nin~nx", [
              MbCtx1, PatExpr1, MbCtx1
            ])
          ),
        io_lib:format("let ~s =~nnew[~s]~nin~n~s", [
          MbCtx0, Interface, PatExpr2
        ])
    end,
  {PatExpr, ExprSeq};

%%translate_c_expr({match, _, Pat, Expr}, ExprSeq) ->
%%  % Match.
%%  {PatExpr0, []} = translate_c_expr(Expr, []),
%%  ?TRACE("Translated C expr: ~p", [PatExpr0]),
%%  PatExpr1 = translate_body(ExprSeq),
%%  PatExpr2 = io_lib:format(
%%    "(let ~s =~n~s~nin~n~s)", [translate_pat(Pat), PatExpr0, PatExpr1]
%%  ),
%%  {PatExpr2, []};
translate_c_expr({match, _, Pat, Expr}, ExprSeq) ->
  % Match.
  {PatExpr0, []} = translate_c_expr(Expr, []),
  ?TRACE("Translated C expr: ~p", [PatExpr0]),

  LetPat = translate_pat(Pat),
  Cont =
    case translate_body(ExprSeq) of
      [] ->
        % This was the last expression in the expression sequence. Continuation of
        % let expression is the variable in the subject of the let expression.
        LetPat;
      PatExpr ->
        PatExpr
    end,

  PatExpr2 = io_lib:format(
    "let ~s =~n~s~nin~n~s", [LetPat, PatExpr0, Cont]
  ),
  {PatExpr2, []};
translate_c_expr({op, Anno, Op, Expr0, Expr1}, ExprSeq) ->
  % Binary operator.
  {PatExpr0, []} = translate_c_expr(Expr0, []),
  {PatExpr1, []} = translate_c_expr(Expr1, []),
  PatExpr2 = io_lib:format("~s ~s ~s", [PatExpr0, Op, PatExpr1]),
  {PatExpr2, ExprSeq};
translate_c_expr({op, _, Op, Expr}, ExprSeq) ->
  % Unary operator.
  {PatExpr0, []} = translate_c_expr(Expr, []),
  PatExpr1 = io_lib:format("~s ~s", [Op, PatExpr0]),
  {PatExpr1, ExprSeq};
translate_c_expr({'if', _, [Clause0, Clause1]}, ExprSeq) ->
  % If else.
%%  [PatExpr0, PatExpr1] = translate_if_clauses(Clauses),
  {Cond, PatExpr0} = translate_if_clause(Clause0), % If
  PatExpr1 = translate_if_clause(Clause1), % Else
  ?TRACE("If: ~p", [PatExpr0]),
  ?TRACE("Else: ~p", [PatExpr1]),
  PatExpr2 = io_lib:format("if (~s) {~n~s~n}~nelse {~n~s~n}", [Cond, PatExpr0, PatExpr1]),
  {PatExpr2, ExprSeq};
translate_c_expr(Other, ExprSeq) ->
  PatExpr = io_lib:format("<Cannot translate ~p>", [Other]),
  {PatExpr, ExprSeq}.

%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------

%% @private Translates a guard sequence.
translate_guard_seq(GuardSeq) ->
  [translate_guard(Guard) || Guard <- GuardSeq].

%% @private Translates a guard, which is a sequence of guard tests.
translate_guard(GuardTests) ->
  [translate_guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates a guard test.
translate_guard_test(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  translate_lit(Lit);
translate_guard_test({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Function call.
  % TODO: I think the arguments need to be translated as expressions and not guard tests.
  GuardTests0 = string:join(translate_guard(GuardTests), ","),
  [atom_to_list(Name), "(", GuardTests0, ")"];
translate_guard_test({op, _, Op, GuardTest0, GuardTest1}) ->
  % Binary operator.
  GuardTest2 = translate_guard_test(GuardTest0),
  GuardTest3 = translate_guard_test(GuardTest1),
  [GuardTest2, " ", atom_to_list(Op), " ", GuardTest3];
translate_guard_test({op, _, Op, GuardTest}) ->
  % Unary operator.
  GuardTest0 = translate_guard_test(GuardTest),
  [atom_to_list(Op), GuardTest0].

translate_params(PatSeq) ->
  Translate =
    fun(Pat) ->
      Anno = element(2, Pat),
      Type = paterl_anno:type(Anno),
%%      ?TRACE("±Type is: ~p", [Type]),
%%      Capability =
%%        case paterl_anno:read(Anno) of
%%          true -> "?";
%%          false -> "!"
%%        end,

%%      io_lib:format("~s: ~s~s", [translate_pat(Pat), translate_type(Type), Capability])
      io_lib:format("~s: ~s", [translate_pat(Pat), translate_type(Type)])
    end,
  string:join([Translate(Pat) || Pat <- PatSeq], ?SEP_PAT).

%% @private Translates pattern sequences.
translate_pat_seq(PatSeq) ->
  string:join([translate_pat(Pat) || Pat <- PatSeq], ?SEP_PAT).

%% @private Translates patterns.
translate_pat(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  translate_lit(Lit);
translate_pat({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_pat({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
  Tag = make_type_name(Name),
  Args = translate_pat_seq(Payload),
  io_lib:format("~s(~s)", [Tag, Args]).

%% @private Translates literals.
translate_lit({integer, _, Value}) ->
  integer_to_list(Value);
translate_lit({float, _, Value}) ->
  float_to_list(Value);
translate_lit({string, _, Value}) ->
  [$", Value, $"];
translate_lit({atom, _, Value}) ->
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%%make_mb(MbId) ->
%%  "mb" + integer_to_list(MbId).

%%new_mb1(MbId) ->
%%  {"mb" ++ integer_to_list(MbId), MbId + 1}.

new_mb() ->
  "mb" ++ integer_to_list(?MB_IDX_START).

new_mb([$m, $b | IdStr]) ->
  "mb" ++ integer_to_list(list_to_integer(IdStr) + 1).

%%reset_mb() ->
%%  put(mb, 0).
%%
%%new_mb() ->
%%  "mb" ++ integer_to_list(put(mb, get(mb) + 1)).

%%new_mb() ->
%%  "mb" ++
%%  integer_to_list(
%%    case get(mb) of
%%      undefined ->
%%        put(mb, 1),
%%        0;
%%      Mb ->
%%        put(mb, Mb + 1),
%%        Mb
%%    end
%%  ).

%% @private Checks whether a mailbox can become potentially empty.
is_mb_empty(Regex) ->
  case re:run(Regex, "^(\\*.*|1.*|.*1)$") of
    {match, _} ->
      true;
    _ ->
      false
  end.

% TODO: Requires a regular expression parser.
simplify([]) ->
  ok.


