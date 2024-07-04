%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans).
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

-define(TYP_INTEGER, "Int").

-define(TYP_FLOAT, "Float").

-define(TYP_STRING, "String").

-define(TYP_UNIT, "Unit").

-define(CAP_WRITE, "!").

-define(CAP_READ, "?").

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
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------

translate_form({attribute, _, module, Name}) ->
  io_lib:format("# Translated from ~s.erl~n", [Name]);
translate_form({attribute, _, interface, {Name, Type, _Vars = []}}) ->
  Type0 = translate_type(Type),
  io_lib:format("interface ~s { ~s }~n~n", [make_type_name(Name), Type0]);

translate_form({function, _, Name, Arity, Clauses = [_]}) ->
  % Function with one clause.
  ?TRACE("Translating function ~s/~b.", [Name, Arity]),
  Clauses0 = translate_clauses(Clauses),
  io_lib:format("def ~s ~s~n", [make_fun_name(Name), Clauses0]);
translate_form(_) ->
  % Other Erlang module attributes without Pat equivalent.
  "".

%%TODO: Refactor the paterl_anno module to remove the capability annotation.
translate_type({type, _, pid, _Vars = []}) ->
  % Erlang PID type is not translated. This handles the case where the a mailbox
  % interface type is just a PID.
  "";
translate_type({type, _, integer, _Vars = []}) ->
  % Integer type.
  ?TYP_INTEGER;
translate_type({type, _, float, _Vars = []}) ->
  % Float type.
  ?TYP_FLOAT;
translate_type({type, _, string, _Vars = []}) ->
  % String type.
  ?TYP_STRING;
translate_type({type, _, Name, _Vars = []})
  when Name =:= no_return; Name =:= any; Name =:= none ->
  % Unit type.
  ?TYP_UNIT;
translate_type({atom, _, ok}) ->
  % Atom ok translated as unit type.
  ?TYP_UNIT;
translate_type({user_type, _, Name, _Vars = []}) ->
  % Mailbox type. Mailbox types default to the write capability.
  make_type_name(Name) ++ ?CAP_WRITE;
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
translate_clauses(Clauses, MbCtx) ->
  {Clauses0, MbCtx0} =
    lists:foldl(
      fun(Clause, {Clauses0, MbCtx0}) ->
        {Clause1, MbCtx1} = translate_clause(Clause, MbCtx0),
        {[Clause1 | Clauses0], MbCtx1}
      end,
      {[], MbCtx}, Clauses
    ),
  {lists:reverse(Clauses0), MbCtx0}.

%% @private Translates a receive/case and if clause.
translate_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, MbCtx) ->
  % Unguarded receive or case clause.
  MbCtx0 = new_mb(MbCtx),

  PatSeq0 = translate_pat_seq(PatSeq),
  {Body0, MbCtx1} = translate_body(Body, MbCtx0),
  Expr1 = io_lib:format("receive ~s from ~s ->~n~s", [
    PatSeq0, MbCtx0, Body0
  ]),

  {Expr1, MbCtx1};

translate_clause(_Clause = {clause, Anno, _PatSeq = [], GuardSeq = [_], Body}, MbCtx) ->
  % Unguarded if clause.
  ?TRACE("(~p) About to translate if clause: ~p", [MbCtx, _Clause]),
  {Body0, MbCtx0} = translate_body(Body, MbCtx),
  ?TRACE("(~p) Translated body of if clause: ~p", [MbCtx0, Body0]),

  Expr1 =
    case translate_guard_seq(GuardSeq) of
      [["true"]] ->
        % Default Erlang if guard equates to else branch in Pat.
        io_lib:format("{~n~s~n}", [Body0]);
      GuardSeq0 ->
        % If branch.
        io_lib:format("(~s) {~n~s~n}~n", [GuardSeq0, Body0])
    end,

  {Expr1, MbCtx0}.


translate_body(ExprSeq, MbCtx) ->
  {ExprSeq0, MbCtx0} = translate_expr_seq(ExprSeq, MbCtx),
  {string:join(ExprSeq0, ?SEP_NL), MbCtx0}.

translate_args(ExprSeq, MbCtx) -> % TODO: Not needed and for completeness.
  {ExprSeq0, MbCtx0} = translate_expr_seq(ExprSeq, MbCtx),
  {string:join(ExprSeq0, ?SEP_PAT), MbCtx0}.

translate_expr_seq(ExprSeq, MbCtx) ->
  ?TRACE("(~s) Translating open expression sequence.", [MbCtx]),
  {ExprSeq0, MbCtx0} =
    lists:foldl(
      fun(Expr, {ExprSeq0, MbCtx0}) ->
        {Expr1, MbCtx1} = translate_expr(Expr, MbCtx0),
        {[Expr1 | ExprSeq0], MbCtx1}
      end,
      {[], MbCtx}, ExprSeq
    ),
  {lists:reverse(ExprSeq0), MbCtx0}.

hack_it(Expr = {call, Anno, Spawn = {atom, _, spawn}, _MFArgs = [_, Fun, Args]}, MbCtx) ->
  % Spawn.
%%  Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),
  Interface = make_type_name(paterl_anno:interface(Anno)),

  Args0 = erl_syntax:list_elements(Args),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args0), paterl_anno:set_modality(use, Anno))
  ),

  MbCtx0 = new_mb(MbCtx),
  MbCtx1 = new_mb(MbCtx0),
  MbCtx2 = new_mb(MbCtx1),

  {Expr1, MbCtx1} = translate_expr(Expr0, MbCtx1),

  Expr2 =
    lists:flatten(
      io_lib:format("spawn { let (x, ~s) = ~s in free(~s); x }", [
        MbCtx2, Expr1, MbCtx2
      ])
    ),
  Expr3 = io_lib:format("let ~s =~nnew[~s]~nin~nlet y =~n~s~nin~n~s", [
    MbCtx1, Interface, Expr2, MbCtx1
  ]),
  Expr4 = io_lib:format("(~s, ~s)", [Expr3, MbCtx]),
  {Expr4, MbCtx};
hack_it({call, Anno, Fun = {atom, _, Name}, Args}, MbCtx) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.
  case paterl_anno:modality(Anno) of
    undefined ->
      % Call to closed function call outside mailbox context.
      Args0 = translate_args(Args),
      ?TRACE("Translated function arguments: ~p", [lists:flatten(Args0)]),
      io_lib:format("ERROR!(~s(~s), ~s)", [Name, Args0, MbCtx]);
    new ->
      % Only the new interface modality is expected at this point.
%%      Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),
      Interface = make_type_name(paterl_anno:interface(Anno)),

      Expr0 = erl_syntax:revert(
        erl_syntax:set_pos(
          erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
      ),

      MbCtx0 = new_mb(MbCtx),
      MbCtx1 = new_mb(MbCtx0),
      MbCtx2 = new_mb(MbCtx1),

      {Expr1, MbCtx1} = translate_expr(Expr0, MbCtx1),

      Expr2 =
        lists:flatten(
          io_lib:format("let (x, ~s) =~n~s~nin~nlet y =~nfree(~s)~nin~nx", [
            MbCtx2, Expr1, MbCtx2
          ])
        ),

      Expr3 = io_lib:format("let ~s =~nnew[~s]~nin~n~s", [
        MbCtx1, Interface, Expr2
      ]),
      Expr4 = io_lib:format("(~s, ~s)", [Expr3, MbCtx]),
      {Expr4, MbCtx2}
  end.


%% @private Translates values and expressions.
translate_expr({call, _, {atom, _, self}, _MFArgs = []}, MbCtx) ->
  % Self.
  Expr0 = io_lib:format("(~s, ~s)", [MbCtx, MbCtx]),
  {Expr0, MbCtx};

% TODO: This is currently a hack until I discuss with Simon the let expression scoping in when as a tuple element issue.
translate_expr(Expr = {call, _, {atom, _, spawn}, _MFArgs = [_Mod, _Fun, _Args]}, MbCtx) ->
  % Spawn.
  hack_it(Expr, MbCtx);

translate_expr(Expr = {call, Anno, Fun = {atom, _, Name}, Args}, MbCtx) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.

  Expr1 =
    case paterl_anno:interface(Anno) of
      undefined ->
        io:format("-------------------------------------------------------> We are translating function in context but it is closed", []),
        % Call to closed function call outside mailbox context.
        io_lib:format("(~s, ~s)", [translate_expr(Expr), MbCtx]);

      Interface ->
        % Call to an open function call inside mailbox context.
        case paterl_anno:modality(Anno) of
          new ->
            io:format("-------------------------------------------------------> We are translating a NEW function in context", []),
            % Inject new mailbox.
%%            io_lib:format("(~s, ~s)££", [translate_expr(Expr), MbCtx]); % Re-enable this line and delete the one below once fixed.
            % TODO: This is a hack until the issue is fixed from Pat's end.
            {Expr2, _} = hack_it(Expr, MbCtx),
            Expr2;

          use ->
            io:format("-------------------------------------------------------> We are translating a USE function in context ~p", [Expr]),
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(MbCtx)) | Args],
            io_lib:format("~s(~s)", [
              Name, translate_args(Args0)
            ])
        end
    end,
  {Expr1, MbCtx};



translate_expr({match, _, Pat, Expr}, MbCtx) ->
  % Match.
%%  {Expr0, MbCtx0} = translate_expr(Expr, MbCtx),
%%
%%  MbCtx1 = new_mb(MbCtx0),
%%
%%  Expr1 = io_lib:format("let (~s, ~s) =~n~s~nin", [
%%    translate_pat(Pat), MbCtx1, Expr0
%%  ]),
%%  {Expr1, MbCtx1};

  {Expr0, MbCtx0} = translate_expr(Expr, MbCtx),

  MbCtx1 = new_mb(MbCtx0),

  Expr1 = io_lib:format("let (~s, ~s) =~n~s~nin", [
    translate_pat(Pat), MbCtx1, Expr0
  ]),
  {Expr1, MbCtx1};


translate_expr({'if', _, Clauses = [_, _]}, MbCtx) ->
  % If else.
  {[Clause0, Clause1], MbCtx0} = translate_clauses(Clauses, MbCtx),
  ?TRACE("Translated clauses: ~p and ~p", [Clause0, Clause1]),

  Expr1 = io_lib:format("if ~selse ~s", [
    Clause0, Clause1
  ]),
  MbCtx1 = new_mb(MbCtx0),
%%  MbCtx1 = new_mb(),

%%  {["if ", Clause0, "else ", Clause1], MbCtx0};
  {Expr1, MbCtx1};
translate_expr({'receive', Anno, Clauses}, MbCtx) ->
  % Receive.
  State = paterl_anno:state(Anno),

  {Clauses0, MbCtx0} = translate_clauses(Clauses, MbCtx),

  Clauses1 =
    case is_mb_empty(State) of
      true ->
        MbCtx1 = new_mb(MbCtx),
        [io_lib:format("empty(~s) -> ((), ~s)~n", [MbCtx1, MbCtx1]) | Clauses0];
      false ->
        Clauses0
    end,

  Expr0 = io_lib:format("guard ~s: ~s {~n~s~n}", [
    MbCtx, State, Clauses1
  ]),
  {Expr0, MbCtx0};
translate_expr(Expr, MbCtx) ->
  % Literal.
  % Variable.
  % Unguarded function call to closed expression.
  % Binary operator.
  % Unary operator.
  % Spawn.
  Expr1 = io_lib:format("(~s, ~s)", [translate_expr(Expr), MbCtx]),
  {Expr1, MbCtx}.


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------

%% @private Translates closed functions and if clauses.
translate_clauses(Clauses) ->
  [translate_clause(Clause) || Clause <- Clauses].

%% @private Translates closed function and if clauses.
translate_clause(_Clause = {clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
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
  end;

translate_clause({clause, _, _PatSeq = [], GuardSeq = [_], Body}) ->
  % Guarded if clause.
  Body0 = translate_body(Body),

  % Determine whether clause is the default catch-all true clause.
  case translate_guard_seq(GuardSeq) of
    [["true"]] ->
      % Default Erlang if guard equates to else branch in Pat.
      io_lib:format("{~n~s~n}", [Body0]);
    GuardSeq0 ->
      % If branch.
      io_lib:format("(~s) {~n~s~n}~n", [GuardSeq0, Body0])
  end.

translate_body(ExprSeq) ->
  string:join(translate_expr_seq(ExprSeq), ?SEP_NL).
translate_args(ExprSeq) ->
  string:join(translate_expr_seq(ExprSeq), ?SEP_PAT).

%% @private Translates value and expression sequences.
translate_expr_seq(ExprSeq) ->
  [translate_expr(Expr) || Expr <- ExprSeq].

%% @private Translates values and expressions.
translate_expr(Lit)
  when
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  % Literal.
  translate_lit(Lit);
translate_expr({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_expr({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
  Tag = make_type_name(Name),
  Args = translate_args(Payload),
  io_lib:format("~s(~s)", [Tag, Args]);
translate_expr(Expr = {call, Anno, Spawn = {atom, _, spawn}, _MFArgs = [_, Fun, Args]}) ->
  % Spawn.
%%  Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),
  Interface = make_type_name(paterl_anno:interface(Anno)),

  Args0 = erl_syntax:list_elements(Args),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args0), paterl_anno:set_modality(use, Anno))
  ),

  MbCtx0 = new_mb(),
  MbCtx1 = new_mb(MbCtx0),

  {Expr1, _} = translate_expr(Expr0, MbCtx0),

  Expr2 =
    lists:flatten(
      io_lib:format("spawn { let (x, ~s) = ~s in free(~s); x }", [
        MbCtx1, Expr1, MbCtx1
      ])
    ),
  io_lib:format("let ~s =~nnew[~s]~nin~nlet y =~n~s~nin ~s", [
    MbCtx0, Interface, Expr2, MbCtx0
  ]);
translate_expr({call, _, {atom, _, format}, _}) ->
  % Format calls.
  "()";
translate_expr({call, Anno, Fun = {atom, _, Name}, Args}) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.

  case paterl_anno:modality(Anno) of
    undefined ->
      % Call to closed function call outside mailbox context.
      Args0 = translate_args(Args),
      ?TRACE("Translated function arguments: ~p", [lists:flatten(Args0)]),
      io_lib:format("~s(~s)", [Name, Args0]);
    new ->
      % Only the new interface modality is expected at this point.
%%      Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),
      Interface = make_type_name(paterl_anno:interface(Anno)),

      MbCtx0 = new_mb(),
      MbCtx1 = new_mb(MbCtx0),

      Expr0 = erl_syntax:revert(
        erl_syntax:set_pos(
          erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
      ),

      {Expr1, _} = translate_expr(Expr0, MbCtx0),

      Expr2 =
        lists:flatten(
          io_lib:format("let (x, ~s) =~n~s~nin~nlet y =~nfree(~s)~nin~nx", [
            MbCtx1, Expr1, MbCtx1
          ])
        ),
      io_lib:format("let ~s =~nnew[~s]~nin~n~s", [
        MbCtx0, Interface, Expr2
      ])
  end;

translate_expr({match, _, Pat, Expr}) ->
  % Match.
  io_lib:format("let ~s =~n~s~nin", [translate_pat(Pat), translate_expr(Expr)]);
translate_expr({op, Anno, Op, Expr0, Expr1}) ->
  % Binary operator.
  Expr2 = translate_expr(Expr0),
  Expr3 = translate_expr(Expr1),
  io_lib:format("~s ~s ~s", [Expr2, Op, Expr3]);
translate_expr({op, _, Op, Expr}) ->
  % Unary operator.
  Expr0 = translate_expr(Expr),
  io_lib:format("~s ~s", [Op, Expr0]);
translate_expr({'if', _, Clauses = [_, _]}) ->
  % If else.
  [Clause0, Clause1] = translate_clauses(Clauses),
  io_lib:format("if ~selse ~s", [Clause0, Clause1]);
%%translate_expr({cons, _, Expr, Rest}) ->
%%  Expr0 = translate_format_arg(Expr),
%%  case translate_expr(Rest) of
%%    [] ->
%%      % End of list.
%%      Expr0;
%%    Rest0 ->
%%      Expr0 ++ Rest0
%%  end;
%%translate_expr({nil, _}) ->
%%  "";
translate_expr(Other) ->
  io_lib:format("<Cannot translate ~p>", [Other]).

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
%%  io_lib:format("~.10f", [Value]);
translate_lit({string, _, Value}) ->
  Value;
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


%% Pending.
% 0. Fix bug in let where it generates an empty 'in' expression when it's the last thing in the sequence. NOT A BUG.
% 1. Upper case message tags in regular expressions.
% 2. Regular expression parser to simplify things.
% 3. Add pass before annotations that converts expression sequences to assignments.
% 4. Maybe ask Simon to support the _?
% 5. Add a lightweight check that excludes the unsupported Erlang expressions to bring the input as the syntax we have on paper.
% 6. Insert variable use in let expressions when we have unit () variables. This happens in when we let bind expressions
%    that return unit. There are three cases: send, function calls that return unit, 'if' condition maybe. We can have a function is_unit()?
% 7. Have a module that provides a PAT API to create functions out of translated expressions: this enables us to encapsulate
%    the string mess (pat_syntax).
% 8. Then have another module that provides an API that does the translation? Maybe or maybe not because then we would not
%    be able to leverage pattern matching (it becomes similar to when we are using erl_syntax.)
