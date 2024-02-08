-module(main_pt).
-export([parse_transform/2]).

parse_transform(Ast, _Opt) ->
  io:format("AST:~p~n",[Ast]),
%%  [parse(X) || X <- Ast],
  parse(Ast),
  Ast.

parse([]) ->
  [];
parse([Form | Forms]) ->
  io:format("Form=~p~n", [Form]),
  [Form | parse(Forms)].

%%parse({function, _Line, _FunName, _ArgNum, Clause}) ->
%%  [parse(X) || X <- Clause];
%%
%%parse({clause, _Line, _Vars, _Guard, ExprCalls}) ->
%%  [erl_eval:expr(X, erl_eval:new_bindings()) || X <- ExprCalls];
%%
%%parse(_) -> ok.