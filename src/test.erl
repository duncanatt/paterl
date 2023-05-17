-module(test).

-compile([export_all]).

-define(ABS_KEY, abstract_code).

-spec get_ast(string()) -> erl_syntax:syntaxTree().
get_ast(File) ->
	{ok, {_ModName, [{?ABS_KEY, {raw_abstract_v1, Ast}}]}} =
	beam_lib:chunks(File, [?ABS_KEY]),
	Ast.

-spec print_ast(erl_syntax:syntaxTree()) -> ok.
print_ast(Ast) ->
	io:put_chars(erl_prettypr:format(erl_syntax:form_list(Ast))).
%%	io:put_chars(erl_prettypr:format(Ast)).

