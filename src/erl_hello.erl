-module(erl_hello).

-export[hello/1].

-dummy("Duncan Paul Attard").

-record(user, {name :: string(), age :: integer()}).

-type user() :: #user{}.

-type literal_integer() :: integer().
-type literal_string() :: string().

-spec hello(Name :: string()) -> ok.
hello(Name) when "test" ->
	io:format("Hello ~p.~n", [Name]).

%%-spec new_user(string(), integer()) -> user().
%%new_user(Name, Age) ->
%%	#user{name = Name, age = Age}.

%%binary() ->
%%	<<"duncan">>,
%%	<<"binary">> = <<"bin">>.

case_test() ->
	case "" of
		X when "" ->
			ok
	end.

receive_test() ->
	"Regexp",
	receive
		X when X =:= "Regexp One" ->
			ok;
		X when "Regexp Standalone" ->
			ok;
		Y when 1 ->
			ok
	end.
%%
%%string_test() -> "test", 15 + 6, hello ! 56.
%%
%%atomic_lit_pat(1, 3.14, hello, "test", $c) -> ok.

