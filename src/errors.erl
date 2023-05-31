%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2023 15:09
%%%-------------------------------------------------------------------
-module(errors).
-author("duncan").

%%% Public API.
-export([show_errors/1, show_errors/2, show_error/1]).

%%% Types.
-export_type([error/0, errors/0]).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%{element(2, Node), ?MODULE, {Class, Node}}

-type error() :: {Anno :: erl_anno:anno(), Mod :: module(), Error :: any()} |
{Mod :: module(), Error :: any()}.
-type errors() :: [error()].


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec show_errors(errors()) -> any().
show_errors(Errors) ->
  [show_error(E) || E <- Errors].

-spec show_errors(file:name_all(), errors()) -> any().
show_errors(File, Errors) ->
  File0 = lists:last(filename:split(File)),
  [show_error({File0, E}) || E <- Errors].

-spec show_error(Descriptor :: {file:name_all(), error()} | error()) -> any().
show_error({File, {ANNO, Mod, E}}) ->
  io:format(
    standard_error, "~ts:~w: Error: ~ts~n", [File, ANNO, Mod:format_error(E)]
  );
show_error({ANNO, Mod, E}) ->
  io:format(standard_error, "~w: Error: ~ts~n", [ANNO, Mod:format_error(E)]);
show_error({Mod, E}) ->
  io:format(standard_error, "Error: ~ts~n", [Mod:format_error(E)]).