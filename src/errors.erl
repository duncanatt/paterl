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

show_errors(Errors) ->
  [show_error(E) || E <- Errors].

show_errors(File, Errors) ->
  File0 = lists:last(filename:split(File)),
  [show_error({File0, E}) || E <- Errors].


show_error({File, {ANNO, Mod, E}}) ->
  io:format(
    standard_error, "~ts:~w: Error: ~ts~n", [File, ANNO, Mod:format_error(E)]
  );
show_error({ANNO, Mod, E}) ->
  io:format(standard_error, "~w: Error: ~ts~n", [ANNO, Mod:format_error(E)]);
show_error({Mod, E}) ->
  io:format(standard_error, "Error: ~ts~n", [Mod:format_error(E)]).