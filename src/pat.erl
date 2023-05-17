%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2023 12:29
%%%-------------------------------------------------------------------
-module(pat).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").


%% API
-export([]).

-compile(export_all).


file(File) ->
%%  case filelib:is_file(File) of
%%    true ->

%%      {ok, String} = file:read_file(File),
%%      {ok, Tokens, _} = erl_scan:string(binary_to_list(String), 0, []),

%%      ?DEBUG("Tokens: ~p", [Tokens]),

%%      parse_forms(Tokens, [], []);
      case epp:parse_file("src/my_test.erl", []) of
        {ok, Forms} ->
          Forms;
%%        {ok, [Form], Extra} ->
%%          ok;
        {error, OpenError} ->
          OpenError
%%      end;

%%    false ->
%%      invalid_file
  end.




