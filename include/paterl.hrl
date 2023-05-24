%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2023 09:39
%%%-------------------------------------------------------------------
-author("duncan").

%% Asserts the state of the mailbox in terms of the messages it can contain.
-define(mb_state(Regex), "@state:" ++ Regex).
-define(mb_state(Regex, _), "@state:" ++ Regex, no_free).

%% Asserts the state of the mailbox in terms of the messages it can contain and
%% specifies that a free guard is to be injected in the generated Pat IR.
%% TODO: An alternative is to always generate free when the regex contains *,
%% and use a flag to opt out of it, because it is more common.
-define(mb_state_free(Regex), "@state:" ++ Regex, free).

%% Specifies that a free guard is to be injected in the generated IR.
-define(free, free).
