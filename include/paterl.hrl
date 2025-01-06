%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% This file is intended for users of paterl.
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

%% Asserts the state of the mailbox in terms of the messages it can contain.
%%-define(mb_state(Regex, MbName), {mb_state, usable, Regex, MbName}).

%% Asserts the state of the mailbox in terms of the messages it can contain and
%% specifies that a free guard is to be injected in the generated Pat IR.
%%-define(mb_state_free(Regex, MbName), {mb_state, free, Regex, MbName}).

%%-define(free(MbName), {mb_state, free, "shouldbefilledautomatically?", MbName}).

%%-define(mb_assert(Mailbox, Regex), {'@state', Mailbox, Regex}).
%%-define(mb_assert(Mailbox, Regex), {'state', Mailbox, Regex}).
-define(mb_assert_regex(Regex), {'state', Regex}).

-define(mb_type(Mailbox), {'type', Mailbox}).

-define(mb_new(Mailbox), {'new', Mailbox}).

-define(mb_use(Mailbox), {'use', Mailbox}).

-define(as(MbName), {'@as', MbName}).

-define(expects(MbName, Pattern), {'@expects', MbName, Pattern}).


%%-define(M_NEW, new).
%%-define(M_USE, use).

%%-define(T_TYPE, type).
%%-define(T_SPEC, spec).
%%-define(T_MBOX, mbox).

