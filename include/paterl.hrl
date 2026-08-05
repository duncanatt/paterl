%%
%% %CopyrightBegin%
%%
%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-author("duncan").

%% Views the pid returned by self through the specified mailbox interface.
%% The annotation is used to decorate self expressions.
-define(as(MbName), {'@as', MbName}).

%% Asserts the state of the mailbox in terms of the messages it can contain.
%% The annotation is used to decorate receive expressions.
%%
%% Must be used when the mailbox interface scope consists of one or more
%% mailbox interface names.
-define(expects(MbName, Pattern), {'@expects', MbName, Pattern}).

%% Asserts the state of the mailbox in terms of the messages it can contain.
%% The annotation is used to decorate receive expressions.
%%
%% Can be used when the mailbox interface scope consists of one mailbox
%% interface name. The mailbox interface name is inferred from the enclosing
%% scope.
-define(expects(Pattern), {'@expects', Pattern}).

%% Asserts the state of the mailbox as ?expects does, and additionally exempts
%% the receive expression from the Pat alias check.
%%
%% Pat requires that no two distinct variables represent the same mailbox, on
%% which it relies to reason that syntactically distinct variables refer to
%% distinct mailboxes. Communication can violate the requirement, since a
%% received reference may denote the mailbox that a reference already in scope
%% denotes. Pat cannot detect this, and instead rejects a receive expression
%% that binds a mailbox reference while another reference of the same mailbox
%% interface is in scope. Should the two coincide, the one mailbox is typed as
%% though it were two, and the communication errors that mailbox typing rules
%% out, among them use-after-free and self-deadlock, are no longer detected in
%% the code in question.
%%
%% Annotating a receive expression asserts that the reference it receives and
%% those already in scope necessarily denote distinct mailboxes. Pat does not
%% check the assertion.
%% Must be used when the mailbox interface scope consists of one or more
%% mailbox interface names.
-define(expects_unsafe(MbName, Pattern), {'@expects_unsafe', MbName, Pattern}).

%% Asserts the state of the mailbox and exempts the receive expression from the
%% Pat alias check, as ?expects_unsafe/2 does.
%%
%% Can be used when the mailbox interface scope consists of one mailbox
%% interface name. The mailbox interface name is inferred from the enclosing
%% scope.
-define(expects_unsafe(Pattern), {'@expects_unsafe', Pattern}).
