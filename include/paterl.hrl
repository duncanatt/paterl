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
