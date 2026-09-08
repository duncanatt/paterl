%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. This module needs no encoding and
%%% does not include the header: it declares no interface and holds no
%%% pid-valued type. Read ../master_worker_msgs_otp.erl for the proposed
%%% notation.
%%%
%%% Shared message types for the one-module-per-role OTP master_worker
%%% example.
%%%
%%% The three role modules exchange the same four messages. They are declared
%%% once here and referred to remotely, as
%%% master_worker_msgs_otp_encoded:result().
%%% Interfacer loads this module's type table to check a payload against a
%%% type named in another module.
%%%-------------------------------------------------------------------
-module(master_worker_msgs_otp_encoded).

-export_type([task/0, run/0, work/0, result/0]).

-type task()   :: {task, integer()}.
-type run()    :: {run, integer()}.
-type work()   :: {work, integer()}.
-type result() :: {result, integer()}.

%% Encoded form of ../master_worker_msgs_otp.erl. Build and run everything with
%%   ./run-interfacer-examples.sh
%%
%% Or, from the repo root, this example alone. It is spread over five
%% modules and driven by master_worker_roles_otp_encoded, so compile the
%% whole directory:
%%   erlc -I include -o ebin-interfacer src/examples/interfacer/encoded/*.erl
%%   erl -pa ebin-interfacer -noshell \
%%     -eval 'master_worker_roles_otp_encoded:main(), timer:sleep(500), init:stop().'
%%
%% Eqwalizer, Dialyzer and TypEr supply ordinary Erlang type information;
%% Interfacer performs the process-interface checks.
%%   elp eqwalize master_worker_msgs_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_msgs_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_msgs_otp_encoded.erl
