%%%-------------------------------------------------------------------
%%% ENCODED form: see include/interfacer.hrl. pid(I) is written pid_of(I).
%%% This module declares no interface, so nothing is hoisted to the top.
%%% Read ../master_worker_roles_otp.erl for the proposed notation.
%%%
%%% One-module-per-role OTP Interfacer version of master_worker.
%%%
%%% Master-worker set-up. This module is the launcher and client; the master,
%%% pool and worker roles are separate gen_server callback modules, and the
%%% message types they share are declared in master_worker_msgs_otp.
%%%
%%% Why one module per role: In master_worker_otp.erl a single callback module
%%% implements all three roles, so start_master/0 and start_pool/0 both call
%%% gen_server:start_link(?MODULE, ...) while claiming different return types.
%%% Only the init argument distinguishes them, which no checker can see, so
%%% those endpoint types are asserted rather than checked. Splitting the roles
%%% makes each start_link/0 return exactly one kind of process, so the declared
%%% endpoint type is established by construction.
%%%
%%% All communication uses gen_server:call/2. Replies are ordinary return
%%% values, so no role needs a client-side mailbox interface.
%%%-------------------------------------------------------------------
-module(master_worker_roles_otp_encoded).

-include("interfacer.hrl").
-import(io, [format/2]).

-export([client/2, main/0]).

%% @doc Client issuing one numerical task to the master.
-spec client(integer(), pid_of(master_worker_master_otp_encoded:master_in())) -> ok.
client(N, Master) ->
  {result, Result} = gen_server:call(Master, {task, N}),
  format("Result from master: ~b.~n", [Result]).

%% @doc Launcher.
-spec main() -> ok.
main() ->
  {ok, Master} = master_worker_master_otp_encoded:start_link(),
  client(5, Master).

%% Encoded form of ../master_worker_roles_otp.erl. Build and run everything with
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
%%   elp eqwalize master_worker_roles_otp_encoded
%% One-time Dialyzer PLT setup:
%%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
%% Check this file with Dialyzer:
%%   dialyzer --src --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_roles_otp_encoded.erl
%% Show inferred function specs with Typer:
%%   typer --show --plt .dialyzer_plt -I include src/examples/interfacer/encoded/master_worker_roles_otp_encoded.erl
