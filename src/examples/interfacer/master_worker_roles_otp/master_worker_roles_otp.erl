%%%-------------------------------------------------------------------
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
-module(master_worker_roles_otp).

-import(io, [format/2]).

-export([client/2, main/0]).

%% @doc Client issuing one numerical task to the master.
-spec client(integer(), master_worker_master_otp:master_if()) -> ok.
client(N, Master) ->
  {result, Result} = gen_server:call(Master, {task, N}),
  format("Result from master: ~b.~n", [Result]).

%% @doc Launcher.
-spec main() -> ok.
main() ->
  {ok, Master} = master_worker_master_otp:start_link(),
  client(5, Master).

% erlc -o ebin src/examples/interfacer/master_worker_roles_otp/*.erl
% erl -pa ebin -noshell -eval 'master_worker_roles_otp:main().' -s init stop
%
% From repo root:
%   Eqwalizer/Dialyzer/Typer provide ordinary Erlang type information;
%   Interfacer performs the process-interface checks.
%   elp eqwalize master_worker_roles_otp
%   elp eqwalize master_worker_master_otp
%   elp eqwalize master_worker_pool_otp
%   elp eqwalize master_worker_worker_otp
% One-time Dialyzer PLT setup:
%   dialyzer --build_plt --apps erts kernel stdlib --output_plt .dialyzer_plt
% Check these files with Dialyzer:
%   dialyzer --src --plt .dialyzer_plt src/examples/interfacer/master_worker_roles_otp/*.erl
% Show inferred function specs with Typer:
%   typer --show --plt .dialyzer_plt src/examples/interfacer/master_worker_roles_otp/*.erl
