%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Adapted from Savina/cigsmok.
%%%
%%% A benchmark modelling n smokers and one arbiter that decides which smoker to
%%% allow to smoke. The benchmark is parameterized by the number of smoker
%%% processes. Since the array type is not available in Pat, we fix the number
%%% of account processes to 3.
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(cig_smok_dir_rec).
-author("duncan").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).
-import(rand, [uniform/1]).
-import(timer, [sleep/1]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([arbiter/1, smoker/1]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Arbiter.
-type start() :: {start}.
-type started_smoking() :: {started_smoking}.

%% Smoker.
-type start_smoking() :: {start_smoking, integer()}.
-type exit() :: {exit}.

%%% Interfaces.

%% Arbiter.
-type arbiter_mb() :: pid() | start() | started_smoking().

%% Smoker.
-type smoker_mb() :: pid() | start_smoking() | exit().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Arbiter.
-new({arbiter_mb, [arbiter/1]}).
-use({arbiter_mb, [arbiter_loop/4]}).

%% Smoker.
-new({smoker_mb, [smoker/1]}).
-use({smoker_mb, [smoker_exit/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Arbiter process handling the creation of smokers and launching of main
%% loop.
-spec arbiter(integer()) -> no_return().
arbiter(NumRounds) ->
  Self = self(),
  Smoker1 = spawn(?MODULE, smoker, [Self]),
  Smoker2 = spawn(?MODULE, smoker, [Self]),
  Smoker3 = spawn(?MODULE, smoker, [Self]),

  ?expects("Start . *Started_smoking"),
  receive
    {start} ->
      notify_smoker(Smoker1, Smoker2, Smoker3),
      arbiter_loop(NumRounds, Smoker1, Smoker2, Smoker3)
  end.

%% @doc Randomly chooses the smoker and requests it to smoke.
-spec notify_smoker(smoker_mb(), smoker_mb(), smoker_mb()) -> no_return().
notify_smoker(Smoker1, Smoker2, Smoker3) ->
  SmokerId = uniform(3),
  SleepTimeMs = 1000,

  if SmokerId == 1 ->
    SleepTimeMsSmoker1 = uniform(SleepTimeMs),
    Smoker1 ! {start_smoking, SleepTimeMsSmoker1};
    true ->
      if SmokerId == 2 ->
        SleepTimeMsSmoker2 = uniform(SleepTimeMs),
        Smoker2 ! {start_smoking, SleepTimeMsSmoker2};
        true ->
          SleepTimeMsSmoker3 = uniform(SleepTimeMs),
          Smoker3 ! {start_smoking, SleepTimeMsSmoker3}
      end
  end.

%% @doc Notifies all smokers to terminate.
-spec notify_smoker_exit(smoker_mb(), smoker_mb(), smoker_mb()) -> any().
notify_smoker_exit(Smoker1, Smoker2, Smoker3) ->
  Smoker1 ! {exit},
  Smoker2 ! {exit},
  Smoker3 ! {exit}.

%% @doc Arbiter process main loop issuing start smoking requests and handling started
%% smoking replies.
-spec arbiter_loop(integer(), smoker_mb(), smoker_mb(), smoker_mb()) -> no_return().
arbiter_loop(Num_rounds, Smoker1, Smoker2, Smoker3) ->
  ?expects("*Started_smoking"),
  receive
    {started_smoking} ->
      % The if here introduces the internal choice, which means that on the
      % receiver side I might or might not receive the message. In this case,
      % the smoker might or might nor receive the Exit message, and must either
      % use (Exit + 1) or (*Exit) in its pattern.
      if Num_rounds =< 0 ->
        notify_smoker_exit(Smoker1, Smoker2, Smoker3);
        true ->
          notify_smoker(Smoker1, Smoker2, Smoker3)
      end,

      % Arbiter needs to service all requests before, even if it has sent the
      % Exit messages to smokers. Remember that smokers may still be processing
      % StartSmoking messages, but the arbiter has already issued Exit messages
      % and it still needs to await all the StartedSmoking replies before
      % terminating. This is why we do not have an exit_arbiter flush function.
      arbiter_loop(Num_rounds - 1, Smoker1, Smoker2, Smoker3)
  end.

%% @doc Smoker process main loop handling start smoking requests and issuing
%% started smoking replies to/from the arbiter.
-spec smoker(arbiter_mb()) -> no_return().
smoker(ArbiterMb) ->
  ?expects("*Start_smoking . *Exit"),
  receive
    {start_smoking, Ms} ->
      ArbiterMb ! {started_smoking},
      format("Smoker sleeping for ~b ms.~n", [Ms]),
      sleep(Ms),
      smoker(ArbiterMb);
    {exit} ->
      smoker_exit()
  end.

%% @doc Smoker process exit procedure that flushes potential residual messages.
-spec smoker_exit() -> no_return().
smoker_exit() ->
  ?expects("*Start_smoking . *Exit"),
  receive
    {start_smoking, Ms} ->
      smoker_exit();
    {exit} ->
      smoker_exit()
    after 0 ->
     format("~p exited.~n", [self()])
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  ArbiterMb = spawn(?MODULE, arbiter, [10]),
  ArbiterMb ! {start}.


%% ./src/paterl src/examples/erlang/savina/cig_smok_dir_rec.erl -v all -I include