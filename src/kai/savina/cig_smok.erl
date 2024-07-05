%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 14:12
%%%-------------------------------------------------------------------
-module(cig_smok).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([arbiter/1, smoker/1]).

%% Mailbox interface-function associations.
-new({arbiter_mb, [arbiter/1]}).
-use({arbiter_mb, [arbiter_loop/4]}).
-new({smoker_mb, [smoker/1]}).
-use({smoker_mb, [smoker_exit/0]}).
-new({main_mb, [main/0]}).

%% ArbiterMb's message types
-type start() :: {start}.
-type started_smoking() :: {started_smoking}.

%% SmokerMb's message types
-type start_smoking() :: {start_smoking, integer()}.
-type exit() :: {exit}.

%% Interface ArbiterMb {
%%    Start(),
%%    StartedSmoking()
%% }
%%
%% Interface SmokerMb {
%%    StartSmoking(Int),
%%    Exit()
%% }
-type arbiter_mb() :: pid() | start() | started_smoking().
-type smoker_mb() :: pid() | start_smoking() | exit().
-type main_mb() :: pid().

%%  def arbiter(self: ArbiterMb?, numRounds: Int): Unit {
%%    let smokerMb1 = new [SmokerMb] in
%%    spawn { smoker(smokerMb1, self) };
%%
%%    let smokerMb2 = new [SmokerMb] in
%%    spawn { smoker(smokerMb2, self) };
%%
%%    let smokerMb3 = new [SmokerMb] in
%%    spawn { smoker(smokerMb3, self) };
%%
%%    guard self: Start . (*StartedSmoking) {
%%      receive Start() from self ->
%%        notify_smoker(smokerMb1, smokerMb2, smokerMb3);
%%        arbiter_loop(self, numRounds, smokerMb1, smokerMb2, smokerMb3)
%%    }
%%  }
-spec arbiter(integer()) -> no_return().
arbiter(Num_rounds) ->
  Self = self(),
  ?mb_new(smoker_mb),
  Smoker1 = spawn(?MODULE, smoker, [Self]),
  ?mb_new(smoker_mb),
  Smoker2 = spawn(?MODULE, smoker, [Self]),
  ?mb_new(smoker_mb),
  Smoker3 = spawn(?MODULE, smoker, [Self]),

  ?mb_assert_regex("Start . (*StartedSmoking)"),
  receive
    {start} ->
      notify_smoker(Smoker1, Smoker2, Smoker3),
      arbiter_loop(Num_rounds, Smoker1, Smoker2, Smoker3)
  end.

%%  def notify_smoker(smokerMb1: SmokerMb!, smokerMb2: SmokerMb!, smokerMb3: SmokerMb!): Unit {
%%    let smokerId = rand(2) in
%%    let sleepTimeMs = 1000 in
%%
%%    if (smokerId == 0) {
%%      smokerMb1 ! StartSmoking(rand(sleepTimeMs))
%%    }
%%    else {
%%      if (smokerId == 1) {
%%        smokerMb2 ! StartSmoking(rand(sleepTimeMs))
%%      }
%%      else {
%%        smokerMb3 ! StartSmoking(rand(sleepTimeMs))
%%      }
%%    }
%%  }
-spec notify_smoker(smoker_mb(), smoker_mb(), smoker_mb()) -> no_return().
notify_smoker(Smoker1, Smoker2, Smoker3) ->
  Smoker_id = rand:uniform(3),
%%  TODO if-else clause doesn't work
  Sleep_time_ms = 1000,
  if Smoker_id == 1 ->
    Smoker1 ! {start_smoking, rand:uniform(Sleep_time_ms)};
    Smoker_id == 2 ->
      Smoker2 ! {start_smoking, rand:uniform(Sleep_time_ms)};
    true ->
      Smoker3 ! {start_smoking, rand:uniform(Sleep_time_ms)}
  end.


%%  def notify_smoker_exit(smokerMb1: SmokerMb!, smokerMb2: SmokerMb!, smokerMb3: SmokerMb!): Unit {
%%    smokerMb1 ! Exit();
%%    smokerMb2 ! Exit();
%%    smokerMb3 ! Exit()
%%  }
-spec notify_smoker_exit(smoker_mb(), smoker_mb(), smoker_mb()) -> no_return().
notify_smoker_exit(Smoker1, Smoker2, Smoker3) ->
  Smoker1 ! {exit},
  Smoker2 ! {exit},
  Smoker3 ! {exit}.

%%  def arbiter_loop(self: ArbiterMb?, numRounds:Int, smokerMb1: SmokerMb!, smokerMb2: SmokerMb!, smokerMb3: SmokerMb!): Unit {
%%    guard self: *StartedSmoking {
%%      free ->
%%        ()
%%      receive StartedSmoking() from self ->
%%        if (numRounds <= 0) {
%%          notify_smoker_exit(smokerMb1, smokerMb2, smokerMb3)
%%        }
%%        else {
%%          notify_smoker(smokerMb1, smokerMb2, smokerMb3)
%%        };
%%        arbiter_loop(self, numRounds - 1, smokerMb1, smokerMb2, smokerMb3)
%%    }
%%  }
-spec arbiter_loop(integer(), smoker_mb(), smoker_mb(), smoker_mb()) -> no_return().
arbiter_loop(Num_rounds, Smoker1, Smoker2, Smoker3) ->
  ?mb_assert_regex("*StartedSmoking"),
  receive
    {started_smoking} ->
%%      TODO if-else clause doesn't work
      if Num_rounds =< 0 ->
        notify_smoker_exit(Smoker1, Smoker2, Smoker3);
        true ->
          notify_smoker(Smoker1, Smoker2, Smoker3)
      end,
      arbiter_loop(Num_rounds - 1, Smoker1, Smoker2, Smoker3)
  end.

%%  def smoker(self: SmokerMb?, arbiterMb: ArbiterMb!): Unit {
%%    guard self: (*StartSmoking) . (*Exit) {
%%      free ->
%%        ()
%%      receive StartSmoking(ms) from self ->
%%        arbiterMb ! StartedSmoking();
%%        sleep(ms);
%%        smoker(self, arbiterMb)
%%      receive Exit() from self ->
%%        smoker_exit(self)
%%    }
%%  }
-spec smoker(arbiter_mb()) -> no_return().
smoker(ArbiterMb) ->
  ?mb_assert_regex("(*StartSmoking) . (*Exit)"),
  receive
    {start_smoking, Ms} ->
      ArbiterMb ! {started_smoking},
%%      TODO: Sleep doesn't work
%%      timer:sleep(Ms),
      smoker(ArbiterMb);
    {exit} ->
      smoker_exit()
  end.

%%  def smoker_exit(self: SmokerMb?): Unit {
%%    guard self: (*StartSmoking) . (*Exit) {
%%      free -> ()
%%      receive StartSmoking(ms) from self ->
%%        smoker_exit(self)
%%      receive Exit() from self ->
%%        smoker_exit(self)
%%    }
%%  }
-spec smoker_exit() -> no_return().
smoker_exit() ->
  ?mb_assert_regex("(*StartSmoking) . (*Exit)"),
  receive
    {start_smoking, Ms} ->
      smoker_exit();
    {exit} ->
      smoker_exit()
  end.

%%  def main(): Unit {
%%    let arbiterMb = new [ArbiterMb] in
%%    spawn { arbiter(arbiterMb, 10) };
%%    arbiterMb ! Start()
%%  }
-spec main() -> no_return().
main() ->
  ?mb_new(arbiter_mb),
  ArbiterMb = spawn(?MODULE, arbiter, [10]),
  ArbiterMb ! {start}.
