%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 6月 2024 18:47
%%%-------------------------------------------------------------------
-module(log_map).
-author("walker").

-include("paterl.hrl").

-import(io, [format/2]).

%% API
-export([main/0]).

-export([computer/1, worker/4, master/2]).

%% Mailbox interface-function associations.
-new({master_mb, [master/2]}).
-use({master_mb, [master_loop/5]}).
-new({worker_mb, [worker/4]}).
-use({worker_mb, [worker_exit/0]}).
-new({computer_mb, [computer/1]}).
-use({computer_mb, [computer_exit/0]}).
-new({main_mb, [main/0]}).

%% MasterMb's message types
-type start() :: {start}.
-type result() :: {result, integer()}.

%% WorkerMb's message types
-type next_term() :: {next_term}.
-type get_term() :: {get_term}.
-type result_worker() :: {result_worker, integer()}.
-type stop() :: {stop}.

%% TermMb's message types
-type done() :: {done, integer()}.

%% ComputerMb's message types
-type compute() :: {compute, term_mb(), integer()}.
-type stop_compute() :: {stop_compute}.

%% Interface MasterMb {
%%    Start(),
%%    Result(Int)
%% }
%%
%% Interface WorkerMb {
%%    NextTerm(),
%%    GetTerm(),
%%    ResultWorker(Int),
%%    Stop()
%% }
%%
%% Interface TermMb {
%%    Done(Int)
%% }
%%
%% Interface ComputerMb {
%%    Compute(TermMb!, Int),
%%    StopCompute()
%% }
-type master_mb() :: pid() | start() | result().
-type worker_mb() :: pid() | next_term() | get_term() | result_worker() | stop().
-type term_mb() :: pid() | done().
-type computer_mb() :: pid() | compute() | stop_compute().
-type main_mb() :: pid().

%%  def master(self: MasterMb?, startRate: Int, increment: Int): Unit {
%%
%%  let computerMb1 = new [ComputerMb] in
%%  let rate1 = startRate + (1 * increment) in
%%  spawn { computer(computerMb1, rate1) };
%%
%%  let workerMb1 = new [WorkerMb] in
%%  let startTerm1 = 1 * increment in
%%  spawn { worker(workerMb1, 1, self, computerMb1, startTerm1) };
%%
%%  let computerMb2 = new [ComputerMb] in
%%  let rate2 = startRate + (2 * increment) in
%%  spawn { computer(computerMb2, rate2) };
%%
%%  let workerMb2 = new [WorkerMb] in
%%  let startTerm2 = 2 * increment in
%%  spawn { worker(workerMb2, 2, self, computerMb2, startTerm2) };
%%
%%  guard self: Start . (*Result) {
%%    receive Start() from self ->
%%      workerMb1 ! NextTerm();
%%      workerMb1 ! NextTerm();
%%      workerMb2 ! NextTerm();
%%      workerMb2 ! NextTerm();
%%      workerMb2 ! NextTerm();

%%      workerMb1 ! GetTerm();
%%      workerMb2 ! GetTerm();
%%
%%      # Collect results.
%%      master_loop(self, 0, workerMb1, computerMb1, workerMb2, computerMb2)
%%    }
%%  }
-spec master(integer(), integer()) -> no_return().
master(Start_rate, Increment) ->
  Self = self(),

  Rate1 = Start_rate + (1 * Increment),
  ?mb_new(computer_mb),
  Computer1 = spawn(?MODULE, computer, [Rate1]),

  Start_term1 = 1 * Increment,
  ?mb_new(worker_mb),
  Worker1 = spawn(?MODULE, worker, [1, Self, Computer1, Start_term1]),

  Rate2 = Start_rate + (2 * Increment),
  ?mb_new(computer_mb),
  Computer2 = spawn(?MODULE, computer, [Rate2]),

  Start_term2 = 2 * Increment,
  ?mb_new(worker_mb),
  Worker2 = spawn(?MODULE, worker, [2, Self, Computer2, Start_term2]),

  ?mb_assert_regex("Start . (*Result)"),
  receive
    {start} ->
      Worker1 ! {next_term},
      Worker1 ! {next_term},
      Worker2 ! {next_term},
      Worker2 ! {next_term},
      Worker2 ! {next_term},

      Worker1 ! {get_term},
      Worker2 ! {get_term},

      master_loop(0, Worker1, Computer1, Worker2, Computer2)
  end.

%%  def master_loop(self: MasterMb?, termSum: Int, workerMb1: WorkerMb!, computerMb1: ComputerMb!, workerMb2: WorkerMb!, computerMb2: ComputerMb!): Unit {
%%    guard self: *Result {
%%      free ->
%%        workerMb1 ! Stop();
%%        workerMb2 ! Stop();
%%        computerMb1 ! StopCompute();
%%        computerMb2 ! StopCompute();
%%        print(concat("Result is: ", intToString(termSum)))
%%      receive Result(term) from self ->
%%        master_loop(self, termSum + term, workerMb1, computerMb1, workerMb2, computerMb2)
%%    }
%%  }
-spec master_loop(integer(), worker_mb(), computer_mb(), worker_mb(), computer_mb()) -> no_return().
master_loop(Term_sum, Worker1, Computer1, Worker2, Computer2) ->
%%  TODO: Not sure about free
%%  Worker1 ! {stop},
%%  Worker2 ! {stop},
%%  Computer1 ! {stop_compute},
%%  Computer2 ! {stop_compute},
%%  io:format("Result is: ~p~n", [Term_sum])
  ?mb_assert_regex("*Result"),
  receive
    {result, Term} ->
      master_loop(Term_sum + Term, Worker1, Computer1, Worker2, Computer2)
  end.

%% def worker(self: WorkerMb?, id: Int, masterMb: MasterMb!, computerMb: ComputerMb!, currTerm: Int): Unit {
%%  guard self: (*NextTerm) . GetTerm . Stop {
%%    receive NextTerm() from self ->

%%      let termMb = new [TermMb] in
%%      computerMb ! Compute(termMb, currTerm);
%%      guard termMb: Done {
%%        receive Done(term) from termMb ->
%%          free(termMb);
%%          worker(self, id, masterMb, computerMb, term)
%%      }
%%
%%
%%    receive GetTerm() from self ->
%%      masterMb ! Result(currTerm);
%%      guard self: (*NextTerm) . Stop {
%%        receive Stop() from self ->
%%          worker_exit(self)
%%      }
%%    }
%%  }
-spec worker(integer(), master_mb(), computer_mb(), integer()) -> no_return().
worker(Id, Master, Computer, Curr_term) ->
  ?mb_assert_regex("(*NextTerm) . GetTerm . Stop"),
  receive
    {next_term} ->
      Self = self(),
      Computer ! {compute,Self, Curr_term},
      ?mb_assert_regex("Done"),
      receive
        {done, Term} ->
          worker(Id, Master, Computer, Term)
      end;
    {get_term} ->
      Master ! {result, Curr_term},
      ?mb_assert_regex("(*NextTerm) . Stop"),
      receive
        {stop} ->
          worker_exit()
      end
  end.

%%  def worker_exit(self: WorkerMb?): Unit {
%%    guard self: *NextTerm {
%%      free -> ()
%%      receive NextTerm() from self ->
%%        worker_exit(self)
%%    }
%%  }
-spec worker_exit() -> no_return().
worker_exit() ->
  ?mb_assert_regex("*NextTerm"),
  receive
    {next_term} ->
      worker_exit()
  end.

%%  def computer(self: ComputerMb?, rate: Int): Unit {
%%    guard self: (*Compute) . StopCompute {
%%      free -> ()
%%      receive Compute(termMb, term) from self ->
%%
%%        # Compute next term.
%%        termMb ! Done(rate * term * (1 - term));
%%        computer(self, rate)
%%      receive StopCompute() from self ->
%%        computer_exit(self)
%%    }
%%  }
-spec computer(integer()) -> no_return().
computer(Rate) ->
  ?mb_assert_regex("(*Compute) . StopCompute"),
  receive
    {compute, Term_mb, Term} ->
      Term_mb ! {done, Rate * Term * (1 - Term)},
      computer(Rate);
    {stop_compute} ->
      computer_exit()
  end.

%%  def computer_exit(self: ComputerMb?): Unit {
%%    guard self: *Compute {
%%      free -> ()
%%      receive Compute(termMb, term) from self ->
%%
%%        # Send back the same term value so that the final computation on the
%%        # worker is kept fixed.
%%        termMb ! Done(term);
%%        computer_exit(self)
%%    }
%%  }
-spec computer_exit() -> no_return().
computer_exit() ->
  ?mb_assert_regex("*Compute"),
  receive
    {compute, Term_mb, Term} ->
      Term_mb ! {done, Term},
      computer_exit()
  end.

%%  def main(): Unit {
%%    let masterMb = new [MasterMb] in
%%    spawn { master(masterMb, 3, 1) };
%%    masterMb ! Start()
%%  }
-spec main() -> no_return().
main() ->
  ?mb_new(master_mb),
  Master = spawn(?MODULE, master, [3, 1]),
  Master ! {start}.
