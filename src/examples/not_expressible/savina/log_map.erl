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

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([computer/1, worker/4, master/2]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Master.
-type start() :: {start}.
-type result() :: {result, integer()}.

%% Worker.
-type next_term() :: {next_term}.
-type get_term() :: {get_term}.
-type result_worker() :: {result_worker, integer()}.
-type stop() :: {stop}.

%% Term.
-type done() :: {done, integer()}.

%% Computer.
-type compute() :: {compute, term_mb(), integer()}.
-type stop_compute() :: {stop_compute}.

%%% Interfaces.

%% Master.
-type master_mb() :: pid() | start() | result().

%% Worker.
-type worker_mb() :: pid() | next_term() | get_term() | result_worker() | stop().

%% Term.
-type term_mb() :: pid() | done().

%% Computer.
-type computer_mb() :: pid() | compute() | stop_compute().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Master.
-new({master_mb, [master/2]}).
-use({master_mb, [master_loop/7]}).

%% Worker.
-new({worker_mb, [worker/4]}).
-use({worker_mb, [worker_loop/4, worker_exit/0]}).

%% Computer.
-new({computer_mb, [computer/1]}).
-use({computer_mb, [computer_loop/1, computer_exit/0]}).

%% Term.
-new({term_mb, [compute_term/2]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

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
master(StartRate, Increment) ->
  Self = self(),

  % Computer 1.
  Rate1 = StartRate + (1 * Increment),
%%  ?mb_new(computer_mb),
  Computer1 = spawn(?MODULE, computer, [Rate1]),

  % Worker 1.
  StartTerm1 = 1 * Increment,
%%  ?mb_new(worker_mb),
  Worker1 = spawn(?MODULE, worker, [1, Self, Computer1, StartTerm1]),

  % Computer 2.
  Rate2 = StartRate + (2 * Increment),
%%  ?mb_new(computer_mb),
  Computer2 = spawn(?MODULE, computer, [Rate2]),

  % Worker 2.
  StartTerm2 = 2 * Increment,
%%  ?mb_new(worker_mb),
  Worker2 = spawn(?MODULE, worker, [2, Self, Computer2, StartTerm2]),

  ?expects("Start . *Result"),
  receive
    {start} ->
      % We should have a loop around this line to send multiple NextTerm
      % messages, according to the number of terms we send to computer. For
      % now, let this be 2 and 3. Later we can refactor it.
      Worker1 ! {next_term},
      Worker1 ! {next_term},
      Worker2 ! {next_term},
      Worker2 ! {next_term},
      Worker2 ! {next_term},

      % Get result from worker as soon as finished. We should have many workers.
      % The number of workers is numWorkers and to each one, we send just one
      % request.
      Worker1 ! {get_term},
      Worker2 ! {get_term},

      % Collect results.
      master_loop(0, Worker1, Computer1, Worker2, Computer2, 5, 0)
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
-spec master_loop(integer(), worker_mb(), computer_mb(), worker_mb(), computer_mb(), integer(), integer()) -> no_return().
master_loop(TermSum, Worker1, Computer1, Worker2, Computer2, NumWorkRequests, NumWorkReceived) ->
  ?expects("*Result"),
  receive
    {result, Term} ->
%%      if (NumWorkRequests >= NumWorkReceived) ->
        %%  TODO: Not sure about free
        % Notify workers and computers.
        Worker1 ! {stop},
        Worker2 ! {stop},
        Computer1 ! {stop_compute},
        Computer2 ! {stop_compute},
%%        format("Result is: ~p~n", [TermSum]);
        format("Result is: ~p~n", [TermSum])
%%        true ->
          % Accumulate computed term.
%%          master_loop(TermSum + Term, Worker1, Computer1, Worker2, Computer2, NumWorkRequests, NumWorkReceived + 1)
%%      end
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
worker(Id, Master, Computer, CurrTerm) ->
  worker_loop(Id, Master, Computer, CurrTerm).

-spec worker_loop(integer(), master_mb(), computer_mb(), integer()) -> no_return().
worker_loop(Id, Master, Computer, CurrTerm) ->
  ?expects("Get_term . *Next_term . Stop"),
  receive
    {next_term} ->
%%      Self = self(),
%%      Computer ! {compute, Self, CurrTerm},
%%      ?mb_assert_regex("Done"),
%%      receive
%%        {done, Term} ->
%%          worker(Id, Master, Computer, Term)
%%      end;
      % Delegate computation of term to computer process via the local mailbox
      % termMb.
%%      Term = compute_term(Computer, CurrTerm),
      worker_loop(Id, Master, Computer, 0);
    {get_term} ->
      Master ! {result, CurrTerm},
      ?expects("Stop . *Next_term"),
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
  ?expects("*Next_term"),
  receive
    {next_term} ->
      worker_exit()
  end.

-spec compute_term(computer_mb(), integer()) -> integer().
compute_term(Computer, CurrTerm) ->
  Self = self(),
  Computer ! {compute, Self, CurrTerm},
  ?expects("Done"),
  receive
    {done, Term} ->
      Term
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
  computer_loop(Rate).

-spec computer_loop(integer()) -> no_return().
computer_loop(Rate) ->
  ?expects("*Compute . Stop_compute"),
  receive
    {compute, Term_mb, Term} ->
      Term_mb ! {done, Rate * Term * (1 - Term)},
      computer_loop(Rate);
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
  ?expects("*Compute"),
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
-spec main() -> any().
main() ->
%%  ?mb_new(master_mb),
  Master = spawn(?MODULE, master, [3, 1]),
  Master ! {start}.
