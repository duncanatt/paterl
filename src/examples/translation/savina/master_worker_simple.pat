
## Master interface.
## @type master_mb() :: {task, client_mb(), integer()}
interface MasterMb { Task(ClientMb!, Int) }

## Pool interface.
## @type pool_mb() :: {result, integer() }
interface PoolMb { Result(Int) }

## Worker interface.
## @type worker_mb() :: {work, pool_mb(), integer()}
interface WorkerMb { Work(PoolMb!, Int) }

## Client interface.
## @type client_mb() :: {result, integer()}
interface ClientMb { Result(Int) }

## %% @spec master() -> none()
## %% @new master_mb()
## master() ->
##   %% @mb master_mb()
##   %% @assert task*
##   receive
##     {task, ReplyTo, Task} ->
##       format("= Master received task '~p' from client ~n", [Task]),
##
##       Result =
##         %% @new pool_mb()
##         farm_and_harvest(Task),
##       ReplyTo ! {result, Result},
##
##       %% @use master_mb()
##       master()
##   end.


def master(mb0: MasterMb?): (Unit * MasterMb?) {
  guard mb0: *Task {
  receive Task(replyTo, task) from mb1 ->
      #let (result, mb2) =
      #  (let mb3 =
      #    new [PoolMb]
      #  in
      #    let (x0, mb4) =
      #      farm_and_harvest(mb3, task)
      #    in
      #      let y0 =
      #        free(mb4)
      #      in
      #        x0
      #  , mb1)
      #in
      #  let (z0, mb5) =
      #    (replyTo ! Result(result), mb2)
      #  in
      #    master(mb5)

      let (z0, mb1) =
        (replyTo ! Result(0), mb0)
      in
        z0;
        master(mb1)
  }
}

## %% @spec farm(integer()) -> integer()
## %% @new pool_mb()
## farm_and_harvest(Chunks) ->
##   %% @use pool_mb()
##   _Dummy = farm(Chunks),
##
##   %% @use pool_mb()
##   harvest(Chunks, 0).

def farm_and_harvest(mb0: PoolMb?, chunks: Int): (Int * PoolMb?) {
  #let (dummy, mb1) =
  #  farm(mb0, chunks)
  #in
  #  harvest(mb1, chunks, 0)
  (0, mb0)
}

## %% @spec farm(integer()) -> integer()
## %% @use pool_mb()
## farm(Chunk) ->
##   if Chunk == 0 ->
##     format("= Farming complete~n", []),
##     Chunk;
##     true ->
##       format("= Farming chunk ~p~n", [Chunk]),
##       WorkerPid =
##         %% @new worker_mb()
##         spawn(?MODULE, worker, []),
##
##       Self =
##         %% @mb pool_mb()
##         self(),
##       WorkerPid ! {work, Self, Chunk},
##       Next = Chunk - 1,
##
##       %% @use pool_mb()
##       farm(Next)
##   end.

def farm(mb0: PoolMb?, chunk: Int): (Int * PoolMb?) {
  if (chunk == 0) {
    (chunk, mb0)
  }
  else {
    let (workerPid, mb1) =
      (let mb2 =
        new [WorkerMb]
      in
        let y0 =
          spawn { let (x0, mb3) = worker(mb2) in free(mb3); x0}
        in
          mb2
      , mb0)
    in
      let (self, mb4) =
        (mb1, mb1)
      in
        let (z0, mb5) =
          (workerPid ! Work(self, chunk), mb4)
        in
          let (next, mb6) =
            (chunk - 1, mb5)
          in
            farm(mb6, next)
  }
}

## %% @spec harvest(integer(), integer()) -> integer()
## %% @use pool_mb()
## harvest(Chunk, Acc) ->
##   if Chunk == 0 ->
##     format("= Harvesting complete~n", []),
##     Acc;
##     true ->
##       %% @mb pool_mb
##       %% @assert result*
##       receive
##         {result, N} ->
##           format("= Harvested task ~p with result '~p'~n", [Chunk, N]),
##           Next = Chunk - 1,
##
##           %% @use pool_mb()
##           harvest(Next, Acc + N)
##       end
##   end.
def harvest(mb0: PoolMb?, chunk: Int, acc: Int): (Int * PoolMb?) {
  if (chunk == 0) {
    (acc, mb0)
  }
  else {
    guard mb0: *Result {
      receive Result(n) from mb1 ->
        let (next, mb2) =
          (chunk - 1, mb1)
        in
          harvest(mb2, next, acc + n)
      empty(mb1) ->
        (0, mb1)
    }
  }
}

## %% @spec compute(integer()) -> integer()
## compute(N) ->
##   N * N.
def compute(n: Int): Int {
  n * n
}

## %% @spec worker() -> none()
## %% @new worker_mb()
## worker() ->
##   %% @mb worker_mb()
##   %% @assert work
##   receive
##     {work, ReplyTo, Chunk} ->
##       format("= Worker computing chunk ~p~n", [Chunk]),
##       Result = compute(Chunk),
##       ReplyTo ! {result, Result}
##   end.
def worker(mb0: WorkerMb?): (Unit * WorkerMb?) {
  guard mb0: Work {
    receive Work(replyTo, chunk) from mb1 ->
      let (result, mb2) =
        (compute(chunk), mb1)
      in
        (replyTo ! Result(result), mb2)
  }
}

## %% @spec client(integer(), master_mb()) -> none()
## %% @new client_mb()
## client(Task, MasterPid) ->
##   format("= Started client~n", []),
##   Self =
##     %% @mb client_mb()
##     self(),
##   MasterPid ! {task, Self, Task},
##
##   %% @mb client_mb()
##   %% @assert result
##   receive
##     {result, Result} ->
##       format("Result: '~p'~n", [Result])
##   end.
def client(mb0: ClientMb?, task: Int, masterPid: MasterMb!): (Unit * ClientMb?) {
  let (self, mb1) =
    (mb0, mb0)
  in
    let (z0, mb2) =
      (masterPid ! Task(self, task), mb1)
    in
      guard mb2: Result {
        receive Result(result) from mb3 ->
          (print(intToString(result)), mb3)
      }
}

## %% @spec main() -> none()
## %% @new client_mb()
## main() ->
##   MasterPid =
##     %% @new master_mb()
##     spawn(?MODULE, master, []),
##   %% @new client_mb()
##   spawn(?MODULE, client, [5, MasterPid]),
##   %% @new client_mb()
##   spawn(?MODULE, client, [6, MasterPid]).

def main(mb0: ClientMb?): (Unit * ClientMb?) {
  let (masterPid, mb1) =
    (let mb2 =
      new [MasterMb]
    in
      let y0 =
        spawn { let (x0, mb3) = master(mb2) in free(mb3); x0 }
      in
        mb2
    , mb0)
  in
    let (z0, mb4) =
      (let mb5 =
        new [ClientMb]
      in
        let y1 =
          spawn { let (x1, mb6) = client(mb5, 5, masterPid) in free(mb6); x1 }
        in
          mb5
      , mb1)
    in
      ((), mb4)
}


