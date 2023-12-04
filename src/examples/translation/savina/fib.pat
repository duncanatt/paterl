## Fib interface.
## @type fib_mb() :: {req, fib_mb(), integer(()} | {resp, integer()} (reference to mailbox is always ! because ? cannot be delegated)
interface FibMb { Req(FibMb!, Int) , Resp(Int) }

### API.

## %% @spec fib() -> none()
## %% @new fib_mb()
## fib() ->
##   %% @mb fib_mb()
##   %% @assert req
##   receive
##     {req, ReplyTo, N} ->
##       Term =
##         if N =< 2 ->
##           1;
##         true ->
##           FibPid1 =
##             %% @new fib_mb()
##             spawn(?MODULE, fib, []),
##           FibPid2 =
##             %% @new fib_mb()
##             spawn(?MODULE, fib, []),
##
##           Self =
##             %% @mb fib_mb()
##             self(),
##           FibPid1 ! {req, Self, N - 1},
##           FibPid2 ! {req, Self, N - 2},
##
##           %% @mb fib_mb()
##           %% @assert resp.resp
##           receive
##             {resp, Term1} ->
##               %% @mb fib_mb()
##               %% @assert resp
##               receive
##                 {resp, Term2} ->
##                   Term1 + Term2
##               end
##           end
##         end,
##       ReplyTo ! {resp, Term}
##   end.


def fib(mb0: FibMb?): (Unit * FibMb?) {
  guard mb0: Req {
    receive Req(replyTo, n) from mb1 ->
      let (term, mb2) =
          if (n <= 2) {
            (1, mb1)
          }
          else {
            let (fibPid1, mb2) =
              (let mb3 =
                new [FibMb]
              in
                let y0 =
                  spawn { let (x0, mb4) = fib(mb3) in free(mb4); x0 }
                in
                  mb3
              , mb1)
            in
              let (fibPid2, mb7) =
                (let mb5 =
                  new [FibMb]
                in
                  let y1 =
                    spawn { let (x1, mb6) = fib(mb5) in free(mb6); x1 }
                  in
                    mb5
                , mb2)
              in
                let (self, mb8) =
                  (mb7, mb7)
                in
                  let (z0, mb9) =
                    (fibPid1 ! Req(self, n - 1), mb8)
                  in
                    z0;
                    let (z1, mb10) =
                      (fibPid2 ! Req(self, n - 2), mb9)
                    in
                      z1;
                      guard mb10: Resp.Resp {
                        receive Resp(term1) from mb11 ->
                          guard mb11: Resp {
                          receive Resp(term2) from mb12 ->
                              (term1 + term2, mb12)
                          }
                      }
          }
        in
          (replyTo ! Resp(term), mb2)
  }
}

## %% @spec main() -> none()
## %% @new fib_mb()
## main() ->
##   FibPid1 =
##     %% @new fib_mb()
##     spawn(?MODULE, fib, []),
##
##   Self =
##     %% @mb fib_mb()
##     self(),
##   FibPid1 ! {req, Self, 16},
##
##   %% @mb fib_mb()
##   %% @assert Resp
##   receive
##     {resp, Term} ->
##       format("Result: ~p~n", [Term])
##   end.

def main(mb0: FibMb?): (Unit * FibMb?) {
  let (fibPid1, mb1) =
    (let mb2 =
      new [FibMb]
    in
      let y0 =
        spawn { let (x0, mb3) = fib(mb2) in free(mb3); x0}
      in
        mb2
    , mb0)
  in
    let (self, mb4) =
      (mb1, mb1)
    in
      let (z0, mb5) =
        (fibPid1 ! Req(self, 16), mb4)
      in
        z0;
        guard mb5: Resp {
          receive Resp(term) from mb6 ->
            (print(concat("Result: ", intToString(term))) , mb6)
        }
}

def main0(): Unit {
  let mb0 =
    new [FibMb]
  in
    let (x0, mb1) =
      main(mb0)
    in
      let y0 =
        free(mb1)
      in
        x0
}