#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).
#### File: two_receives_seq_many_aliases_to_self_let_fun.typ
#### Result: Client-side translation succeeds with translation 3.

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

## Typed Erlang (server side).

# def server(mb0: ServerMb?): (Unit * ServerMb?) { # Will see how to solve this!
def server(mb0: ServerMb?): Unit {
  guard mb0: *Add . *Mul {                                                      # MB = mb0, L = {}
    receive Add(client, a, b) from mb1 ->                                       # MB = mb0, L = {}
      let (x0, mb2) =
        (client ! Ans(a + b), mb1)                                              # MB = mb1, L = {}
      in
        x0;
        server(mb2)                                                             # MB = mb2, L = {}
    receive Mul(client, a, b) from mb1 ->                                       # MB = mb0, L = {}
      let (x0, mb2) =
        (client ! Ans(a * b), mb1)                                              # MB = mb1, L = {}
      in
        x0;
        server(mb2)                                                             # MB = mb2, L = {}
    free -> () # Inserted manually for now.
  }
}

## Typed Erlang (client side).

def main0(): Int {
  let (x0, mb1) =
    (let mb0 = new [ClientMb] in main(mb0))
  in
    let x1 =
      free(mb1)
    in
      x1;
      x0
}

def main(mb0: ClientMb?): (Int * ClientMb?) {
    let (serverPid, x0) =
      (let mb1 =                                                                # MB = mb0, L = {}
        new [ServerMb]
      in
        let x1 =                                                                # MB = mb0, L = {}
          spawn { server(mb1) }                                                 # MB = mb0, L = {}
        in
          (mb1, x1))
    in
      x0;
      let (x2, self0) =
        ((), mb0)                                                               # MB = mb0, L = {}
      in
        x2;
        let (x3, self1) =
          (let (x4, self2) =                                                    # MB = self0, L = {self0}
            get_self(self0)                                                     # MB = self0, L = {self0}
          in
            (x4, self2)) # x4 added manually to circumvent Pat aliasing issue.  # MB = self2, L = {self0, self2}
        in
          x3;                                                                   # MB = self1, L = {self0, self1}
          let (x5, mb2) =
            (serverPid ! Add(self1, 1, 2), self1)                               # MB = self1, L = {self0, self1}
          in
            x5;                                                                 # MB = mb2, L = {self0, self1}
            let (x6, mb3) =
              (serverPid ! Mul(mb2, 3, 4), mb2)                                 # MB = mb2, L = {self0, self1}
            in
              x6;                                                               # MB = mb3, L = {self0, self1}
              let (a, mb4) =
                guard mb3: Ans . Ans {                                          # MB = mb3, L = {self0, self1}
                  receive Ans(a) from mb5 ->
                    (a, mb5)                                                    # MB = mb5, L = {self0, self1}
                }
              in
                let (b, mb6) =
                  guard mb4: Ans {                                              # MB = mb4, L = {self0, self1}
                    receive Ans(b) from mb7 ->
                      (b, mb7)                                                  # MB = mb7, L = {self0, self1}
                  }
                in
                  (add(a, b), mb6)                                              # MB = mb6, L = {self0, self1}

}

def get_self(mb0: ClientMb?): (Unit * ClientMb?) { # Had to be modified to circumvent Pat aliasing issue.
  ((), mb0)                                                                     # MB = mb0, L = {}
}

def add(a: Int, b: Int): Int {
  a + b
}