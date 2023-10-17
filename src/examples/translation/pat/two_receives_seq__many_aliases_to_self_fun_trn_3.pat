#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).
#### File: two_receives_seq__many_aliases_to_self_fun.typ
#### Result: Client-side translation succeeds with translation 3.

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

## Typed Erlang (server side).

def server(mb0: ServerMb?): Unit {
#def server(mb0: ServerMb?): (Unit * ServerMb?) {                                # MB = -, L = {}
  guard mb0: *Add . *Mul {                                                      # MB = mb0, L = {}
    receive Add(client, a, b) from mb1 ->                                       # MB = mb0, L = {}
      let (x0, mb2) =                                                           # MB = mb1, L = {}
        (client ! Ans(a + b), mb1)                                              # MB = mb1, L = {}
      in
        x0;
        server(mb2)                                                             # MB = mb2, L = {}
    receive Mul(client, a, b) from mb1 ->                                       # MB = mb0, L = {}
      let (x0, mb2) =                                                           # MB = mb1, L = {}
        (client ! Ans(a * b), mb1)                                              # MB = mb1, L = {}
      in
        x0;
        server(mb2)                                                             # MB = mb2, L = {}
    free -> () # Free inserted manually.
  }
}

## Typed Erlang (client side).

def main0(): Int {                                                              # MB = -, L = {}
  let (x0, mb0) =
    (let mb1 =                                                                  # MB = -, L = {}
      new [ClientMb]                                                            # MB = -, L = {}
    in
      main(mb1))                                                                # MB = -, L = {}
  in
    let x1 =
      free(mb0)                                                                 # MB = mb0, L = {}
    in
      x1;
      x0
}

def main(mb0: ClientMb?): (Int * ClientMb?) {                                   # MB = -, L = {}
  let (serverPid, x0) =                                                         # MB = mb0, L = {}
    (let mb1 =
      new [ServerMb]                                                            # MB = mb0, L = {}
    in
      let x1 =
        spawn { server(mb1) }                                                   # MB = mb0, L = {}
      in
        (mb1, x1))                                                              # MB = mb0, L = {}
  in
    x0;                                                                         # MB = mb0, L = {}
    let (x2, self0) =
      ((), mb0)                                                                 # MB = mb0, L = {}
    in
      x2;
      let (x3, self1) =                                                         # MB = self0, L = {self0}
        get_self(self0)
      in
        x3;
        let (x4, mb2) =                                                         # MB = self1, L = {self0, self1}
          (serverPid ! Add(self1, 1, 2), self1)
        in                                                                      # MB = mb2, L = {self0, self1}
          x4;
          let (x5, mb3) =
            (serverPid ! Mul(mb2, 3, 4), mb2)
          in                                                                    # MB = mb3, L = {self0, self1}
            x5;
            let (a, mb4) =
              guard mb3: Ans.Ans {                                              # MB = mb3, L = {self0, self1}
              receive Ans(a) from mb5 ->                                        # MB = mb5, L = {self0, self1}
                (a, mb5)
              }
            in                                                                  # MB = mb4, L = {self0, self1}
              let (b, mb6) =
                guard mb4: Ans {
                  receive Ans(b) from mb7 ->                                    # MB = mb7, L = {self0, self1}
                    (b, mb7)
                }
              in                                                                # MB = mb6, L = {self0, self1}
                (a + b, mb6)
}

def get_self(mb0: ClientMb?): (Unit * ClientMb?) {
  ((), mb0)
}
