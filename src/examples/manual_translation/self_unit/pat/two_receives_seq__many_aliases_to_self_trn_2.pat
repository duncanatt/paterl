#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).
#### File: two_receives_seq__many_aliases_to_self.typ
#### Result: Client-side translation succeeds with translation 2.

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

## Typed Erlang (server side).

#def server(mb0: ServerMb?): (Unit * ServerMb?) { ### THIS NEEDS LOOKING INTO!!
def server(mb0: ServerMb?): Unit {
  guard mb0: *Add . *Mul {
    receive Add(client, a, b) from mb1 ->
      let (x0, mb2) =
        (client ! Ans(a + b), mb1)
      in
        x0;
        server(mb2)
      receive Mul(client, a, b) from mb1 ->
        let (x0, mb2) =
          (client ! Ans(a * b), mb1)
        in
          x0;
          server(mb2)
      free -> () # Free inserted manually.
  }
}

## Typed Erlang (client side).

def main0(): Int {
  let (x0, mb0) =
    (let mb1 =
      new [ClientMb]
    in
      main(mb1))
  in
    let x1 =
      free(mb0)
    in
      x1;
      x0
}

def main(mb0: ClientMb?): (Int * ClientMb?) {

    let (serverPid, x0) =                                                      # L[]
      (let mb1 =
        new [ServerMb]
      in
        let x1 =
          spawn { server(mb1) }
        in
          (mb1, x1))
    in                                                                          # L[]
      x0;
      let (x2, self0) =                                                         # L[]
        ((), mb0)
      in                                                                        # L[self0]
        x2;
        let (x3, self1) =                                                       # L[self0]
          ((), self0)
        in                                                                      # L[self0, self1]
          x3;
          let (x4, mb2) =                                                       # L[self0, self1]
            (serverPid ! Add(self1, 1, 2), self1)
          in                                                                    # L[self0, self1]
            x4;
            let (x5, mb3) =
              (serverPid ! Mul(mb2, 3, 4), mb2)
            in
              x5;
              let (a, mb4) =                                                    # L[self0, self1]
                guard mb3: Ans. Ans {
                  receive Ans(a) from mb5 ->
                    (a, mb5)
                }
              in                                                                # L[self0, self1]
                let (b, mb6) =                                                  # L[self0, self1]
                  guard mb4: Ans {
                    receive Ans(b) from mb7 ->
                      (b, mb7)
                  }
                in                                                              # L[self0, self1]
                  (a + b, mb6)
}



