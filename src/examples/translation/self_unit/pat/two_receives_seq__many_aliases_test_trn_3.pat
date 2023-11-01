#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).
#### File: two_receives_seq__many_aliases_test.typ
#### Result: Client-side translation succeeds with translation 3???

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

## Typed Erlang (server side).

#def server(mb0: ServerMb?): (Unit * ServerMb?) { # Will see how to solve this!
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
    #free -> ((), mb0) # Inserted manually for now. [PRETYPE] Type mismatch. Expected Unit but got (Unit * ServerMb).
    free -> () # Inserted manually for now. [PRETYPE] Type mismatch. Expected (Unit * ServerMb) but got Unit.
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

    let (serverPid, x0) =                                                       # MB = mb0, L = {}
      (let mb1 = # Because the type of ServerPid =/= ClientMb, which is the type of the only interface decorating this function.
        new [ServerMb]
      in
        let x1 =
          spawn { server(mb1) }
        in
          (mb1, x1))
    in
      x0;                                                                       # MB = mb0, L = {}
      let (self0, mb2) =
        self_unit(mb0) # Because the type of Self0 = ClientMb, which is the type of the only interface decorating this function.
        #(mb0, mb0) # Because the type of Self0 = ClientMb, which is the type of the only interface decorating this function.
      in
        self0;
        let (self1, mb3) =                                                      # MB = mb2, L = {self0}
          self_unit(mb2) # Because the type of Self1 = ClientMb, which is the type of the only interface decorating this function.
          #(mb2, mb2) # Because the type of Self1 = ClientMb, which is the type of the only interface decorating this function.
        in
          self1;
          let (x2, mb4) =                                                       # MB = mb3, L = {self0, self1}
            (serverPid ! Add(mb3, 1, 2), mb3) # Because the type of () =/= ClientMb, which is the type of the only interface decorating this function.
          in
            x2;                                                                 # MB = mb4, L = {self0, self1}
            let (x3, mb5) =
              (serverPid ! Mul(mb4, 3, 4), mb4) # Because the type of () =/= ClientMb, which is the type of the only interface decorating this function.
            in
              x3;                                                               # MB = mb5, L = {self0, self1}
              let (a, mb6) =
                guard mb5: Ans.Ans {                                            # Because the type of Int =/= ClientMb, which is the type of the only interface decorating this function.
                  receive Ans(a) from mb7 ->
                    (a, mb7)                                                    # MB = mb7, L = {self0, self1}
                }
              in
                let (b, mb8) =                                                  # MB = mb6, L = {self0, self1}
                  guard mb6: Ans { # Because the type of Int =/= ClientMb, which is the type of the only interface decorating this function.
                    receive Ans(b) from mb9 ->
                      (b, mb9)                                                  # MB = mb9, L = {self0, self1}
                  }
                in
                  (a + b, mb8)                                                  # MB = mb8, L = {self0, self1}
}

#def self(mb0: ClientMb?): (ClientMb! * ClientMb?) {
#  (mb0, mb0)
#}

def self_unit(mb0: ClientMb?): (Unit * ClientMb?) {
  ((), mb0)
}