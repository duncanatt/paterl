def main(self: ClientMb): Unit {
    let (mb0, v0) =
        let serverPid = new [ServerMb] in
        spawn { server(serverPid) };
        (serverPid, serverPid)
    in
    let (m1, v1) =
        v0 ! Add(self, 1, 2);
        (m1, v0)
    in
    let (m2, v2) =
        v1 ! Mul(self, 1, 2);
        (m2, v1)
    in
    let (m3, v3) =
        guard Lambda(ServerMb): Ans.Ans {
            receive Ans(x) from mb ->
                (mb, x)
        }
    in
    let (m4, v4) =
        guard Lambda(ServerMb): Ans {
            receive Ans(y) from mb ->
                (mb, y)
        }
    in
    e2
}

def main0(): Unit {
    let self = new [ClientMb] in
    main(self);
    free(self) % This is wrong because self is stale.

    It must be:
    let self = new [ClientMb] in
    let (x, self') = main(self);
    free(self) Maybe
}

#### Two receives in sequence.

def main(self: ClientMb!): Int {
    let (mb0, serverPid) =
        let y0 =
            new [ServerMb]
        in
            spawn { let x0 = server(serverPid) in free(serverPid) }; Wrong, server pid is stale          % Maybe gc(serverPid) instead of free(serverPid)?
            (y0, y0)
    in
        let (y1, x1) =
            (serverPid, serverPid ! Add(Lambda(ClientMb), 1, 2))               % Assume Lambda[ClientMb -> self]
        in
            let (y2, x2) =
                (serverPid, serverPid ! Mul(Lambda(ClientMb), 1, 2))           % Assume Lambda[ClientMb -> self]
            in
                let (y3, x) =
                    guard Lambda(ClientMb): Ans.Ans {
                        receive Ans(x) from y3 ->                              % Rebinding: Lambda[ClientMb -> y3]
                            (y3, x)
                    }
                in
                    let (y4, y) =
                        guard Lambda(ClientMb): Ans {
                            receive Ans(y) from y4 ->                          % Rebinding: Lambda[ClientMb -> y4]
                                (y4, y)
                        }
                    in
                        x + y
}

#### Two nested receives.

def f(self0:ClientMb?): Int {
    let (y0, serverPid) =
        let y =
            new [ServerMb]
        in
            spawn { let x0 = server(y) in free(y) };                           % Translation definition needs to account for parameter injection: currently does not.
            (y, y)
    in
        let (y1, x1) =
            (serverPid, serverPid ! Add(Lambda(ClientMb), 1, 2))               % Lambda = [ClientMb=self0]
        in
            let (y2, x2) =
                (serverPid, serverPid, Mul(Lambda(ClientMb), 1, 2))            % Lambda = [ClientMb=self0]
            in
                guard Lambda(ClientMb): Ans.Ans {                              % Lambda = [ClientMb=self0]
                    receive Ans(a) from mb0 ->                                 % Rebinding: Lambda[ClientMb -> mb0]
                        guard Lambda(ClientMb): Ans {
                            receive Ans(b) from mb1 ->                         % Rebinding: Lambda[ClientMb -> mb1]
                                (mb1, a + b)
                        }
                }
}


#### Two receives in sequence (without state-passing translation but using the same variable for shadowing).
#### Result: Should fail (see notes in shared Latex document as to why!)

## Typed Erlang.

-interface ClientMb.
main(): Int ->
  ServerPid = spawn:ServerMb server(),

  ServerPid ! {add, self:ClientMb, 1, 2},
  ServerPid ! {mul, self:ClientMb, 3, 4},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  A + B.

## Pat translation.

def f(y0:ClientMb?): Int {                                                      % Δ[ClientMb=y0]

  let serverPid =
    let y1 =
      new [ServerMb]
    in
      server(y1);                                                               % Δ[ClientMb=y0, ServerMb=y1]
      y1
  in

  let x0 =
    serverPid ! Add(y0, 1, 2)                                                   % Δ[ClientMb=y0]
  in
    let x1 =
      serverPid ! Mul(y0, 3, 4)                                                 % Δ[ClientMb=y0]
    in
      let a =
        guard y0: Ans.Ans {                                                     % Δ[ClientMb=y0]
          receive Ans(a) from y0 ->                                             % Δ[ClientMb=y0] !! We do not update the mapping, but use it instead.
            a
        }
      in
        let b =
          guard y0[must be returnable]: Ans {                                                       % Δ[ClientMb=y0]
            receive Ans(b) from y0 ->                                           % Δ[ClientMb=y0] !! We do not update the mapping, but use it instead.
              b
          }
        in
          a + b                                                                 % Δ[ClientMb=y0]
}



#### Two receives in sequence (without state-passing translation but using the a fresh variable each time).
#### Result: Should fail and is why we need a state-passing translation if we decide to follow this route.

## Typed Erlang.

-interface ClientMb.
main(): Int ->
  ServerPid = spawn:ServerMb server(),

  ServerPid ! {add, self:ClientMb, 1, 2},
  ServerPid ! {mul, self:ClientMb, 3, 4},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  A =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  A + B.

 ## Pat translation.

 def main(y0:ClientMb?): Int {                                                  % Δ[ClientMb=y0]

  let serverPid =
    let y1 =
      new [ServerMb]
    in
      server(y1);                                                               % Δ[ClientMb=y0, ServerMb=y1]
      y1
  in
    let x0 =
      serverPid ! Add(y0, 1, 2)                                                 % Δ[ClientMb=y0]
    in
      let x1 =
        serverPid ! Mul(y0, 3, 4)
      in
        let a =
          guard y0: Ans.Ans {                                                   % Δ[ClientMb=y0]
            receive Ans(a) from y2 ->                                           % Δ[ClientMb=y2]
              a
          }
        in
          let b =
            guard y0: Ans {                                                     % Δ[ClientMb=y0] !! Fails in Pat since the mailbox is stale.
              receive Ans(b) from y3 ->                                         % Δ[ClientMb=y3]
            }
          in
            a + b                                                               % Δ[ClientMb=y0]
 }





#### Two receives in sequence (with state-passing translation but using the a fresh variable each time).
#### Result: Should succeed.

## Typed Erlang.

-main.
-interface ClientMb.
main(): Int ->
  ServerPid = spawn:ServerMb server(),

  ServerPid ! {add, self:ClientMb, 1, 2},
  ServerPid ! {mul, self:ClientMb, 3, 4},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  A + B.

## Pat translation.

def main0():Int {
  let mb0 =
    new [ClientMb]
  in
    let (x0, mb1) =
      main(mb0)
    in
      free(mb1);
      x0
}

def main(mb0: ClientMb?):(Int * ClientMb!) {

  let (serverPid, mb1) =
    (let mb2 =
      new [ServerMb]
    in
      spawn { let (x0, mb3) = server(mb2) in free(mb3) };
      (mb2, mb2))
  in
    let (x1, mb4) =
      (serverPid ! Add(mb0, 1, 2), mb0)
    in
      let (x2, mb5) =
        (serverPid ! Mul(mb4, 3, 4), mb4)
      in
        let (a, mb6) =
          guard mb5: Ans.Ans {
            receive Ans(a) from mb7 ->
              (a, mb7)
          }
        in
          let (b, mb8) =
            guard mb6: Ans {
              receive Ans(b) from mb9 ->
                (b, mb9)
            }
          in
            (a + b, mb8)
}

## Typed Erlang (server side).

-interface(ServerMb).
server(): Unit ->
  assert("Add*.Mul*"),
  receive
    {add, Client, A, B} ->
      Client ! {ans, A + B},
      server();
    {mul, Client, A, B} ->
      Client ! {ans, A * B},
      server();
  end.

## Pat translation.

def server(mb0: ServerMb?): (Unit * ServerMb!) {
  guard mb0: Add*.Mul* {
    receive Add(client, a, b) from mb1 ->
      let (x0, mb2) =
        (client ! Ans(a + b), mb1)
      in
        server(mb2)
    receive Mul(client, a, b) from mb1 ->
      let (x0, mb2) =
        (client ! Ans(a * b), mb1)
      in
        server(mb2)
  }
}



#### Two receives in sequence (with state-passing translation but using the a fresh variable each time).
#### Result: Should succeed.

## Typed Erlang (client side).

-main.
-interface ClientMb.
main(): Int ->
  ServerPid = spawn server:ServerMb^0(),

  Self = self:ClientMb,
  AddA = 1,
  AddB = 2,
  ServerPid ! {add, Self, AddA, AddB},

  MulA = 3,
  MulB = 4,
  ServerPid ! {mul, Self, MulA, MulB},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  A + B.

## Pat translation.


def main0(): Int {
  let mb0 =
    new [ClientMb]
  in
    let (x0, mb1) =
      main(mb0)
    in
      free(mb1);
      x0
}

def main(mb2: ClientMb?): (Int * ClientMb?) {

  let (serverPid, mb3) =
    let mb4 =
      new [ServerMb]
    in
      server(mb4); # Free is unhandled for now.
      (mb4, mb4)
  in
    let (self, mb5) =
      (mb2, mb2)
    in
      let (addA, mb6) =
        (1, mb5)
      in
        let (addB, mb7) =
          (2, mb6)
        in
          let (x1, mb8) =
            (serverPid ! Add(self, addA, addB), mb7)
          in
            let (mulA, mb9) =
              (3, mb8)
            in
              let (mulB, mb10) =
                (4, mb9)
              in
                let (x2, mb11) =
                  (serverPid ! Mul(self, mulA, mulB), mb10)
                in
                  let (a, mb12) =
                    guard mb11: Ans.Ans {
                      receive Ans(a) from mb13 ->
                        (a, mb13)
                    }
                  in
                    let (b, mb14) =
                      guard mb12: Ans {
                        receive Ans(b) from mb15 ->
                          (b, mb15)
                      }
                    in
                      (a + b, mb14)
}


#### Two receives in sequence (with state-passing translation but using the a fresh variable each time).
#### Result: Should succeed with translation 1.

## Typed Erlang (client side).

-main.
-interface ClientMb.
main(): Int ->
  ServerPid = spawn server:ServerMb^o(),

  Self1 = self:ClientMb^o,
  AddA = 1,
  AddB = 2,
  ServerPid ! {add, Self1, AddA, AddB},

  Self2 = self:ClientMb^o,
  MulA = 3,
  MulB = 4,
  ServerPid ! {mul, Self2, MulA, MulB},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  add(A, B).

# Must be templated to be used with other mailboxes!
add(A, B): Int -> A + B.

## Pat translation.

def main0(): Int {
  let (x0, mb0) =
    let mb1 =
      new [ClientMb]
    in
      main(mb1)
  in
    let y =
      free(mb0)
    in
      x
}


def main(mb0: ClientMb?): (Int * ClientMb?) {

    let (serverMb, mb1) =
      (let mb2 =
        new [ServerMb]
      in
        let x0 =
          spawn { server(mb2) }
        in
          (mb2, x0))
    in

      let (self1, mb2) =
        (mb1, mb1)
      in
        let (addA, mb3) =
          (1, mb2)
        in
          let (addB, mb4) =
            (2, mb3)
          in
            let (x1, mb5) =
              (let (x2, mb00) =
                (serverPid, mb4)
              in
                (x2 ! Add(self1, addA, addB), mb4))
            in
              let (self2, mb6) =
                (mb5, mb5)
              in
                let (mulA, mb7) =
                  (3, mb6)
                in
                  let (mulB, mb8) =
                    (4, mb7)
                  in
                    let (x3, mb9) =
                      (let (x4, mb01) =
                        (serverPid, mb8)
                      in
                        (x4 ! Mul(self2, mulA, mulB), mb8))
                    in
                      let (a, mb10) =
                        guard mb9: Ans.Ans {
                          receive Ans(a) from mb10 ->
                            (a, mb10)
                        }
                      in
                        let (b, mb12) =
                          guard mb10: Ans {
                            receive Ans(b) from mb13 ->
                              (b, mb13)
                          }
                        in
                          add(mb12, a, b)
}

def add(mb0: ClientMb?, a: Int, b: Int): (Int * ClientMb?) {
  (a + b, mb0)
}

#### Two receives in sequence (with state-passing translation but using the a fresh variable each time).
#### Result: Should succeed with translation 1.

## Typed Erlang (client side).

-main.
-interface ClientMb.
main(): Int ->
  ServerPid = spawn server:ServerMb^o(),

  Self = self:ClientMb^o,
  AddA = 1,
  AddB = 2,
  ServerPid ! {add, Self, AddA, AddB},

  MulA = 3,
  MulB = 4,
  ServerPid ! {mul, Self, MulA, MulB},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  A + B.

## Pat translation.

def main(mb0: ClientMb?): (Int * ClientMb?) {
  let (serverPid, mb1) =
    (let mb2 =
      new [ServerMb]
    in
      let x0 =
        spawn { server(mb2) }
      in
        (mb2, x0))
  in
    mb1; # Use unit value.
    let (x1, self) =
      ((), mb0) # alias = self to be replaced with the current mailbox
    in
      x1; # Use unit value.
      let (addA, mb3) =
        (1, self)
      in
        let (addB, mb4) =
          (2, mb3)
        in
          let (x2, mb5) =
            (let (x3, mb6) =
              (serverPid, mb4)
            in
              (x3 ! Add(mb6, addA, addB), mb6)) # alias = self replaced with the current mailbox mb6
          in
            x2; # Use unit value.
            let (mulA, mb7) =
              (3, mb5)
            in
              let (mulB, mb8) =
                (4, mb7)
              in
                let (x4, mb9) =
                  (let (x5, mb10) =
                    (serverPid, mb8)
                  in
                    (x5 ! Mul(mb10, mulA, mulB) , mb10)) # alias = self replaced with the current mailbox mb10
                in
                  x4; # Use unit value.
                  let (a, mb11) =
                    guard mb9: Ans.Ans {
                      receive Ans(a) from mb12 ->
                        (a, mb12)
                    }
                  in
                    let (b, mb13) =
                      guard mb11: Ans {
                        receive Ans(b) from mb14 ->
                          (b, mb14)
                      }
                    in
                    (a + b, mb13)
}







#### Two receives in sequence (with state-passing translation and using self as
#### an argument).
#### Result: Should succeed with translation 0.

## Typed Erlang (client side).

-main main:ClientMb^o(): Int ->
  ServerPid = spawn server:ServerMb^o(),

  ServerPid ! {add, self:ClientMb^o, 1, 2},
  ServerPid ! {mul, self:ClientMb^o, 3, 4},

  A =
  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive
    {ans, B} -> B
  end,

  A + B.

## Pat translation.

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
  let (serverPid, mb1) =
    (let mb2 =
      new [ServerMb]
    in
      let x0 =
        spawn { server(mb2) }
      in
        (mb2, x0))
  in
    mb1; # Use unit value.
    let (x1, mb3) =
      (let (x2, mb4) =
        (serverPid, mb3)
      in
        (x2 ! Add[mb4, 1, 2], mb4))
    in
      x1; # Use unit value.
      let (x3, mb5) =
        (let (x4, mb6) =
          (serverPid, mb5)
        in
          (x4 ! Mul[mb6, 3, 4], mb6))
      in
        x4; # Use unit value.
        let (a, mb7) =
          guard mb5: Ans.Ans {
            receive Ans(a) from mb8 ->
              (a, mb8)
          }
        in
          let (b, mb9) =
            guard mb7: Ans {
              receive Ans(b) from mb10 ->
                (b, mb10)
            }
          in
            (a + b, mb9)
}













#### First receive invoked in a separate function.




#### Using a plain function to add the final result of A and B.


#### Asynchronous calls in separate functions.