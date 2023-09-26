#### Two receives in sequence (with state-passing translation and using one
#### alias to self).

## Typed Erlang (server side).

server:ServerMb^o(): Unit ->
  assert("Add*.Mul*"),
  receive
    {add, Client, A, B} ->
      Client ! {ans, A + B},
      server:ServerMb^.();
    {mul, Client, A, B} ->
      Client ! {ans, A * B},
      server:ServerMb^.();
  end.

## Typed Erlang (client side).

-main main:ClientMb^o(): Int ->
  ServerPid = spawn server:ServerMb^o(),

  Self = self:ClientMb^o,

  ServerPid ! {add, Self, 1, 2},
  ServerPid ! {mul, Self, 3, 4},

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