#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).

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

  Self0 = self:ClientMb^o,
  Self1 = self:ClientMb^o,

  ServerPid ! {add, Self0, 1, 2},
  ServerPid ! {mul, Self1, 3, 4},

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