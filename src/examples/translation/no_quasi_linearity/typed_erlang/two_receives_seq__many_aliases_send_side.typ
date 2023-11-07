#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).

## Typed Erlang (server side).

server•ServerMb^o(): Unit ->
  assert("Add*.Mul*"),
  receive•ServerMb
    {add, Client, A, B} ->
      Client ! {ans, A + B},
      server•ServerMb^.();
    {mul, Client, A, B} ->
      Client ! {ans, A * B},
      server•ServerMb^.();
  end.

## Typed Erlang (client side).

-main main•ClientMb^o(): Int ->
  ServerPid0 = spawn server:ServerMb^o(),

  ServerPid1 = serverPid0,

  Self0 = self•ClientMb,
  Self1 = self•ClientMb,

  ServerPid1 ! {add, Self0, 1, 2},
  ServerPid0 ! {mul, Self1, 3, 4},

  A =
  assert("Ans.Ans", ClientMb),
  receive•ClientMb
    {ans, A} -> A
  end,

  B =
  assert("Ans", ClientMb),
  receive•ClientMb
    {ans, B} -> B
  end,

  A + B.