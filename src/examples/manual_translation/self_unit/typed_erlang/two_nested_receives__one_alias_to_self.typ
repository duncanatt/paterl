#### Two nested receives (with state-passing translation and using self as
#### an argument directly).

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

  assert("Ans.Ans", ClientMb),
  receive
    {ans, A} ->
      assert("Ans", ClientMb),
        receive
          {ans, B} -> A + B
        end
  end.