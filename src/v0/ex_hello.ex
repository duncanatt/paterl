defmodule :ex_hello do
  require Record
  Record.defrecord :user, [name: nil, age: nil]

  @type user :: record(:user, name: String.t, age: integer)

  @spec hello(String.t) :: :ok
  def hello(name) do
    IO.puts "Hello #{name}."
  end

  @spec new_user(String.t, integer) :: user
  def new_user(name, age) do
    user(name: name, age: age)
  end
end
