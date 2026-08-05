%%%-------------------------------------------------------------------
%%% @author laura
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% Port of the pat-lang barber2 example, itself a variant of the Savina
%%% sleeping barber benchmark. A waiting room queues customers by sending
%%% messages to itself and relays them one by one to the barber, who
%%% alternates between cutting hair and sleeping when the room is empty.
%%% @end
%%%-------------------------------------------------------------------
-module(barber2).
-author("laura").

%%% Includes.
-include("paterl.hrl").

%%% Imports.
-import(io, [format/2]).

%%% API.
-export([main/0]).

%%% Internal exports.
-export([barber/0, waiting_room_start/2, customer/1]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

%%% Messages.

%% Waiting room.
-type enter() :: {enter, customer_mb()}.
-type next() :: {next, barber_mb()}.
-type sleeping() :: {sleeping, barber_mb()}.
-type waiting_customer() :: {waiting_customer, customer_mb()}.

%% Customer.
-type start() :: {start}.
-type full() :: {full}.
-type no_wait() :: {no_wait}.
-type wait() :: {wait}.
-type done() :: {done}.

%% Barber.
-type wake() :: {wake, waiting_room_mb()}.
-type customer_ready() :: {customer_ready, customer_mb(), waiting_room_mb()}.
-type room_empty() :: {room_empty, waiting_room_mb()}.
-type exit() :: {exit}.

%%% Interfaces.

%% Waiting room.
-type waiting_room_mb() :: pid() | enter() | next() | sleeping() | waiting_customer().

%% Customer.
-type customer_mb() :: pid() | start() | full() | no_wait() | wait() | done().

%% Barber.
-type barber_mb() :: pid() | wake() | customer_ready() | room_empty() | exit().

%% Main.
-type main_mb() :: pid().

%%% Interface-function associations.

%% Waiting room.
-new({waiting_room_mb, [waiting_room_start/2]}).
-use({waiting_room_mb, [waiting_room/2, waiting_room_sleeping_barber/2]}).

%% Customer.
-new({customer_mb, [customer/1]}).
-use({customer_mb, [waiting_customer/0]}).

%% Barber.
-new({barber_mb, [barber/0]}).
-use({barber_mb, [awake_barber/0, sleeping_barber/0]}).

%% Main.
-new({main_mb, [main/0]}).


%%% ----------------------------------------------------------------------------
%%% API.
%%% ----------------------------------------------------------------------------

%% @doc Waiting room process started with a sleeping barber.
-spec waiting_room_start(barber_mb(), integer()) -> no_return().
waiting_room_start(Barber, Capacity) ->
  waiting_room_sleeping_barber(Barber, Capacity).

%% @doc Waiting room loop serving customers while the barber is awake.
-spec waiting_room(integer(), integer()) -> no_return().
waiting_room(NumCustomers, Capacity) ->
  ?expects("Enter* . Waiting_customer* . Next"),
  receive
    {enter, Customer} ->
      if NumCustomers >= Capacity ->
        format("Waiting room full; kicking customer out~n", []),
        Customer ! {full},
        waiting_room(NumCustomers, Capacity);
        true ->
          Customer ! {wait},
          Self = self(),
          Self ! {waiting_customer, Customer},
          waiting_room(NumCustomers + 1, Capacity)
      end;
    {next, Barber} ->
      if NumCustomers > 0 ->
        ?expects("Enter* . Waiting_customer*"),
        receive
          {waiting_customer, Customer} ->
            Self0 = self(),
            Barber ! {customer_ready, Customer, Self0},
            waiting_room(NumCustomers - 1, Capacity);
          {enter, Customer} ->
            % Customer has just entered.
            Customer ! {no_wait},
            Self1 = self(),
            Barber ! {customer_ready, Customer, Self1},
            waiting_room(NumCustomers, Capacity)
        after 100 ->
          Barber ! {exit}
        end;
        true ->
          Self2 = self(),
          Barber ! {room_empty, Self2},
          ?expects("Enter* . Waiting_customer* . Sleeping"),
          receive
            {sleeping, Barber0} ->
              waiting_room_sleeping_barber(Barber0, Capacity)
          end
      end
  end.

%% @doc Waiting room loop while the barber is snoozing.
-spec waiting_room_sleeping_barber(barber_mb(), integer()) -> no_return().
waiting_room_sleeping_barber(Barber, Capacity) ->
  ?expects("Enter* . Waiting_customer*"),
  receive
    {waiting_customer, Customer} ->
      Self = self(),
      Self ! {waiting_customer, Customer},
      Barber ! {wake, Self},
      waiting_room(1, Capacity);
    {enter, Customer} ->
      Self = self(),
      Self ! {waiting_customer, Customer},
      Customer ! {wait},
      Barber ! {wake, Self},
      waiting_room(1, Capacity)
  after 100 ->
    format("Waiting room exited.~n", [])
  end.

%% @doc Barber process starting off asleep.
-spec barber() -> no_return().
barber() ->
  sleeping_barber().

%% @doc Barber loop cutting the hair of the customers relayed by the waiting
%% room.
-spec awake_barber() -> no_return().
awake_barber() ->
  ?expects("Customer_ready + Room_empty + Exit"),
  receive
    {exit} ->
      format("Barber exited.~n", []);
    {room_empty, WaitingRoom} ->
      format("Room empty; going to sleep~n", []),
      Self = self(),
      WaitingRoom ! {sleeping, Self},
      sleeping_barber();
    {customer_ready, Customer, WaitingRoom} ->
      Customer ! {start},
      format("Cutting hair~n", []),
      format("Finished cutting hair; notifying customer and waiting room~n", []),
      Customer ! {done},
      Self0 = self(),
      WaitingRoom ! {next, Self0},
      awake_barber()
  end.

%% @doc Barber loop snoozing until woken by the waiting room.
-spec sleeping_barber() -> no_return().
sleeping_barber() ->
  ?expects("Wake + 1"),
  receive
    {wake, WaitingRoom} ->
      Self = self(),
      WaitingRoom ! {next, Self},
      awake_barber()
  after 100 ->
    format("Barber exited while sleeping.~n", [])
  end.

%% @doc Customer entering the waiting room for a haircut.
-spec customer(waiting_room_mb()) -> no_return().
customer(WaitingRoom) ->
  Self = self(),
  WaitingRoom ! {enter, Self},
  ?expects(customer_mb, "Full + (No_wait . Start . Done) + (Wait . Start . Done)"),
  receive
    {full} ->
      format("Room is full. Oh well, best go somewhere else~n", []);
    {wait} ->
      format("Waiting~n", []),
      waiting_customer();
    {no_wait} ->
      format("No need to wait; going to barber~n", []),
      waiting_customer()
  end.

%% @doc Customer waiting for their haircut to start and finish.
-spec waiting_customer() -> no_return().
waiting_customer() ->
  ?expects("Start . Done"),
  receive
    {start} ->
      format("Barber is starting my haircut~n", []),
      ?expects("Done"),
      receive
        {done} ->
          format("Haircut finished!~n", [])
      end
  end.

%% @doc Launcher.
-spec main() -> any().
main() ->
  BarberMb = spawn(?MODULE, barber, []),
  WaitingRoomMb = spawn(?MODULE, waiting_room_start, [BarberMb, 10]),
  Customer1 = spawn(?MODULE, customer, [WaitingRoomMb]),
  Customer2 = spawn(?MODULE, customer, [WaitingRoomMb]),
  Customer3 = spawn(?MODULE, customer, [WaitingRoomMb]),
  ok.


%% ./src/paterl src/examples/erlang/other/barber2.erl -v all -I include
