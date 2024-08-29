%%%-------------------------------------------------------------------
%%% @author walker
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 7月 2024 10:30
%%%-------------------------------------------------------------------
-module(robotsn).
-author("walker").

%%-include("paterl.hrl").
%%
%%-import(io, [format/2]).
%%
%%%% API
%%-export([main/0]).
%%
%%%% Internal exports
%%-export([free_door/1, idle_robot/1, free_warehouse/0]).
%%
%%%% TODO: No idea
%%%% Mailbox interface-function associations
%%-new({door_mb, [free_door/2, busy_door/1, finalise_door/2]}).
%%-new({robot_mb, [idle_robot/2, inside_robot/1]}).
%%-new({warehouse_mb, [free_warehouse/1, prepared_warehouse/1, handle_part_taken/2]}).
%%-new({main_mb, [main/0]}).
%%
%%%% Door's message types
%%-type want() :: {want, integer(), robot_mb()}.
%%-type inside() :: {inside, robot_mb()}.
%%-type prepared() :: {prepared, warehouse_mb()}.
%%-type want_leave() :: {want_leave, robot_mb()}.
%%-type outside() :: {outside}.
%%-type table_idle() :: {table_idle, warehouse_mb()}.
%%
%%%% Robot's message types
%%-type go_in() :: {go_in, door_mb()}.
%%-type go_out() :: {go_out, door_mb()}.
%%-type busy() :: {busy}.
%%-type delivered() :: {delivered, warehouse_mb(), door_mb()}.
%%
%%%% Warehouse's message types
%%-type prepare() :: {prepare, integer(), door_mb()}.
%%-type deliver() :: {deliver, robot_mb(), door_mb()}.
%%-type part_taken() :: {part_taken}.
%%
%%%% # Mailbox message interfaces.
%%%% interface Door {
%%%%    Want(Int, Robot!),
%%%%    Inside(Robot!),
%%%%    Prepared(Warehouse!),
%%%%    WantLeave(Robot!),
%%%%    Outside(),
%%%%    TableIdle(Warehouse!)
%%%% }
%%%%
%%%% interface Robot {
%%%%    GoIn(Door!),
%%%%    GoOut(Door!),
%%%%    Busy(),
%%%%    Delivered(Warehouse!, Door!)
%%%% }
%%%%
%%%% interface Warehouse {
%%%%    Prepare(Int, Door!),
%%%%    Deliver(Robot!, Door!),
%%%%    PartTaken()
%%%% }
%%-type door_mb() :: pid() | want() | inside() | prepared() | want_leave() | outside() | table_idle().
%%-type robot_mb() :: pid() | go_in() | go_out() | busy() | delivered().
%%-type warehouse_mb() :: pid() | prepare() | deliver() | part_taken().
%%-type main_mb() :: pid().
%%
%%%% Door
%%%% def freeDoor(self: Door?, warehouse: Warehouse!): Unit {
%%%%   guard self : *Want {
%%%%     free -> ()
%%%%     receive Want(part, robot) from self ->
%%%%       robot ! GoIn(self);
%%%%       warehouse ! Prepare(part, self);
%%%%       busyDoor(self)
%%%%   }
%%%% }
%%-spec free_door(warehouse_mb()) -> no_return().
%%free_door(Warehouse) ->
%%  Self = self(),
%%  ?mb_assert_regex("*Want"),
%%  receive
%%    {want, Part, Robot} ->
%%      Robot ! {go_in, Self},
%%      Warehouse ! {prepare, Part, Self},
%%      busy_door()
%%  end.
%%
%%%% def busyDoor(self: Door?): Unit {
%%%%   guard self : Inside . Prepared . *Want {
%%%%     receive Want(partNum, robot) from self ->
%%%%       robot ! Busy();
%%%%       busyDoor(self)
%%%%     receive Inside(robot) from self ->
%%%%       guard self : Prepared . *Want {
%%%%         receive Prepared(warehouse) from self ->
%%%%           warehouse ! Deliver(robot, self);
%%%%           guard self : WantLeave . TableIdle . *Want {
%%%%             receive WantLeave(robot) from self ->
%%%%               robot ! GoOut(self);
%%%%               finaliseDoor(self, warehouse)
%%%%           }
%%%%       }
%%%%   }
%%%% }
%%-spec busy_door() -> no_return().
%%busy_door() ->
%%  Self = self(),
%%  ?mb_assert_regex("Inside . Prepared . *Want"),
%%  receive
%%    {want, PartNum, Robot} ->
%%      Robot ! {busy},
%%      busy_door();
%%    {inside, Robot} ->
%%      ?mb_assert_regex("Prepared . *Want"),
%%      receive
%%        {prepared, Warehouse} ->
%%          Warehouse ! {deliver, Robot, Self},
%%          ?mb_assert_regex("WantLeave . TableIdle . *Want"),
%%          receive
%%            {want_leave, Robot} ->
%%              Robot ! {go_out, Self},
%%              finalise_door(Warehouse)
%%          end
%%      end
%%  end.
%%
%%%% def finaliseDoor(self: Door?, warehouse: Warehouse!): Unit {
%%%%   guard self : Outside . TableIdle . *Want {
%%%%     receive Outside() from self ->
%%%%       guard self : TableIdle . *Want {
%%%%         receive TableIdle(warehouse) from self ->
%%%%           freeDoor(self, warehouse)
%%%%       }
%%%%     receive TableIdle(warehouse) from self ->
%%%%       guard self : Outside . *Want {
%%%%         receive Outside() from self ->
%%%%           freeDoor(self, warehouse)
%%%%       }
%%%%   }
%%%% }
%%-spec finalise_door(warehouse_mb()) -> no_return().
%%finalise_door(Warehouse) ->
%%  ?mb_assert_regex("Outside . TableIdle . *Want"),
%%  receive
%%    {outside} ->
%%      ?mb_assert_regex("TableIdle . *Want"),
%%      receive
%%        {table_idle, Warehouse} ->
%%          free_door(Warehouse)
%%      end;
%%    {table_idle, Warehouse} ->
%%      ?mb_assert_regex("Outside . *Want"),
%%      receive
%%        {outside} ->
%%          free_door(Warehouse)
%%      end
%%  end.
%%
%%%% Robot
%%%% def idleRobot(self: Robot?, door: Door!): Unit {
%%%%   door ! Want(0, self);
%%%%   guard self : (Busy + GoIn) {
%%%%     receive Busy() from self -> free(self)
%%%%     receive GoIn(door) from self ->
%%%%       door ! Inside(self);
%%%%       insideRobot(self)
%%%%   }
%%%% }
%%-spec idle_robot(door_mb()) -> no_return().
%%idle_robot(Door) ->
%%  Self = self(),
%%  Door ! {want, 0, Self},
%%  ?mb_assert_regex("Busy + GoIn"),
%%  receive
%%    {busy} ->
%%      ok;
%%    {go_in, Door} ->
%%      Door ! {inside, Self},
%%      inside_robot()
%%  end.
%%
%%%% def insideRobot(self: Robot?): Unit {
%%%%   let self =
%%%%     guard self : Delivered {
%%%%       receive Delivered(warehouse, door) from self ->
%%%%         warehouse ! PartTaken();
%%%%         door ! WantLeave(self);
%%%%         self
%%%%     }
%%%%   in
%%%%   guard self : GoOut {
%%%%     receive GoOut(door) from self ->
%%%%       door ! Outside();
%%%%       free(self)
%%%%   }
%%%% }
%%-spec inside_robot() -> no_return().
%%inside_robot() ->
%%  Self = self(),
%%  ?mb_assert_regex("Delivered"),
%%  receive
%%    {delivered, Warehouse, Door} ->
%%      Warehouse ! {part_taken},
%%      Door ! {want_leave, Self}
%%  end,
%%  ?mb_assert_regex("GoOut"),
%%  receive
%%    {go_out, Door} ->
%%      Door ! {outside}
%%  end.
%%
%%%% Warehouse
%%%% def freeWarehouse(self: Warehouse?): Unit {
%%%%   guard self : Prepare + 1 {
%%%%     free -> ()
%%%%     receive Prepare(partNum, door) from self ->
%%%%       door ! Prepared(self);
%%%%       preparedWarehouse(self)
%%%%   }
%%%% }
%%-spec free_warehouse() -> no_return().
%%free_warehouse() ->
%%  Self = self(),
%%  ?mb_assert_regex("Prepare + 1"),
%%  receive
%%    {prepare, PartNum, Door} ->
%%      Door ! {prepared, Self},
%%      prepared_warehouse(Self)
%%  end.
%%
%%%% def preparedWarehouse(self: Warehouse?): Unit {
%%%%   guard self : Deliver {
%%%%     receive Deliver(robot, door) from self ->
%%%%       robot ! Delivered(self, door);
%%%%       handlePartTaken(self, door)
%%%%   }
%%%% }
%%-spec prepared_warehouse() -> no_return().
%%prepared_warehouse() ->
%%  Self = self(),
%%  ?mb_assert_regex("Deliver"),
%%  receive
%%    {deliver, Robot, Door} ->
%%      Robot ! {delivered, Self, Door},
%%      handle_part_taken(Self, Door)
%%  end.
%%
%%%% def handlePartTaken(self: Warehouse?, door: Door!): Unit {
%%%%   guard self : PartTaken {
%%%%     receive PartTaken() from self ->
%%%%       door ! TableIdle(self);
%%%%       freeWarehouse(self)
%%%%   }
%%%% }
%%-spec handle_part_taken(door_mb()) -> no_return().
%%handle_part_taken(Door) ->
%%  Self = self(),
%%  ?mb_assert_regex("PartTaken"),
%%  receive
%%    {part_taken} ->
%%      Door ! {table_idle, Self},
%%      free_warehouse()
%%  end.
%%
%%%% def main(): Unit {
%%%%   let robot1 = new[Robot] in
%%%%   let robot2 = new[Robot] in
%%%%   let robot3 = new[Robot] in
%%%%   let door = new[Door] in
%%%%   let warehouse = new[Warehouse] in
%%%%   spawn { freeDoor(door, warehouse) };
%%%%   spawn { idleRobot(robot1, door) };
%%%%   spawn { idleRobot(robot2, door) };
%%%%   spawn { idleRobot(robot3, door) };
%%%%   spawn { freeWarehouse(warehouse) }
%%%% }
%%-spec main() -> no_return().
%%main() ->
%%  ?mb_new(warehouse_mb),
%%  spawn(?MODULE, free_door, [Warehouse]),
%%  ?mb_new(robot_mb),
%%  spawn(?MODULE, idle_robot, [Door]),
%%  ?mb_new(robot_mb),
%%  spawn(?MODULE, idle_robot, [Door]),
%%  ?mb_new(robot_mb),
%%  spawn(?MODULE, idle_robot, [Door]),
%%  ?mb_new(warehouse_mb),
%%  spawn(?MODULE, free_warehouse, []).