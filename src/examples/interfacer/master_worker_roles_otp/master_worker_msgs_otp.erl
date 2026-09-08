%%%-------------------------------------------------------------------
%%% Shared message types for the one-module-per-role OTP master_worker
%%% example.
%%%
%%% The three role modules exchange the same four messages. They are declared
%%% once here and referred to remotely, as master_worker_msgs_otp:result().
%%% Interfacer loads this module's type table to check a payload against a
%%% type named in another module.
%%%
%%%-------------------------------------------------------------------
-module(master_worker_msgs_otp).

-export_type([task/0, run/0, work/0, result/0]).

-type task()   :: {task, integer()}.
-type run()    :: {run, integer()}.
-type work()   :: {work, integer()}.
-type result() :: {result, integer()}.
