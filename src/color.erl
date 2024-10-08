%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2024 14:52
%%%-------------------------------------------------------------------
-module(color).
-author("duncan").

%% API
-export([black/1, blackb/1, red/1, redb/1, green/1, greenb/1, blue/1, blueb/1]).
-export([yellow/1, yellowb/1, magenta/1, magentab/1, cyan/1, cyanb/1, white/1, whiteb/1]).
-export([on_black/1, on_red/1, on_green/1, on_blue/1, on_yellow/1, on_magenta/1, on_cyan/1, on_white/1]).
-export([rgb/2, on_rgb/2]).
-export([true/2, on_true/2]).

-compile(export_all).

-define(ESC, <<"\e[">>).
-define(RST, <<"0">>).
-define(BOLD, <<"1">>).
-define(SEP, <<";">>).
-define(END, <<"m">>).

%% Colors
-define(BLACK, <<"30">>).
-define(RED, <<"31">>).
-define(GREEN, <<"32">>).
-define(YELLOW, <<"33">>).
-define(BLUE, <<"34">>).
-define(MAGENTA, <<"35">>).
-define(CYAN, <<"36">>).
-define(WHITE, <<"37">>).
-define(DEFAULT, <<"39">>).

%% Background colors
-define(BLACK_BG, <<"40">>).
-define(RED_BG, <<"41">>).
-define(GREEN_BG, <<"42">>).
-define(YELLOW_BG, <<"43">>).
-define(BLUE_BG, <<"44">>).
-define(MAGENTA_BG, <<"45">>).
-define(CYAN_BG, <<"46">>).
-define(WHITE_BG, <<"47">>).
-define(DEFAULT_BG, <<"49">>).

%% RGB
-define(RGB_FG, [<<"38">>, ?SEP, <<"5">>]).
-define(RGB_BG, [<<"48">>, ?SEP, <<"5">>]).

%% True 24-bit colors
-define(TRUE_COLOR_FG, [<<"38">>, ?SEP, <<"2">>]).
-define(TRUE_COLOR_BG, [<<"48">>, ?SEP, <<"2">>]).


make_bin(Code, Data, End) when is_binary(Code), is_list(Data), is_binary(End) ->
  <<Code/binary, (list_to_binary(Data))/binary, End/binary>>;
make_bin(Code, Data, End) when is_binary(Code), is_binary(Data), is_binary(End) ->
  <<Code/binary, Data/binary, End/binary>>.

black(Text) -> make_bin(color(?BLACK), Text, reset()).
blackb(Text) -> make_bin(colorb(?BLACK), Text, reset()).
red(Text) -> make_bin(color(?RED), Text, reset()).
redb(Text) -> make_bin(colorb(?RED), Text, reset()).
green(Text) -> make_bin(color(?GREEN), Text, reset()).
greenb(Text) -> make_bin(colorb(?GREEN), Text, reset()).
yellow(Text) -> make_bin(color(?YELLOW), Text, reset()).
yellowb(Text) -> make_bin(colorb(?YELLOW), Text, reset()).
blue(Text) -> make_bin(color(?BLUE), Text, reset()).
blueb(Text) -> make_bin(colorb(?BLUE), Text, reset()).
magenta(Text) -> make_bin(color(?MAGENTA), Text, reset()).
magentab(Text) -> make_bin(colorb(?MAGENTA), Text, reset()).
cyan(Text) -> make_bin(color(?CYAN), Text, reset()).
cyanb(Text) -> make_bin(colorb(?CYAN), Text, reset()).
white(Text) -> make_bin(color(?WHITE), Text, reset()).
whiteb(Text) -> make_bin(colorb(?WHITE), Text, reset()).
on_black(Text) -> make_bin(color(?BLACK_BG), Text, reset_bg()).
on_red(Text) -> make_bin(color(?RED_BG), Text, reset_bg()).
on_green(Text) -> make_bin(color(?GREEN_BG), Text, reset_bg()).
on_blue(Text) -> make_bin(color(?BLUE_BG), Text, reset_bg()).
on_yellow(Text) -> make_bin(color(?YELLOW_BG), Text, reset_bg()).
on_magenta(Text) -> make_bin(color(?MAGENTA_BG), Text, reset_bg()).
on_cyan(Text) -> make_bin(color(?CYAN_BG), Text, reset_bg()).
on_white(Text) -> make_bin(color(?WHITE_BG), Text, reset_bg()).

rgb(RGB, Text) ->
  [?ESC, ?RGB_FG, ?SEP, rgb_color(RGB), ?END, Text, reset()].

on_rgb(RGB, Text) ->
  [?ESC, ?RGB_BG, ?SEP, rgb_color(RGB), ?END, Text, reset_bg()].

true(RGB, Text) ->
  [?ESC, ?TRUE_COLOR_FG, ?SEP, true_color(RGB), ?END, Text, reset()].

on_true(RGB, Text) ->
  [?ESC, ?TRUE_COLOR_BG, ?SEP, true_color(RGB), ?END, Text, reset()].

%% Internal
color(Color) ->
  <<?ESC/binary, Color/binary, ?END/binary>>.

colorb(Color) ->
  <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

rgb_color([R, G, B]) when R >= 0, R =< 5, G >= 0, G =< 5, B >= 0, B =< 5 ->
  integer_to_list(16 + (R * 36) + (G * 6) + B).

true_color([R1, R2, G1, G2, B1, B2]) ->
  R = erlang:list_to_integer([R1, R2], 16),
  G = erlang:list_to_integer([G1, G2], 16),
  B = erlang:list_to_integer([B1, B2], 16),
  true_color([R, G, B]);

true_color([R, G, B]) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
  [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)].

reset() ->
  <<?ESC/binary, ?RST/binary, ?END/binary>>.

reset_bg() ->
  <<?ESC/binary, ?DEFAULT_BG/binary, ?END/binary>>.