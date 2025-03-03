%%
%% %CopyrightBegin%
%%
%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(color).
-moduledoc "Shell output color support.".
-author("duncan").

%%% Public API.
-export([black/1, blackb/1, red/1, redb/1, green/1, greenb/1, blue/1, blueb/1]).
-export([yellow/1, yellowb/1, magenta/1, magentab/1, cyan/1, cyanb/1, white/1, whiteb/1]).
-export([on_black/1, on_red/1, on_green/1, on_blue/1, on_yellow/1, on_magenta/1, on_cyan/1, on_white/1]).
-export([rgb/2, on_rgb/2]).
-export([true/2, on_true/2]).

%%% Public types.
-export_type([rgb_range/0, rgb/0, hex_range/0, hex/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Escape codes.
-define(ESC, <<"\e[">>).
-define(RST, <<"0">>).
-define(BOLD, <<"1">>).
-define(SEP, <<";">>).
-define(END, <<"m">>).

%% Colors.
-define(BLACK, <<"30">>).
-define(RED, <<"31">>).
-define(GREEN, <<"32">>).
-define(YELLOW, <<"33">>).
-define(BLUE, <<"34">>).
-define(MAGENTA, <<"35">>).
-define(CYAN, <<"36">>).
-define(WHITE, <<"37">>).
-define(DEFAULT, <<"39">>).

%% Background colors.
-define(BLACK_BG, <<"40">>).
-define(RED_BG, <<"41">>).
-define(GREEN_BG, <<"42">>).
-define(YELLOW_BG, <<"43">>).
-define(BLUE_BG, <<"44">>).
-define(MAGENTA_BG, <<"45">>).
-define(CYAN_BG, <<"46">>).
-define(WHITE_BG, <<"47">>).
-define(DEFAULT_BG, <<"49">>).

%% RGB.
-define(RGB_FG, [<<"38">>, ?SEP, <<"5">>]).
-define(RGB_BG, [<<"48">>, ?SEP, <<"5">>]).

%% True 24-bit colors.
-define(TRUE_COLOR_FG, [<<"38">>, ?SEP, <<"2">>]).
-define(TRUE_COLOR_BG, [<<"48">>, ?SEP, <<"2">>]).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Color.".
-type color() :: binary().

-doc "Color code.".
-type code() :: binary().

-doc "RGB range.".
-type rgb_range() :: 0..5.

-doc "True color range.".
-type hex_range() :: 0..255.

-doc "RGB color specification.".
-type rgb() :: {R :: rgb_range(), G :: rgb_range(), B :: rgb_range()}.

-doc "True color specification.".
-type hex() :: {R :: hex_range(), G :: hex_range(), B :: hex_range()} |
string().


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc "Returns `Text` in black.".
-spec black(Text :: iolist()) -> iolist().
black(Text) -> make_bin(color(?BLACK), Text, reset()).

-doc "Returns `Text` in bold black.".
-spec blackb(Text :: iolist()) -> iolist().
blackb(Text) -> make_bin(colorb(?BLACK), Text, reset()).

-doc "Returns `Text` in red.".
-spec red(Text :: iolist()) -> iolist().
red(Text) -> make_bin(color(?RED), Text, reset()).

-doc "Returns `Text` in bold red.".
-spec redb(Text :: iolist()) -> iolist().
redb(Text) -> make_bin(colorb(?RED), Text, reset()).

-doc "Returns `Text` in green.".
-spec green(Text :: iolist()) -> iolist().
green(Text) -> make_bin(color(?GREEN), Text, reset()).

-doc "Returns `Text` in bold green.".
-spec greenb(Text :: iolist()) -> iolist().
greenb(Text) -> make_bin(colorb(?GREEN), Text, reset()).

-doc "Returns `Text` in yellow.".
-spec yellow(Text :: iolist()) -> iolist().
yellow(Text) -> make_bin(color(?YELLOW), Text, reset()).

-doc "Returns `Text` in bold yellow.".
-spec yellowb(Text :: iolist()) -> iolist().
yellowb(Text) -> make_bin(colorb(?YELLOW), Text, reset()).

-doc "Returns `Text` in blue.".
-spec blue(Text :: iolist()) -> iolist().
blue(Text) -> make_bin(color(?BLUE), Text, reset()).

-doc "Returns `Text` in bold blue.".
-spec blueb(Text :: iolist()) -> iolist().
blueb(Text) -> make_bin(colorb(?BLUE), Text, reset()).

-doc "Returns `Text` in magenta.".
-spec magenta(Text :: iolist()) -> iolist().
magenta(Text) -> make_bin(color(?MAGENTA), Text, reset()).

-doc "Returns `Text` in bold magenta.".
-spec magentab(Text :: iolist()) -> iolist().
magentab(Text) -> make_bin(colorb(?MAGENTA), Text, reset()).

-doc "Returns `Text` in cyan.".
-spec cyan(Text :: iolist()) -> iolist().
cyan(Text) -> make_bin(color(?CYAN), Text, reset()).

-doc "Returns `Text` in bold cyan.".
-spec cyanb(Text :: iolist()) -> iolist().
cyanb(Text) -> make_bin(colorb(?CYAN), Text, reset()).

-doc "Returns `Text` in white.".
-spec white(Text :: iolist()) -> iolist().
white(Text) -> make_bin(color(?WHITE), Text, reset()).

-doc "Returns `Text` in bold white.".
-spec whiteb(Text :: iolist()) -> iolist().
whiteb(Text) -> make_bin(colorb(?WHITE), Text, reset()).

-doc "Returns `Text` on a black background.".
-spec on_black(Text :: iolist()) -> iolist().
on_black(Text) -> make_bin(color(?BLACK_BG), Text, reset_bg()).

-doc "Returns `Text` on a red background.".
-spec on_red(Text :: iolist()) -> iolist().
on_red(Text) -> make_bin(color(?RED_BG), Text, reset_bg()).

-doc "Returns `Text` on a green background.".
-spec on_green(Text :: iolist()) -> iolist().
on_green(Text) -> make_bin(color(?GREEN_BG), Text, reset_bg()).

-doc "Returns `Text` on a blue background.".
-spec on_blue(Text :: iolist()) -> iolist().
on_blue(Text) -> make_bin(color(?BLUE_BG), Text, reset_bg()).

-doc "Returns `Text` on a yellow background.".
-spec on_yellow(Text :: iolist()) -> iolist().
on_yellow(Text) -> make_bin(color(?YELLOW_BG), Text, reset_bg()).

-doc "Returns `Text` on a magenta background.".
-spec on_magenta(Text :: iolist()) -> iolist().
on_magenta(Text) -> make_bin(color(?MAGENTA_BG), Text, reset_bg()).

-doc "Returns `Text` on a cyan background.".
-spec on_cyan(Text :: iolist()) -> iolist().
on_cyan(Text) -> make_bin(color(?CYAN_BG), Text, reset_bg()).

-doc "Returns `Text` on a white background.".
-spec on_white(Text :: iolist()) -> iolist().
on_white(Text) -> make_bin(color(?WHITE_BG), Text, reset_bg()).

-doc "Returns `Text` in RGB.".
-spec rgb(RGB, Text) -> Text0
  when
  RGB :: rgb(),
  Text :: iolist(),
  Text0 :: iolist().
rgb(RGB, Text) ->
  [?ESC, ?RGB_FG, ?SEP, rgb_color(RGB), ?END, Text, reset()].

-doc "Returns `Text` on a RGB background.".
-spec on_rgb(RGB, Text) -> Text0
  when
  RGB :: rgb(),
  Text :: iolist(),
  Text0 :: iolist().
on_rgb(RGB, Text) ->
  [?ESC, ?RGB_BG, ?SEP, rgb_color(RGB), ?END, Text, reset_bg()].

-doc "Returns `Text` in true color.".
-spec true(Hex, Text) -> Text0
  when
  Hex :: hex(),
  Text :: iolist(),
  Text0 :: iolist().
true(Hex, Text) ->
  [?ESC, ?TRUE_COLOR_FG, ?SEP, true_color(Hex), ?END, Text, reset()].

-doc "Returns `Text` on a true color background.".
-spec on_true(Hex, Text) -> Text0
  when
  Hex :: hex(),
  Text :: iolist(),
  Text0 :: iolist().
on_true(Hex, Text) ->
  [?ESC, ?TRUE_COLOR_BG, ?SEP, true_color(Hex), ?END, Text, reset()].


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc "Returns a binary representing the color code.".
-spec make_bin(Code, Data, End) -> Code0
  when
  Code :: code(),
  Data :: list() | binary(),
  End :: code(),
  Code0 :: code().
make_bin(Code, Data, End) when is_binary(Code), is_list(Data), is_binary(End) ->
  <<Code/binary, (list_to_binary(Data))/binary, End/binary>>;
make_bin(Code, Data, End) when is_binary(Code), is_binary(Data), is_binary(End) ->
  <<Code/binary, Data/binary, End/binary>>.

-doc "Resets the foreground color.".
-spec reset() -> code().
reset() ->
  <<?ESC/binary, ?RST/binary, ?END/binary>>.

-doc "Resets the background color.".
-spec reset_bg() -> code().
reset_bg() ->
  <<?ESC/binary, ?DEFAULT_BG/binary, ?END/binary>>.

-doc "Returns the foreground `Color` code.".
-spec color(Color :: color()) -> code().
color(Color) ->
  <<?ESC/binary, Color/binary, ?END/binary>>.

-doc "Returns the backgound `Color` code.".
-spec colorb(Color :: color()) -> code().
colorb(Color) ->
  <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

-doc "Returns the RGB color code for the specified RGB range.".
-spec rgb_color(RGB :: rgb()) -> non_neg_integer().
rgb_color({R, G, B}) when R >= 0, R =< 5, G >= 0, G =< 5, B >= 0, B =< 5 ->
  integer_to_list(16 + (R * 36) + (G * 6) + B).

-doc """
Returns the true color code for the specified three-component RGB color
code or hex string.
""".
-spec true_color(Hex :: hex()) -> [non_neg_integer()].
true_color([R1, R2, G1, G2, B1, B2]) ->
  R = erlang:list_to_integer([R1, R2], 16),
  G = erlang:list_to_integer([G1, G2], 16),
  B = erlang:list_to_integer([B1, B2], 16),
  true_color({R, G, B});
true_color({R, G, B}) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
  [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)].