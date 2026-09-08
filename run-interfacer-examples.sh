#!/usr/bin/env bash
#
# Build, run and analyse the Interfacer examples.
#
# The examples come in two forms. Those under src/examples/interfacer/ use the
# proposed notation, pid(I) and -interface, neither of which currently parses;
# they are design sketches and this script only reports them.
# Those under src/examples/interfacer/encoded/ use the encoding in
# include/interfacer.hrl and do compile, run and analyse.
#
# Usage:
#   ./run-interfacer-examples.sh              build and run the encoded examples
#   ./run-interfacer-examples.sh check        also run Dialyzer and TypEr
#   ./run-interfacer-examples.sh eqwalize     also run Eqwalizer through ELP
#   ./run-interfacer-examples.sh all          everything
#   ./run-interfacer-examples.sh parse        parse-only check of every example,
#                                             including the proposed-notation
#                                             ones, to show exactly where they
#                                             fail
set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EX="$ROOT/src/examples/interfacer"
ENC="$EX/encoded"
EBIN="$ROOT/ebin-interfacer"
PLT="$ROOT/.dialyzer_plt"
MODE="${1:-run}"

green() { printf '\033[32m%s\033[0m\n' "$1"; }
red()   { printf '\033[31m%s\033[0m\n' "$1"; }
head_() { printf '\n\033[1m== %s ==\033[0m\n' "$1"; }

need() {
  command -v "$1" >/dev/null 2>&1 || { red "missing: $1"; return 1; }
}

pass=0; fail=0; skip=0

# ---------------------------------------------------------------- compile ----
build() {
  head_ "compile (encoded examples)"
  need erlc || return 1
  mkdir -p "$EBIN"
  local f
  for f in "$ENC"/*.erl; do
    [ -e "$f" ] || { red "no encoded examples found in $ENC"; return 1; }
    if out=$(erlc -I "$ROOT/include" -o "$EBIN" "$f" 2>&1); then
      green "  ok   $(basename "$f")"
      [ -n "$out" ] && echo "$out" | sed 's/^/       /'
      pass=$((pass+1))
    else
      red   "  FAIL $(basename "$f")"
      echo "$out" | sed 's/^/       /'
      fail=$((fail+1))
    fi
  done
}

# -------------------------------------------------------------------- run ----
run() {
  head_ "run (encoded examples)"
  need erl || return 1
  local f m out
  for f in "$ENC"/*.erl; do
    m="$(basename "$f" .erl)"
    # only modules with a main/0 entry point are runnable; the rest are role
    # modules or type-only modules reached from them
    grep -q 'main/0' "$f" || { printf '  --   %s (no main/0)\n' "$m"; continue; }
    # Sleep before stopping: several examples spawn and return, so init stop
    # would halt the VM before the children print.
    out=$(erl -pa "$EBIN" -noshell -eval "$m:main(), timer:sleep(500), init:stop()." 2>&1)
    if [ $? -eq 0 ]; then
      green "  ok   $m"; echo "$out" | sed 's/^/       /'
      pass=$((pass+1))
    else
      red "  FAIL $m"; echo "$out" | sed 's/^/       /'
      fail=$((fail+1))
    fi
  done
}

# ------------------------------------------------------------------ parse ----
# Parse-only pass over every example, so the proposed-notation files report the
# exact syntax error rather than silently being skipped.
parse_all() {
  head_ "parse check (all examples)"
  need erlc || return 1
  local f rc
  for f in "$EX"/*.erl "$EX"/master_worker_roles_otp/*.erl "$ENC"/*.erl; do
    [ -e "$f" ] || continue
    if erlc -I "$ROOT/include" -o /tmp "$f" >/tmp/parse.log 2>&1; then  # status is erlc's, not a pipeline's
      green "  parses      $(basename "$f")"; pass=$((pass+1))
    else
      printf '  \033[33mproposed\033[0m    %s\n' "$(basename "$f")"
      head -2 /tmp/parse.log | sed 's/^/       /'
      skip=$((skip+1))
    fi
  done
  echo
  echo "  Files reported as 'proposed' use pid(I) or -interface and are expected"
  echo "  not to parse. See include/interfacer.hrl for the encoding that does."
}

# --------------------------------------------------------------- dialyzer ----
check() {
  head_ "Dialyzer / TypEr (encoded examples)"
  need dialyzer || return 1
  if [ ! -f "$PLT" ]; then
    echo "  building PLT once, this takes a few minutes..."
    dialyzer --build_plt --apps erts kernel stdlib --output_plt "$PLT" \
      >/dev/null 2>&1 || { red "  PLT build failed"; return 1; }
  fi
  dialyzer --src --plt "$PLT" -I "$ROOT/include" "$ENC"/*.erl 2>&1 | sed 's/^/    /'
  if command -v typer >/dev/null 2>&1; then
    head_ "TypEr inferred specs"
    typer --show --plt "$PLT" -I "$ROOT/include" "$ENC"/*.erl 2>&1 | sed 's/^/    /'
  fi
}

# -------------------------------------------------------------- eqwalizer ----
eqwalize() {
  head_ "Eqwalizer (via ELP)"
  need elp || return 1
  local f m
  for f in "$ENC"/*.erl; do
    m="$(basename "$f" .erl)"
    echo "  -- $m"
    (cd "$ROOT" && elp eqwalize "$m" 2>&1 | sed 's/^/     /')
  done
}

case "$MODE" in
  run)      build && run ;;
  parse)    parse_all ;;
  check)    build && run && check ;;
  eqwalize) build && eqwalize ;;
  all)      build && run && check && eqwalize && parse_all ;;
  *)        echo "usage: $0 [run|parse|check|eqwalize|all]"; exit 2 ;;
esac

head_ "summary"
case "$MODE" in
  # $skip is only meaningful when parse_all ran; otherwise reporting it as 0
  # reads as "there are no proposed-notation examples".
  parse|all) echo "  passed $pass   failed $fail   proposed-notation (not compiled) $skip" ;;
  *)         echo "  passed $pass   failed $fail" ;;
esac
[ "$fail" -eq 0 ]
