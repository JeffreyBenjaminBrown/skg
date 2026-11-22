#!/bin/bash
# USAGE: bash/run-skg-rerunning-on-exit.sh PROFILE_DIR SKG_CONFIG
#   e.g. bash/run-skg-rerunning-on-exit.sh target/debug data/jeff/skgconfig.toml
#
# Runs the already built server binary PROFILE_DIR/skg on SKG_CONFIG,
# and runs it again whenever it exits, so a killed server comes back
# (with whatever config the file now holds). bash/start-servers.sh
# runs this directly, or, given --hot-reload, has cargo-watch run it
# after each build; then, when cargo-watch rebuilds on an edit under
# server/, it signals this script, which stops the binary and exits,
# and cargo-watch starts a fresh copy.
#
# A run that ends quickly with a non-zero status (a bad config, say, or
# a port still held, or a skgrepo the server refuses) doubles the pause
# before the next. After QUICK_FAILURES_BEFORE_GIVING_UP such runs in a
# row, this script stops trying and exits with status 1, rather than
# retrying forever a server that cannot start; the server's last error
# is just above, in the same log. A run that lasts resets both the pause
# and the count. A run that ends quickly with status 0 (the 'shutdown'
# request, by which a user rereads the config, or a plain kill, which
# the server catches) is rerun and changes neither.

PROFILE_DIR="$1"
SKG_CONFIG="$2"
if [ -z "$PROFILE_DIR" ] || [ -z "$SKG_CONFIG" ]; then
  echo "usage: $0 PROFILE_DIR SKG_CONFIG" >&2
  exit 2
fi

QUICK_EXIT_SECONDS=30 # a shorter run that exits non-zero failed to start
QUICK_FAILURES_BEFORE_GIVING_UP=5 # pausing 2, 4, 8 and 16 s between them
skg_pid=""
pause_pid=""

stop_skg_and_exit() {
  if [ -n "$pause_pid" ]; then kill "$pause_pid" 2>/dev/null; fi
  if [ -n "$skg_pid" ]; then
    kill -TERM "$skg_pid" 2>/dev/null
    wait "$skg_pid" 2>/dev/null
  fi
  exit 0
}
trap stop_skg_and_exit TERM INT HUP

pause_seconds=1
quick_failures_in_a_row=0
while true; do
  started_at=$SECONDS
  "$PROFILE_DIR/skg" "$SKG_CONFIG" &
  skg_pid=$!
  wait "$skg_pid"
  status=$?
  skg_pid=""
  if (( SECONDS - started_at >= QUICK_EXIT_SECONDS )); then
    quick_failures_in_a_row=0
    pause_seconds=1
  elif (( status != 0 )); then
    quick_failures_in_a_row=$(( quick_failures_in_a_row + 1 ))
    pause_seconds=$(( pause_seconds * 2 ))
  fi # a quick run that exited 0 was stopped, not failing: it changes neither
  if (( quick_failures_in_a_row >= QUICK_FAILURES_BEFORE_GIVING_UP )); then
    echo "skg exited with status $status; $quick_failures_in_a_row runs in a row each failed within $QUICK_EXIT_SECONDS s, so giving up. Its error is above."
    exit 1
  fi
  echo "skg exited with status $status; running it again in $pause_seconds s."
  sleep "$pause_seconds" &
  pause_pid=$!
  wait "$pause_pid" # a 'wait', unlike a bare 'sleep', lets the trap run at once
  pause_pid=""
done
