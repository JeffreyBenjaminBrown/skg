#!/bin/bash
# USAGE: bash/run-skg-rerunning-on-exit.sh PROFILE_DIR SKG_CONFIG
#   e.g. bash/run-skg-rerunning-on-exit.sh target/debug data/skgconfig.toml
#
# Runs the already built server binary PROFILE_DIR/skg on SKG_CONFIG,
# and runs it again whenever it exits, so a killed server comes back
# (with whatever config the file now holds). bash/start-servers.sh has
# cargo-watch run this after each build; when cargo-watch rebuilds on
# an edit under server/, it signals this script, which stops the binary
# and exits, and cargo-watch starts a fresh copy.
#
# A run that ends quickly (a bad config, say, or a port still held)
# doubles the pause before the next, up to a minute, so a server that
# cannot start retries slowly instead of filling the log.

PROFILE_DIR="$1"
SKG_CONFIG="$2"
if [ -z "$PROFILE_DIR" ] || [ -z "$SKG_CONFIG" ]; then
  echo "usage: $0 PROFILE_DIR SKG_CONFIG" >&2
  exit 2
fi

QUICK_EXIT_SECONDS=30 # a run shorter than this counts as failing to start
MAX_PAUSE_SECONDS=60
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
while true; do
  started_at=$SECONDS
  "$PROFILE_DIR/skg" "$SKG_CONFIG" &
  skg_pid=$!
  wait "$skg_pid"
  status=$?
  skg_pid=""
  if (( SECONDS - started_at < QUICK_EXIT_SECONDS )); then
    pause_seconds=$(( pause_seconds * 2 > MAX_PAUSE_SECONDS
                      ? MAX_PAUSE_SECONDS : pause_seconds * 2 ))
  else
    pause_seconds=1
  fi
  echo "skg exited with status $status; running it again in $pause_seconds s."
  sleep "$pause_seconds" &
  pause_pid=$!
  wait "$pause_pid" # a 'wait', unlike a bare 'sleep', lets the trap run at once
  pause_pid=""
done
