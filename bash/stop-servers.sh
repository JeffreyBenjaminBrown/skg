#!/bin/bash
# USAGE: bash/stop-servers.sh [CONFIG]
#   Stops the server that 'bash/start-servers.sh CONFIG' started, and
#   the start script itself, rerun loop, cargo-watch and all. CONFIG is
#   the path of an skg config file, by default data/jeff/skgconfig.toml
#   (as for bash/start-servers.sh). A relative CONFIG is read from the
#   working directory, except the default, which is read from the top
#   of this repository, so the script can be run from anywhere.
#
# HOW
#   Each start writes CONFIG.running/stop-servers.sh, which signals that
#   start's process group (its PID, as it is a group leader), and
#   records, in a comment, when the kernel says that start began. This
#   script runs the same signal, but only after checking that the PID
#   still names that start, and then waits for the group to exit. A bare run
#   of the generated script, after its start has ended, could signal
#   whatever has since taken that PID: another config's start, or, if
#   run on the other side of a docker container from the start, an
#   unrelated process that happens to share the number.
#
# PITFALL: start-servers.sh has no '#!' line, so when Jeff's shell runs
# it, the process's command line is the shell's, and names neither the
# script nor the config. So the check cannot read the config off the
# command line. Instead it asks three things of the PID: that it still
# leads its own process group; that its start time is exactly the one
# its stop script records (so the number was not reused since); and that
# its working directory holds bash/start-servers.sh (start-servers.sh
# must be run from the top of a repository, and a process with the same
# number on the far side of a container is almost surely elsewhere).
#
# PITFALL: the start time is the kernel's own figure for the process,
# in clock ticks since boot (field 22 of /proc/PID/stat), and the check
# is equality with what the start read of itself. No clock is read. A
# start time reckoned in wall-clock seconds cannot be compared with a
# file's modification time or with 'date': over a day of uptime the
# wall clock and the clock that start times are kept in drift seconds
# apart.

exit_with_usage() {
  echo "usage: bash/stop-servers.sh [CONFIG]" >&2
  exit 2
}

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG=""
for arg in "$@"; do
  case "$arg" in
    --*) exit_with_usage ;;
    *) if [ -n "$CONFIG" ]; then exit_with_usage; fi
       CONFIG="$arg" ;;
  esac
done
CONFIG="${CONFIG:-$REPO/data/jeff/skgconfig.toml}"
STOP_FILE="$CONFIG.running/stop-servers.sh"

if [ ! -f "$STOP_FILE" ]; then
  echo "No $STOP_FILE: bash/start-servers.sh has never started a server for $CONFIG." >&2
  exit 1
fi

PID="$( sed -n 's/^kill -TERM -\([0-9][0-9]*\)$/\1/p' "$STOP_FILE" )"
if [ -z "$PID" ]; then
  echo "$STOP_FILE does not have the form 'kill -TERM -PID'; nothing stopped." >&2
  exit 1
fi

if ! kill -0 "$PID" 2>/dev/null; then
  if kill -0 -"$PID" 2>/dev/null; then
    # The start script died without its cleanup (by SIGKILL, say), and
    # left members of its group, which may be the server, or may be
    # strangers that the number has since been given to.
    echo "The last start for $CONFIG (PID $PID) has exited, but its process" >&2
    echo "group still has members. If these are that start's, stop them" >&2
    echo "with 'kill -TERM -$PID':" >&2
    ps -eo pid,pgid,args | awk -v g="$PID" '$2 == g' >&2
    exit 1
  fi
  echo "No server running for $CONFIG: its last start (PID $PID) has already exited."
  exit 0
fi

start_time_in_clock_ticks_since_boot() {
  # Prints the start time of the process whose PID is $1, as the kernel
  # states it: field 22 of /proc/PID/stat.
  # PITFALL: field 2, the command name, may contain spaces and
  # parentheses, so the fields are counted from after the last ')',
  # where field 3 begins.
  local stat
  local -a fields_from_the_third
  stat="$( cat "/proc/$1/stat" 2>/dev/null )" || return 1
  read -r -a fields_from_the_third <<< "${stat##*) }"
  echo "${fields_from_the_third[19]}"
}

RECORDED_START_TIME="$( sed -n 's/^# start time in clock ticks since boot: \([0-9][0-9]*\)$/\1/p' "$STOP_FILE" )"
if [ -z "$RECORDED_START_TIME" ]; then
  echo "Not stopping PID $PID for $CONFIG: $STOP_FILE" >&2
  echo "does not record the start time of its start, so it was written by an" >&2
  echo "older bash/start-servers.sh, and this script cannot tell whether" >&2
  echo "PID $PID still names that start. Look at it:" >&2
  echo "  ps -o pid,pgid,lstart,args -p $PID" >&2
  echo "  ls -l /proc/$PID/cwd" >&2
  echo "If it is that start, stop it and its group with 'kill -TERM -$PID'." >&2
  exit 1
fi

refuse() {
  echo "Not stopping PID $PID for $CONFIG: $1." >&2
  echo "Its start seems to have ended, and the number to name some other" >&2
  echo "process now. Find the start yourself, with 'ps -eo pid,pgid,args'." >&2
  exit 1
}

pgid="$( ps -o pgid= -p "$PID" | tr -d ' ' )"
[ "$pgid" = "$PID" ] || refuse "it does not lead its own process group"

start_time="$( start_time_in_clock_ticks_since_boot "$PID" )"
[ "$start_time" = "$RECORDED_START_TIME" ] \
  || refuse "its start time, in clock ticks since boot, is '$start_time', not the $RECORDED_START_TIME that $STOP_FILE records"

[ -f "/proc/$PID/cwd/bash/start-servers.sh" ] \
  || refuse "its working directory is not the top of an skg repository"

echo "Stopping the server for $CONFIG (process group $PID)..."
kill -TERM -"$PID"
for i in $(seq 1 30); do
  if ! kill -0 -"$PID" 2>/dev/null; then
    echo "Stopped."
    exit 0
  fi
  sleep 1
done
echo "Process group $PID is still running 30 s after SIGTERM; see 'ps -eo pid,pgid,args'." >&2
exit 1
