# USAGE:
# - TO START
#     ./bash/start-servers.sh [--deb] [--hot-reload]
#   By default, this uses data/skgconfig.toml.
#   You can override that default to use data somewhere else,
#     by running this:
#       SKG_CONFIG=another_skg_config ./bash/start-servers.sh
#     or by editing SKG_CONFIG in ./bash/start-servers.sh
#   It builds and runs the release profile unless given --deb,
#     which builds and runs the debug profile instead.
#   The first release build takes minutes, sitting at "Validating config".
#   Any other argument prints a usage line and exits with status 2.
# - WHAT RUNS
#   After the config check, bash/compile-elisp.sh byte-compiles the
#   Emacs client (elisp/), unless emacs is not on the path; a failure
#   there is shown in red, and the server starts anyway.
#   The rerun loop, bash/run-skg-rerunning-on-exit.sh, runs the server,
#   and runs it again whenever it exits, so killing the server (by its
#   PID, or with the 'shutdown' request) restarts it, rereading the config.
#   Hot reload is off unless given --hot-reload: nothing watches server/,
#   and an edit there takes effect only when this script is run again.
#   With --hot-reload, cargo-watch also rebuilds and restarts the server
#   on every edit under server/.
#   Either way, the rerun loop's output goes to logs/cargo-watch.log
#   beside the config.
# - TO SHUTDOWN
#   - If foregrounded at command line, use
#     Ctrl-C
#   - If in background at command line, run
#       ./bash/stop-servers.sh
#     or do it yourself:
#       kill -TERM -PID # kills the whole group
#     If you do it yourself, the PID was printed when it started,
#       and if that printout is buried, can be found running `ps -af`.

DEBUG_PROFILE="" # set by --deb: build and run the debug profile
HOT_RELOAD=""    # set by --hot-reload
for arg in "$@"; do
  case "$arg" in
    --deb)        DEBUG_PROFILE=1 ;;
    --hot-reload) HOT_RELOAD=1 ;;
    *) echo "usage: bash/start-servers.sh [--deb] [--hot-reload]" >&2
       exit 2 ;;
  esac
done

SKG_CONFIG="${SKG_CONFIG:-data/skgconfig.toml}"
DATA_ROOT="$(dirname "$SKG_CONFIG")"
if [ -n "$DEBUG_PROFILE" ]; then
  CARGO_PROFILE_FLAG=""
  PROFILE_DIR="target/debug"
else
  CARGO_PROFILE_FLAG="--release"
  PROFILE_DIR="target/release"
fi

preflight_check_config() {
  # Validate the config before starting the server, using skg's own loader
  # (the 'check-config' subcommand runs the same load_config the server
  # runs at startup). A malformed skgconfig.toml otherwise makes skg
  # exit(1) before binding port 1730, and the rerun loop reruns it,
  # crash-looping (with backoff) with the error buried in
  # logs/cargo-watch.log, which presents as a connection failure. Catching
  # it here, in red, turns that into an obvious fix.
  echo ""
  echo "Validating config: $SKG_CONFIG"
  if ! cargo build -q $CARGO_PROFILE_FLAG --bin skg; then
    printf '\033[1;31m%s\033[0m\n' \
      "ERROR: skg failed to compile (see errors above). Servers not started."
    exit 1
  fi
  local config_err
  if ! config_err="$( cargo run -q $CARGO_PROFILE_FLAG --bin skg -- check-config "$SKG_CONFIG" 2>&1 )"; then
    printf '\033[1;31m'   # bold red
    echo "=================================================================="
    echo "ERROR: invalid skg config '$SKG_CONFIG' -- servers not started:"
    echo "------------------------------------------------------------------"
    echo "$config_err"
    echo "------------------------------------------------------------------"
    echo "Fix the problem above, then re-run ./bash/start-servers.sh"
    echo "=================================================================="
    printf '\033[0m'      # reset color
    exit 1
  fi
  if [ -n "$config_err" ]; then
    # Warnings (unknown keys) do not stop the start; the server also
    # sends them to each client, which shows them in a buffer.
    printf '\033[1;33m%s\033[0m\n' "$config_err"   # bold yellow
  fi
  echo "Config OK."
}

compile_emacs_client() {
  # Byte-compile elisp/ beside its sources (bash/compile-elisp.sh). A
  # failure does not withhold the server: skg-init.el loads any .el
  # newer than its .elc as source, so a stale .elc is never loaded.
  echo ""
  echo "Compiling the Emacs client: bash/compile-elisp.sh"
  if ! command -v emacs >/dev/null 2>&1; then
    printf '\033[1;33m%s\033[0m\n' \
      "WARNING: emacs is not on the path; the Emacs client was not compiled."
    return 0
  fi
  local compile_output
  if compile_output="$( bash/compile-elisp.sh 2>&1 )"; then
    echo "$compile_output" | tail -2
  else
    printf '\033[1;31m%s\033[0m\n' "$compile_output"   # bold red
    printf '\033[1;31m%s\033[0m\n' \
      "ERROR: the Emacs client failed to compile (above). Starting the server anyway."
  fi
}

skg_api_port() {
  # Never assume 1730: Jeff runs several containers at once, each with
  # its own config naming a different port (see coding-advice/docker.org),
  # so the port must come from the config this run was given.
  sed -n 's/^[[:space:]]*port[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' \
    "$SKG_CONFIG" | head -1
}

port_is_bound() {
  (exec 3<>/dev/tcp/127.0.0.1/"$1") 2>/dev/null
}

kill_stale_skg() {
  # PITFALL: the executable calls itself 'cargo-watch', not
  # 'cargo watch', so the old pattern "cargo watch.*skg" matched
  # nothing. A second run of this script therefore left the previous
  # cargo-watch and its skg alive; the new skg died with
  # 'Address already in use' and cargo-watch sat there idle -- which
  # presents as "the server won't start" with the real error buried in
  # logs/cargo-watch.log. Match both spellings, and match the server
  # binary itself ('target/debug/skg <config>'), which neither old
  # pattern caught -- cargo-watch does not always take its child down
  # with it. Kill cargo-watch first so it cannot respawn the rerun
  # loop, then the rerun loop so it cannot rerun skg, then skg.
  # Every pattern is scoped to this config, so skg instances serving
  # other configs (other ports, other data) are left alone. Each matches
  # the debug (--deb) and the release form of its command.
  # bash/stop-servers.sh signals this script's process group, whose
  # 'cleanup' trap runs this function, so it kills by these patterns too.
  pkill -f "cargo[- ]watch .*--bin skg .*$SKG_CONFIG"                2>/dev/null || true
  pkill -f "run-skg-rerunning-on-exit.sh [^ ]+ $SKG_CONFIG"          2>/dev/null || true
  pkill -f "target/(debug|release)/skg $SKG_CONFIG"                  2>/dev/null || true
}

wait_for_free_api_port() {
  # Refuse to launch into an occupied port: skg would exit(1), and the
  # rerun loop would rerun it, silently, forever.
  local port
  port="$( skg_api_port )"
  if [ -z "$port" ]; then return 0; fi
  for i in 1 2 3 4 5; do
    port_is_bound "$port" || return 0
    sleep 1 # a just-killed server needs a moment to release the port
  done
  printf '\033[1;31m'   # bold red
  echo "=================================================================="
  echo "ERROR: TCP port $port (from $SKG_CONFIG) is still in use, by"
  echo "something this script did not start and cannot identify as an"
  echo "skg server for this config. skg would die with"
  echo "'Address already in use'. Servers not started. Find the holder:"
  echo "  ss -tlnp | grep $port"
  echo "=================================================================="
  printf '\033[0m'      # reset color
  exit 1
}

start_skg_with_restart() {
  echo ""
  if [ -n "$HOT_RELOAD" ]; then
    echo "Starting skg server with hot reload, and restart on exit..."
  else
    echo "Starting skg server with restart on exit (no hot reload)..."
  fi
  kill_stale_skg
  wait_for_free_api_port
  mkdir -p "$DATA_ROOT/logs" # a fresh data root has none
  echo "Server logs go to $DATA_ROOT/logs/server-to-user.log and $DATA_ROOT/logs/server.jsonl"
  if [ -n "$HOT_RELOAD" ]; then
    run_skg_under_cargo_watch
  else
    run_skg_rerunning_on_exit
  fi
}

run_skg_under_cargo_watch() {
  echo "cargo-watch starting..."
  echo "cargo-watch/stdout-stderr go to $DATA_ROOT/logs/cargo-watch.log"
  # "-w server/" says "only watch server/". cargo-watch runs the -s
  # command only if the -x build succeeds.
  cargo watch -w server/ \
    -x "build ${CARGO_PROFILE_FLAG:+$CARGO_PROFILE_FLAG }--bin skg" \
    -s "bash/run-skg-rerunning-on-exit.sh $PROFILE_DIR $SKG_CONFIG" \
    >> "$DATA_ROOT/logs/cargo-watch.log" 2>&1
}

run_skg_rerunning_on_exit() {
  # PITFALL: the rerun loop must run in the foreground, in this script's
  # process group, so that Ctrl-C and the generated bash/stop-servers.sh
  # reach it, and this script waits on it. Bash runs the 'cleanup' trap
  # only once the loop exits; the loop traps TERM and INT itself and
  # stops the server first. The preflight already built the binary.
  # The log keeps the name 'cargo-watch.log', which both clients cite.
  echo "Rerun loop stdout-stderr go to $DATA_ROOT/logs/cargo-watch.log"
  bash/run-skg-rerunning-on-exit.sh "$PROFILE_DIR" "$SKG_CONFIG" \
    >> "$DATA_ROOT/logs/cargo-watch.log" 2>&1
}

cleanup() { # trap handler for graceful shutdown
  echo ""
  echo "Shutting down skg server..."

  # Kill cargo-watch and skg processes
  kill_stale_skg
  exit 0
}

# Set up signal traps
trap cleanup SIGINT SIGTERM


#############
# Main script
#############

echo "=== Server Startup Script ==="
echo "Script PID: $$"
echo "Process group: $(ps -o pgid= -p $$)"

# Create kill script for easy shutdown
echo "#!/bin/bash" >     bash/stop-servers.sh
echo "kill -TERM -$$" >> bash/stop-servers.sh
chmod +x                 bash/stop-servers.sh
echo "Created bash/stop-servers.sh - run it to stop all servers"

# Abort with a red error if the config is invalid, so a typo cannot
# silently crash-loop skg later.
preflight_check_config

# Byte-compile the Emacs client; warns, but never stops the start.
compile_emacs_client

# Start skg server with auto-restart
start_skg_with_restart
