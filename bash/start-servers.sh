# USAGE:
# - TO START
#     ./bash/start-servers.sh [CONFIG] [--deb] [--hot-reload]
#   CONFIG is the path of an skg config file, by default
#     data/skgconfig.toml. The options may come before or after it.
#   It builds and runs the release profile unless given --deb,
#     which builds and runs the debug profile instead.
#   The first release build takes minutes, sitting at "Validating config".
#   An unknown option, or a second CONFIG, prints a usage line
#     and exits with status 2, before anything is written.
# - THE RUN FOLDER
#   The files this script writes itself go in CONFIG.running/, beside
#   the config (data/skgconfig.toml.running/ by default). (The build
#   steps write elsewhere: the .elc files beside their sources in
#   elisp/, the server binary under target/.) The script's own files:
#   - stop-servers.sh, which stops this start (rewritten at every start).
#   - start-servers.START.log, one per start, START its UTC time
#     (YYYY-MM-DDTHH-MM-SSZ): the output of the rerun loop, and of
#     cargo-watch with --hot-reload.
#   The server writes its own logs into a subfolder there, one per
#   server run.
# - WHAT RUNS
#   After the config check, bash/compile-elisp.sh byte-compiles the
#   Emacs client (elisp/), unless emacs is not on the path, printing its
#   warnings; a failure there is shown in red, and the server starts anyway.
#   The rerun loop, bash/run-skg-rerunning-on-exit.sh, runs the server,
#   and runs it again whenever it exits, so killing the server (by its
#   PID, or with the 'shutdown' request) reruns it, rereading the config.
#   Hot reload is off unless given --hot-reload: nothing watches server/,
#   and an edit there takes effect only when this script is run again.
#   With --hot-reload, cargo-watch also rebuilds and restarts the server
#   on every edit under server/.
#   Either way, their output goes to CONFIG.running/start-servers.START.log.
# - TO SHUTDOWN
#   - If foregrounded at command line, use
#     Ctrl-C
#   - If in background at command line, run
#       CONFIG.running/stop-servers.sh
#     or do it yourself:
#       kill -TERM -PID # kills the whole group
#     If you do it yourself, the PID was printed when it started,
#       and if that printout is buried, can be found running `ps -af`.

exit_with_usage() {
  echo "usage: bash/start-servers.sh [CONFIG] [--deb] [--hot-reload]" >&2
  exit 2
}

CONFIG=""        # the first argument not starting with '--'
DEBUG_PROFILE="" # set by --deb: build and run the debug profile
HOT_RELOAD=""    # set by --hot-reload
for arg in "$@"; do
  case "$arg" in
    --deb)        DEBUG_PROFILE=1 ;;
    --hot-reload) HOT_RELOAD=1 ;;
    --*)          exit_with_usage ;;
    *) if [ -n "$CONFIG" ]; then exit_with_usage; fi
       CONFIG="$arg" ;;
  esac
done

CONFIG="${CONFIG:-data/skgconfig.toml}"
RUN_FOLDER="$CONFIG.running"
START="$(date -u +%Y-%m-%dT%H-%M-%SZ)" # no colons: a legal file name everywhere
SCRIPT_LOG="$RUN_FOLDER/start-servers.$START.log"
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
  # the script log, which presents as a connection failure. Catching
  # it here, in red, turns that into an obvious fix.
  echo ""
  echo "Validating config: $CONFIG"
  if ! cargo build -q $CARGO_PROFILE_FLAG --bin skg; then
    printf '\033[1;31m%s\033[0m\n' \
      "ERROR: skg failed to compile (see errors above). Servers not started."
    exit 1
  fi
  local config_err
  if ! config_err="$( cargo run -q $CARGO_PROFILE_FLAG --bin skg -- check-config "$CONFIG" 2>&1 )"; then
    printf '\033[1;31m'   # bold red
    echo "=================================================================="
    echo "ERROR: invalid skg config '$CONFIG' -- servers not started:"
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
  # Byte-compile elisp/ beside its sources (bash/compile-elisp.sh), and
  # print all its output: the summary and the warnings worth reading. A
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
    echo "$compile_output"
  else
    printf '\033[1;31m%s\033[0m\n' "$compile_output"   # bold red
    printf '\033[1;31m%s\033[0m\n' \
      "ERROR: the Emacs client failed to compile (above). Starting the server anyway."
  fi
}

skg_api_port() {
  # Never assume 1730: Jeff runs several containers at once, each with
  # its own config naming a different port (see coding-advice/docker.org),
  # so the port must come from the config this start was given.
  sed -n 's/^[[:space:]]*port[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' \
    "$CONFIG" | head -1
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
  # the script log. Match both spellings, and match the server
  # binary itself ('target/debug/skg <config>'), which neither old
  # pattern caught -- cargo-watch does not always take its child down
  # with it. Kill cargo-watch first so it cannot respawn the rerun
  # loop, then the rerun loop so it cannot rerun skg, then skg.
  # Every pattern is scoped to this config, so skg instances serving
  # other configs (other ports, other data) are left alone. Each matches
  # the debug (--deb) and the release form of its command.
  # CONFIG.running/stop-servers.sh signals this script's process group, whose
  # 'cleanup' trap runs this function, so it kills by these patterns too.
  pkill -f "cargo[- ]watch .*--bin skg .*$CONFIG"                2>/dev/null || true
  pkill -f "run-skg-rerunning-on-exit.sh [^ ]+ $CONFIG"          2>/dev/null || true
  pkill -f "target/(debug|release)/skg $CONFIG"                  2>/dev/null || true
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
  echo "ERROR: TCP port $port (from $CONFIG) is still in use, by"
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
    echo "Starting skg server with hot reload, and rerun on exit..."
  else
    echo "Starting skg server with rerun on exit (no hot reload)..."
  fi
  kill_stale_skg
  wait_for_free_api_port
  echo "Server logs go to a folder per server run in $RUN_FOLDER/"
  if [ -n "$HOT_RELOAD" ]; then
    run_skg_with_hot_reload
  else
    run_skg_without_hot_reload
  fi
}

run_skg_with_hot_reload() {
  echo "cargo-watch starting..."
  echo "cargo-watch/stdout-stderr go to $SCRIPT_LOG"
  # "-w server/" says "only watch server/". cargo-watch runs the -s
  # command only if the -x build succeeds.
  cargo watch -w server/ \
    -x "build ${CARGO_PROFILE_FLAG:+$CARGO_PROFILE_FLAG }--bin skg" \
    -s "bash/run-skg-rerunning-on-exit.sh $PROFILE_DIR $CONFIG" \
    >> "$SCRIPT_LOG" 2>&1
}

run_skg_without_hot_reload() {
  # PITFALL: the rerun loop must run in the foreground, in this script's
  # process group, so that Ctrl-C and the generated stop-servers.sh
  # reach it, and this script waits on it. Bash runs the 'cleanup' trap
  # only once the loop exits; the loop traps TERM and INT itself and
  # stops the server first. The preflight already built the binary.
  echo "Rerun loop stdout-stderr go to $SCRIPT_LOG"
  bash/run-skg-rerunning-on-exit.sh "$PROFILE_DIR" "$CONFIG" \
    >> "$SCRIPT_LOG" 2>&1
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

# The run folder holds every file this script writes itself.
mkdir -p "$RUN_FOLDER"
echo "Run folder: $RUN_FOLDER/"

# Create kill script for easy shutdown
echo "#!/bin/bash" >     "$RUN_FOLDER/stop-servers.sh"
echo "kill -TERM -$$" >> "$RUN_FOLDER/stop-servers.sh"
chmod +x                 "$RUN_FOLDER/stop-servers.sh"
echo "Created $RUN_FOLDER/stop-servers.sh - run it to stop all servers"

# The script log, created now, so it exists even if the start stops early.
: >> "$SCRIPT_LOG"

# Abort with a red error if the config is invalid, so a typo cannot
# silently crash-loop skg later.
preflight_check_config

# Byte-compile the Emacs client; warns, but never stops the start.
compile_emacs_client

# Start skg server with auto-restart
start_skg_with_restart
