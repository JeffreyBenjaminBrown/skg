# USAGE:
# - TO START
#   By default, this uses data/skgconfig.toml:
#     ./bash/start-servers.sh
#   You can override that default to use data somewhere else,
#     by running this:
#       SKG_CONFIG=another_skg_config ./bash/start-servers.sh
#     or by editing SKG_CONFIG in ./bash/start-servers.sh
# - TO SHUTDOWN
#   - If foregrounded at command line, use
#     Ctrl-C
#   - If in background at command line, run
#       ./bash/stop-servers.sh
#     or do it yourself:
#       kill -TERM -PID # kills the whole group
#     If you do it yourself, the PID was printed when it started,
#       and if that printout is buried, can be found running `ps -af`.

SKG_CONFIG="${SKG_CONFIG:-data/skgconfig.toml}"
DATA_ROOT="$(dirname "$SKG_CONFIG")"
TYPEDB_DATA_DIR="/opt/typedb/server/data"
if [ ! -e "$TYPEDB_DATA_DIR" ]; then
  TYPEDB_DATA_DIR="/opt/typedb/core/server/data"
fi

# Raise the soft file-descriptor limit before launching TypeDB.
# The container's default soft limit is 1024, but TypeDB opens many
# file handles per database (RocksDB uses dozens). During concurrent
# test runs this overruns 1024 and TypeDB's gRPC thread panics with
# "Too many open files", leaving a half-alive process whose HTTP port
# (8000) is up but whose gRPC port (1729) is dead. We raise the soft
# limit to the hard limit so every child process — TypeDB and skg —
# inherits the higher cap.
HARD_NOFILE="$(ulimit -Hn)"
if [ -n "$HARD_NOFILE" ] && [ "$HARD_NOFILE" != "unlimited" ]; then
  ulimit -Sn "$HARD_NOFILE" 2>/dev/null || true
fi
echo "File descriptor limit (soft/hard): $(ulimit -Sn)/$(ulimit -Hn)"

typedb_process_exists() {
  pgrep -f 'typedb_server_bin' >/dev/null 2>&1
}

typedb_grpc_is_up() {
  # A "healthy" TypeDB accepts gRPC connections on port 1729. Checking
  # the process alone is not enough: we have seen half-alive zombies
  # where the process is present and HTTP (8000) is listening but the
  # gRPC thread has panicked (e.g. "Too many open files"). Tests then
  # fail with "Connection refused" even though pgrep reports success.
  (exec 3<>/dev/tcp/127.0.0.1/1729) 2>/dev/null
}

is_typedb_healthy() {
  typedb_process_exists && typedb_grpc_is_up
}

preflight_check_config() {
  # Validate the config BEFORE touching TypeDB, using skg's own loader
  # (the 'check-config' subcommand runs the same load_config the server
  # runs at startup). A malformed skgconfig.toml otherwise makes skg
  # exit(1) before binding port 1730, and -- because cargo-watch only
  # watches server/ -- it crash-loops with the error buried in
  # logs/cargo-watch.log, which presents as "can't connect / TypeDB is
  # down". Catching it here, in red, turns that into an obvious fix.
  echo ""
  echo "Validating config: $SKG_CONFIG"
  if ! cargo build -q --bin skg; then
    printf '\033[1;31m%s\033[0m\n' \
      "ERROR: skg failed to compile (see errors above). Servers NOT started."
    exit 1
  fi
  local config_err
  if ! config_err="$( cargo run -q --bin skg -- check-config "$SKG_CONFIG" 2>&1 )"; then
    printf '\033[1;31m'   # bold red
    echo "=================================================================="
    echo "ERROR: invalid skg config '$SKG_CONFIG' -- servers NOT started:"
    echo "------------------------------------------------------------------"
    echo "$config_err"
    echo "------------------------------------------------------------------"
    echo "Fix the problem above, then re-run ./bash/start-servers.sh"
    echo "=================================================================="
    printf '\033[0m'      # reset color
    exit 1
  fi
  echo "Config OK."
}

start_typedb() {
  # PITFALL: we kill TypeDB before the rm -rf, because deleting database
  # files out from under a running TypeDB would crash it — see
  # docs/troubleshooting/typedb-will-not-start.org.
  pkill -9 -f typedb_server_bin 2>/dev/null || true
  sleep 1 # let the kernel reclaim the port
  echo ""
  echo "Clearing leaked skg-test-* databases from previous runs..."
  # If the previous run crashed mid-test, test databases accumulate on
  # disk. TypeDB reopens every database at startup, so leaks compound
  # across runs until TypeDB panics on "Too many open files". Clearing
  # them here turns recovery into a no-op.
  rm -rf "$TYPEDB_DATA_DIR"/skg-test* 2>/dev/null || true
  echo ""
  echo "Starting TypeDB server..."
  mkdir -p "$DATA_ROOT/logs"
  nohup typedb server > "$DATA_ROOT/logs/typedb.log" 2>&1 < /dev/null &
  echo "TypeDB server started in background (PID: $!)"
  echo "TypeDB logging to $DATA_ROOT/logs/typedb.log"
  echo "Waiting for TypeDB gRPC port 1729 to accept connections..."
  for i in {1..30}; do
    if is_typedb_healthy; then
      echo "TypeDB server is ready (gRPC port 1729 is listening)."
      return 0
    fi
    sleep 1
  done
  echo ""
  echo "ERROR: TypeDB did not become healthy within 30 seconds."
  echo "Last 20 lines of $DATA_ROOT/logs/typedb.log:"
  tail -20 "$DATA_ROOT/logs/typedb.log" 2>/dev/null || true
  return 1
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
  # with it. Kill cargo-watch first so it cannot respawn skg.
  # Every pattern is scoped to THIS config, so skg instances serving
  # other configs (other ports, other data) are left alone.
  pkill -f "cargo[- ]watch .*--bin skg -- $SKG_CONFIG" 2>/dev/null || true
  pkill -f "cargo run --bin skg -- $SKG_CONFIG"        2>/dev/null || true
  pkill -f "target/debug/skg $SKG_CONFIG"              2>/dev/null || true
}

wait_for_free_api_port() {
  # Refuse to launch into an occupied port: skg would exit(1) and
  # cargo-watch would idle, silently, forever.
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
  echo "'Address already in use'. Servers NOT started. Find the holder:"
  echo "  ss -tlnp | grep $port"
  echo "=================================================================="
  printf '\033[0m'      # reset color
  exit 1
}

start_skg_with_restart() {
  echo ""
  echo "Starting skg server with auto-restart on code changes..."
  kill_stale_skg
  wait_for_free_api_port
  echo "cargo-watch starting..."
  echo "Server logs go to $DATA_ROOT/logs/server-to-user.log and $DATA_ROOT/logs/server.jsonl"
  echo "cargo-watch/stdout-stderr go to $DATA_ROOT/logs/cargo-watch.log"
  cargo watch -w server/ -x "run --bin skg -- $SKG_CONFIG" \
    >> "$DATA_ROOT/logs/cargo-watch.log" 2>&1 # "-w server/" says "only watch server/"
}

cleanup() { # trap handler for graceful shutdown
  echo ""
  echo "Shutting down skg server..."

  # Kill cargo-watch and skg processes
  kill_stale_skg
  sleep 1  # Give TypeDB a second to shutdown
  if typedb_process_exists; then
    echo "TypeDB server was still running (a second after start-servers.sh initiated cleanup)."
  else
    echo "TypeDB server has shut down"
  fi
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

# Abort with a red error if the config is invalid, before any TypeDB
# side effects, so a typo can't silently crash-loop skg later.
preflight_check_config

# Check and start TypeDB server if needed. A zombie TypeDB whose gRPC
# thread has died is treated as not-running — start_typedb will kill it
# and restart a fresh one.
if is_typedb_healthy; then
  echo "TypeDB server is already running and healthy"
elif typedb_process_exists; then
  echo "TypeDB process exists but gRPC port 1729 is not responding —"
  echo "  treating as a zombie and restarting it fresh."
  start_typedb || exit 1
else
  start_typedb || exit 1
fi

# Start skg server with auto-restart
start_skg_with_restart
