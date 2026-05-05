# USAGE:
# - TO START
#   By default, this uses data/skgconfig.toml:
#     ./bash/run-servers.sh
#   You can override that default to use data somewhere else,
#     by running this:
#       SKG_CONFIG=another_skg_config ./bash/run-servers.sh
#     or by editing SKG_CONFIG in ./bash/run-servers.sh
# - TO SHUTDOWN
#   - If foregrounded at command line, use
#     Ctrl-C
#   - If in background at command line, run
#       ./bash/shutdown-servers.sh
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

start_skg_with_restart() {
  echo ""
  echo "Starting skg server with auto-restart on code changes..."

  # Kill any existing cargo-watch processes (fallback to broader kill since we're not tracking PID)
  pkill -f "cargo watch.*skg" 2>/dev/null || true

  echo "cargo-watch starting..."
  echo "Server logs go to $DATA_ROOT/logs/server-to-user.log and $DATA_ROOT/logs/server.jsonl"
  cargo watch -w server/ -x "run --bin skg -- $SKG_CONFIG" \
    > /dev/null 2>&1 # "-w server/" says "only watch server/"
}

cleanup() { # trap handler for graceful shutdown
  echo ""
  echo "Shutting down skg server..."

  # Kill cargo-watch and skg processes
  pkill -f "cargo watch.*skg" 2>/dev/null || true
  pkill -f "cargo run --bin skg" 2>/dev/null || true
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
echo "#!/bin/bash" >     bash/shutdown-servers.sh
echo "kill -TERM -$$" >> bash/shutdown-servers.sh
chmod +x                 bash/shutdown-servers.sh
echo "Created bash/shutdown-servers.sh - run it to stop all servers"

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
