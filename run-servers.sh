#!/bin/bash

# USAGE:
#   Start
#     Run from command line, probably in background.
#   End
#     If foregrounded at command line, use
#       Ctrl-C
#     If in background at command line,
#       use
#         kill -TERM -PID # kills the whole group
#       The PID
#         is printed when it starts,
#         and if buried can be found running `ps -af`.

is_typedb_running() {
  if pgrep -f 'typedb_server_bin' >/dev/null 2>&1; then
    return 0  # It is
  else
    return 1  # It isn't
  fi
}

start_typedb() {
  echo ""
  echo "Starting TypeDB server..."
  nohup typedb server > logs/typedb.log 2>&1 &
  echo "TypeDB server started in background (PID: $!)"
  echo "Waiting for TypeDB server to be ready..."
  for i in {1..30}; do
    # Wait for TypeDB to be ready (max 30 seconds)
    if is_typedb_running; then
      echo "TypeDB server is ready, and logging to logs/typedb.log."
      return 0
    fi
    sleep 1
  done
  echo "Warning: TypeDB server may not be ready yet"
  return 1
}

start_skg_with_restart() {
  echo ""
  echo "Starting skg server with auto-restart on code changes..."

  # Kill any existing cargo-watch processes (fallback to broader kill since we're not tracking PID)
  pkill -f "cargo watch.*skg" 2>/dev/null || true

  echo "Cargo output will be logged to logs/skg.log"
  echo "cargo-watch starting..."
  cargo watch -x "run --bin skg" > logs/skg.log 2>&1
}

cleanup() { # trap handler for graceful shutdown
  echo ""
  echo "Shutting down skg server..."

  # Kill cargo-watch and skg processes
  pkill -f "cargo watch.*skg" 2>/dev/null || true
  pkill -f "cargo run --bin skg" 2>/dev/null || true
  sleep 1  # Give TypeDB a second to shutdown
  if is_typedb_running; then
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
echo "#!/bin/bash" > kill-servers.sh
echo "kill -TERM -$$" >> kill-servers.sh
chmod +x kill-servers.sh
echo "Created kill-servers.sh - run it to stop all servers"

# Check and start TypeDB server if needed
if is_typedb_running; then
  echo "TypeDB server is already running"
else
  start_typedb
fi

# Start skg server with auto-restart
start_skg_with_restart
