# USAGE:
#   Default:
#     ./bash/run-servers.sh
#       uses data/default/skgconfig.toml
#   Override, general:
#     SKG_CONFIG=another_skg_config ./bash/run-servers.sh
#   Override, my usual:
#     SKG_CONFIG=data/subscribe/skgconfig.toml ./bash/run-servers.sh
SKG_CONFIG="${SKG_CONFIG:-data/default/skgconfig.toml}"

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

is_neo4j_running() {
  if neo4j status 2>/dev/null | grep -q 'is running'; then
    return 0  # It is
  else
    return 1  # It isn't
  fi
}

start_neo4j() {
  echo ""
  echo "Starting Neo4j server..."
  neo4j start
  echo "Waiting for Neo4j server to be ready..."
  for i in {1..30}; do
    if is_neo4j_running; then
      echo "Neo4j server is ready."
      return 0
    fi
    sleep 1
  done
  echo "Warning: Neo4j server may not be ready yet"
  return 1
}

start_skg_with_restart() {
  echo ""
  echo "Starting skg server with auto-restart on code changes..."

  # Kill any existing cargo-watch processes (fallback to broader kill since we're not tracking PID)
  pkill -f "cargo watch.*skg" 2>/dev/null || true

  echo "Cargo output will be logged to logs/skg.log"
  echo "cargo-watch starting..."
  cargo watch -x "run --bin skg -- $SKG_CONFIG" > logs/skg.log 2>&1
}

cleanup() { # trap handler for graceful shutdown
  echo ""
  echo "Shutting down skg server..."

  # Kill cargo-watch and skg processes
  pkill -f "cargo watch.*skg" 2>/dev/null || true
  pkill -f "cargo run --bin skg" 2>/dev/null || true
  sleep 1  # Give TypeDB a second to shutdown
  if is_neo4j_running; then
    echo "Neo4j server was still running (a second after start-servers.sh initiated cleanup)."
  else
    echo "Neo4j server has shut down"
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
echo "#!/bin/bash" >     bash/kill-servers.sh
echo "kill -TERM -$$" >> bash/kill-servers.sh
chmod +x                 bash/kill-servers.sh
echo "Created bash/kill-servers.sh - run it to stop all servers"

# Check and start Neo4j server if needed
if is_neo4j_running; then
  echo "Neo4j server is already running"
else
  start_neo4j
fi

# Start skg server with auto-restart
start_skg_with_restart
