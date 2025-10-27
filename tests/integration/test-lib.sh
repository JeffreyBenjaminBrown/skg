#!/bin/bash

# Common library for skg integration tests
# This file should be sourced by individual test runners

# Function to check if TypeDB server is running
is_typedb_running() {
  if pgrep -f 'typedb_server_bin' >/dev/null 2>&1; then
    return 0
  else
    return 1
  fi
}

# Function to verify TypeDB server is running and exit if not
check_typedb_server() {
  echo ""
  echo "Checking TypeDB server..."
  if ! is_typedb_running; then
    echo "ERROR: TypeDB server is not running!"
    echo "Please start TypeDB server first by running: ./run-servers.sh"
    exit 1
  fi
  echo "✓ TypeDB server is running"
}

# Function to send shutdown command to skg server via Emacs
send_shutdown_command() {
  local port=$1
  echo "Sending shutdown command to server on port $port..."

  # Create temporary Emacs Lisp script to send shutdown
  local shutdown_script=$(mktemp)
  cat > "$shutdown_script" << 'EOF'
;; Send shutdown command to skg server
(require 'cl-lib)

(let* ((port (string-to-number (getenv "SKG_TEST_PORT")))
       (tcp-proc (make-network-process
                  :name "skg-shutdown"
                  :host "127.0.0.1"
                  :service port
                  :family 'ipv4)))
  (process-send-string tcp-proc "((request . \"shutdown\"))\n")
  (sleep-for 0.083)
  (delete-process tcp-proc))
EOF

  # Run the script with a timeout to prevent hanging
  timeout 2 env SKG_TEST_PORT=$port emacs --batch -l "$shutdown_script" 2>/dev/null || true
  rm -f "$shutdown_script"
  sleep 0.1
}

# Function to cleanup background processes and test databases
cleanup() {
  echo ""
  echo "Cleaning up..."
  rm -f "$TEMP_CONFIG"

  if [ -n "$CARGO_PID" ]; then
    # Check if process is still running
    if kill -0 $CARGO_PID 2>/dev/null; then
      echo "Stopping server process (PID: $CARGO_PID)"

      # Use SIGINT (not SIGTERM) for graceful shutdown
      # The Rust server only handles SIGINT (Ctrl+C), not SIGTERM
      # This triggers the delete_on_quit cleanup code
      kill -INT $CARGO_PID 2>/dev/null || true

      # Wait for graceful shutdown (delete_on_quit needs time to clean up database)
      local wait_count=0
      while [ $wait_count -lt 20 ] && kill -0 $CARGO_PID 2>/dev/null; do
        sleep 0.1
        wait_count=$((wait_count + 1))
      done

      # If still running after 2 seconds, force kill
      if kill -0 $CARGO_PID 2>/dev/null; then
        echo "Force killing server process (PID: $CARGO_PID)"
        kill -9 $CARGO_PID 2>/dev/null || true
      fi

      wait $CARGO_PID 2>/dev/null || true
    fi
  fi

  # Do NOT manually delete test databases here - it causes TypeDB to crash
  # when it tries to checkpoint deleted databases. The delete_on_quit flag
  # in the test config handles cleanup during graceful shutdown (via SIGINT).
}

# Function to find available port by trying random ports in range
find_available_port() {
  local attempts=0
  local max_attempts=100
  local min_port=1000
  local max_port=9000

  while [ $attempts -lt $max_attempts ]; do
    # Generate random port in the range 1000-9000
    local port=$(( min_port + RANDOM % (max_port - min_port + 1) ))

    # Check if port is available using ss (socket statistics)
    if ! ss -tln 2>/dev/null | grep -q ":$port "; then
      echo $port
      return 0
    fi

    attempts=$(( attempts + 1 ))
  done

  echo "ERROR: No available ports found after $max_attempts attempts" >&2
  exit 1
}

# Function to create unique database name for parallel tests
generate_db_name() {
  local test_name=$(basename "$TEST_DIR")
  local timestamp=$(date +%s)
  local random_id=$((RANDOM % 1000))
  echo "skg-test-${test_name}-${timestamp}-${random_id}"
}

# Function to start skg server with test config
start_skg_server() {
  echo ""
  echo "Starting skg server (direct binary) with test config..."
  cd "$PROJECT_ROOT"
  # Use the pre-built binary directly instead of cargo run to avoid lock contention
  target/debug/skg "$TEMP_CONFIG" > "$TEST_DIR/server.log" 2>&1 &
  CARGO_PID=$!
  echo "✓ Started skg server (PID: $CARGO_PID) with config: $TEMP_CONFIG"
  echo "  Server logs: $TEST_DIR/server.log"
  echo "Waiting for server to be ready..."

  # Wait for server to actually start listening on the port
  # This is especially important when tests run in parallel and compete for cargo locks
  local max_attempts=100
  local attempt=0
  while [ $attempt -lt $max_attempts ]; do
    # Use ss (socket statistics) which is more commonly available than netstat
    if ss -tln 2>/dev/null | grep -q ":$AVAILABLE_PORT "; then
      echo "✓ Server is listening on port $AVAILABLE_PORT"
      return 0
    fi
    sleep 0.2
    attempt=$(( attempt + 1 ))
  done

  echo "ERROR: Server failed to start listening on port $AVAILABLE_PORT after 20 seconds"
  echo "Last 30 lines of server log:"
  tail -30 "$TEST_DIR/server.log"
  return 1
}

# Function to clean up Tantivy index contents
cleanup_tantivy_index() {
  local tantivy_dir="$1"
  if [ -d "$tantivy_dir" ]; then
    rm -f "$tantivy_dir"/* 2>/dev/null || true
    echo "✓ Cleaned up Tantivy index contents"
  fi
}

# Function to run Emacs integration test
run_emacs_test() {
  local test_file="$1"
  echo ""
  echo "Running Emacs integration test..."
  cd "$TEST_DIR"
  if SKG_TEST_PORT="$AVAILABLE_PORT" emacs --batch -l "$test_file"; then
    echo "✓ Integration test PASSED."
    TEST_RESULT=0
  else
    echo "✗ Integration test FAILED."
    TEST_RESULT=1
  fi
}
