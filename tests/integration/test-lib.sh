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

# Function to cleanup background processes and test databases
cleanup() {
  echo ""
  echo "Cleaning up..."
  rm -f "$TEMP_CONFIG"
  if [ -n "$CARGO_PID" ]; then
    echo "Stopping cargo process (PID: $CARGO_PID)"
    kill $CARGO_PID 2>/dev/null || true
    wait $CARGO_PID 2>/dev/null || true
  fi

  # Clean up TypeDB test database to prevent accumulation
  if [ -n "$DB_NAME" ]; then
    echo "Removing TypeDB test database: $DB_NAME"
    rm -rf "/var/lib/typedb/core/data/$DB_NAME" 2>/dev/null || true
    rm -rf "/opt/typedb/core/server/data/$DB_NAME" 2>/dev/null || true
  fi
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

    # Check if port is available
    if ! netstat -tln 2>/dev/null | grep -q ":$port "; then
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
  echo "Starting skg server (cargo run) with test config..."
  cd "$PROJECT_ROOT"
  cargo run --bin skg "$TEMP_CONFIG" > "$TEST_DIR/server.log" 2>&1 &
  CARGO_PID=$!
  echo "✓ Started skg server (PID: $CARGO_PID) with config: $TEMP_CONFIG"
  echo "  Server logs: $TEST_DIR/server.log"
  echo "Waiting for server to be ready..."
  sleep 3
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
