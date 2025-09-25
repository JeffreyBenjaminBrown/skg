#!/bin/bash

# Integration test for skg verify-connection functionality
# This script:
# - Verifies TypeDB server is running
# - Starts an independent cargo run process with test config
# - Uses Emacs to send the verify connection request
# - Tests that Emacs receives the expected result

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

echo "=== SKG Verify Connection Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Function to check if TypeDB server is running
is_typedb_running() {
  if pgrep -f 'typedb_server_bin' >/dev/null 2>&1; then
    return 0
  else
    return 1
  fi
}


# Function to cleanup background processes
cleanup() {
  echo ""
  echo "Cleaning up..."
  rm -f "$TEMP_CONFIG"
  if [ -n "$CARGO_PID" ]; then
    echo "Stopping cargo process (PID: $CARGO_PID)"
    kill $CARGO_PID 2>/dev/null || true
    wait $CARGO_PID 2>/dev/null || true
  fi
}

# Function to find available port starting from 1731
find_available_port() {
  local port=1731
  while netstat -tln 2>/dev/null | grep -q ":$port "; do
    port=$((port + 1))
    if [ $port -gt 1800 ]; then
      echo "ERROR: No available ports found" >&2
      exit 1
    fi
  done
  echo $port
}

# Set up cleanup trap
trap cleanup EXIT

# Verify TypeDB server is running
echo ""
echo "Step 1: Checking TypeDB server..."
if ! is_typedb_running; then
  echo "ERROR: TypeDB server is not running!"
  echo "Please start TypeDB server first by running: ./run-servers.sh"
  exit 1
fi
echo "✓ TypeDB server is running"

# Find available port and create dynamic config
AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Step 2: Using port $AVAILABLE_PORT for test server..."

# Create a dynamic config with the available port
TEMP_CONFIG=$(mktemp)
cat > "$TEMP_CONFIG" << EOF
db_name = "skg-integration-test"
skg_folder = "$TEST_DIR/data/skg"
tantivy_folder = "$TEST_DIR/data/index.tantivy"
port = $AVAILABLE_PORT
EOF

# Start independent cargo run process
echo ""
echo "Starting skg server (cargo run) with test config..."
cd "$PROJECT_ROOT"
cargo run --bin skg "$TEMP_CONFIG" > "$TEST_DIR/server.log" 2>&1 &
CARGO_PID=$!
echo "✓ Started skg server (PID: $CARGO_PID) with config: $TEMP_CONFIG"
echo "  Server logs: $TEST_DIR/server.log"

# Wait for server to start
echo "Waiting for server to be ready..."
sleep 3

# Check if server is still running
if ! kill -0 $CARGO_PID 2>/dev/null; then
  echo "ERROR: Server failed to start!"
  echo "Server logs:"
  cat "$TEST_DIR/server.log" 2>/dev/null || echo "No log file found"
  exit 1
fi

# Run Emacs test in batch mode
echo ""
echo "Step 4: Running Emacs integration test..."
cd "$TEST_DIR"
if SKG_TEST_PORT="$AVAILABLE_PORT" emacs --batch -l test-emacs.el; then
  echo "✓ Integration test PASSED."
  TEST_RESULT=0
else
  echo "✗ Integration test FAILED."
  TEST_RESULT=1
fi

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
