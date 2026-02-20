#!/bin/bash

# Benchmark test: times the full skg pipeline at scale (~781 nodes).
#
# Usage: bash run-test.sh
#
# Generates benchmark data, starts its own server on a random port,
# runs the Emacs benchmark, prints timing results, and cleans up.
#
# Requires: TypeDB server running, project built (cargo build).

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../.." && pwd)"

source "$PROJECT_ROOT/tests/integration/test-lib.sh"

echo "=== SKG Benchmark Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

DATA_DIR="$TEST_DIR/data/skg-data"
TANTIVY_DIR="$TEST_DIR/data/index.tantivy"

# Generate test data
echo ""
echo "Generating benchmark data..."
rm -rf "$DATA_DIR"
mkdir -p "$DATA_DIR"
mkdir -p "$TANTIVY_DIR"
bash "$TEST_DIR/generate-data.sh" "$DATA_DIR"

# Cleanup
benchmark_cleanup() {
  echo ""
  echo "=== Cleaning up benchmark data ==="
  rm -rf "$DATA_DIR"
  cleanup_tantivy_index "$TANTIVY_DIR"
  cleanup  # from test-lib.sh (kills server, removes TEMP_CONFIG)
}

trap benchmark_cleanup EXIT

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for benchmark server..."

TEMP_CONFIG=$(mktemp)
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TANTIVY_DIR"
port = $AVAILABLE_PORT
delete_on_quit = true
initial_node_limit = 5000
timing_log = true

[[sources]]
nickname = "main"
path = "$DATA_DIR"
user_owns_it = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

# Copy and print server-side timing results
TIMING_LOG="$(dirname "$TEMP_CONFIG")/timing.log"
LOCAL_TIMING_LOG="$TEST_DIR/data/timing.log"
echo ""
echo "=== Server-side Timing (from timing.log) ==="
if [ -f "$TIMING_LOG" ]; then
  cp "$TIMING_LOG" "$LOCAL_TIMING_LOG"
  cat "$LOCAL_TIMING_LOG"
  echo ""
  echo "Timing log saved to: $LOCAL_TIMING_LOG"
else
  echo "(timing log not found at $TIMING_LOG)"
fi

echo ""
echo "=== Benchmark Complete ==="
exit $TEST_RESULT
