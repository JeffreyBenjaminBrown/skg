#!/bin/bash

# Common library for skg profiling tests.
# Sets up benchmark data, server, runs the Emacs test, and collects timing.
#
# Expects $TEST_DIR to be set by the caller (the per-test run-test.sh).
# Provides: run_profiling_test()

run_profiling_test() {
  set -e

  PROFILING_DIR="$(cd "$TEST_DIR/.." && pwd)"
  PROJECT_ROOT="$(cd "$PROFILING_DIR/../.." && pwd)"

  source "$PROJECT_ROOT/tests/integration/test-lib.sh"

  echo "=== SKG Benchmark Test ==="
  echo "Test directory: $TEST_DIR"
  echo "Project root: $PROJECT_ROOT"

  BENCH_TMPDIR=$(mktemp -d)
  DATA_DIR="$BENCH_TMPDIR/skg-data"
  TANTIVY_DIR="$BENCH_TMPDIR/index.tantivy"

  # Generate test data
  echo ""
  echo "Generating benchmark data..."
  mkdir -p "$DATA_DIR"
  mkdir -p "$TANTIVY_DIR"
  bash "$PROFILING_DIR/generate-data.sh" "$DATA_DIR"

  # Cleanup
  benchmark_cleanup() {
    echo ""
    echo "=== Cleaning up benchmark data ==="
    rm -rf "$BENCH_TMPDIR"
    cleanup  # from test-lib.sh (kills server, removes TEMP_CONFIG)
  }

  trap benchmark_cleanup EXIT

  check_neo4j_server

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
  LOCAL_TIMING_LOG="$TEST_DIR/timing.log"
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
}
