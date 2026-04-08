#!/bin/bash

# Master integration test runner for skg
# Runs all integration tests in parallel and aggregates results

set -e  # Exit on any error

# Get the directory of the actual script (not the symlink)
SCRIPT_SOURCE="${BASH_SOURCE[0]}"
while [ -L "$SCRIPT_SOURCE" ]; do
  SCRIPT_DIR="$(cd -P "$(dirname "$SCRIPT_SOURCE")" && pwd)"
  SCRIPT_SOURCE="$(readlink "$SCRIPT_SOURCE")"
  [[ $SCRIPT_SOURCE != /* ]] && SCRIPT_SOURCE="$SCRIPT_DIR/$SCRIPT_SOURCE"
done
INTEGRATION_DIR="$(cd -P "$(dirname "$SCRIPT_SOURCE")" && pwd)"
TESTS_LOG="$INTEGRATION_DIR/tests.log"

echo "=== SKG Integration Test Suite ==="
echo "Integration directory: $INTEGRATION_DIR"
echo "Results will be written to: $TESTS_LOG"
echo ""

# Clean up any straggler test databases from previous runs
# Uses TypeDB's API for safe deletion (doesn't require stopping TypeDB)
echo "Cleaning up straggler test databases..."
PROJECT_ROOT="$(cd "$INTEGRATION_DIR/../.." && pwd)"
"$PROJECT_ROOT/target/debug/cleanup-test-dbs"
echo ""

# Auto-discover test directories (any subdirectory with a run-test.sh)
TEST_DIRS=()
for dir in "$INTEGRATION_DIR"/*/; do
  if [ -f "$dir/run-test.sh" ]; then
    TEST_DIRS+=("$(basename "$dir")")
  fi
done

echo "Found ${#TEST_DIRS[@]} tests: ${TEST_DIRS[*]}"

# Function to run a single test in its directory
run_single_test() {
  local test_dir="$1"
  local test_path="$INTEGRATION_DIR/$test_dir"

  if [ ! -d "$test_path" ]; then
    echo "ERROR: Test directory $test_path does not exist!"
    return 1
  fi

  if [ ! -f "$test_path/run-test.sh" ]; then
    echo "ERROR: Test runner $test_path/run-test.sh does not exist!"
    return 1
  fi

  echo "Starting test: $test_dir"
  cd "$test_path"

  # Run the test and capture all output to test.log
  ./run-test.sh > test.log 2>&1
  local exit_code=$?

  if [ $exit_code -eq 0 ]; then
    echo "✓ Test $test_dir PASSED"
  else
    echo "✗ Test $test_dir FAILED (exit code: $exit_code)"
  fi

  return $exit_code
}

# Clear the master log file
> "$TESTS_LOG"

echo ""
echo "Running ${#TEST_DIRS[@]} tests in parallel..."

# Max parallel tests. Each test spins up a server (TypeDB + Tantivy init),
# so too many at once can starve the system — especially under an RT kernel.
MAX_PARALLEL=${SKG_TEST_PARALLEL:-4}

# Start tests in batches, with nice/ionice to reduce system impact.
declare -a pids=()
declare -a test_results=()

for test_dir in "${TEST_DIRS[@]}"; do
  nice -n 15 ionice -c 3 bash -c "$(declare -f run_single_test); INTEGRATION_DIR='$INTEGRATION_DIR' run_single_test '$test_dir'" &
  pids+=($!)

  # Throttle: when we hit MAX_PARALLEL, wait for one to finish.
  while [ "$(jobs -rp | wc -l)" -ge "$MAX_PARALLEL" ]; do
    sleep 0.2
  done
done

# Wait for all tests to complete and collect results
overall_result=0
for i in "${!pids[@]}"; do
  # Capture the exit code
  # and prevent set -e from killing the script
  set +e  # Temporarily disable exit on error
  wait "${pids[$i]}"
  exit_code=$?
  set -e  # Re-enable exit on error
  test_results+=($exit_code)
  if [ $exit_code -ne 0 ]; then
    overall_result=1
  fi
done

echo ""
echo "All tests completed. Aggregating results..."

# Aggregate all test logs into the master log file
echo "# SKG Integration Test Results" >> "$TESTS_LOG"
echo "" >> "$TESTS_LOG"
echo "Generated on: $(date)" >> "$TESTS_LOG"
echo "" >> "$TESTS_LOG"

for i in "${!TEST_DIRS[@]}"; do
  test_dir="${TEST_DIRS[$i]}"
  exit_code="${test_results[$i]}"
  test_log="$INTEGRATION_DIR/$test_dir/test.log"

  echo "## Test: $test_dir" >> "$TESTS_LOG"
  echo "" >> "$TESTS_LOG"

  if [ $exit_code -eq 0 ]; then
    echo "**Status: PASSED** ✓" >> "$TESTS_LOG"
  else
    echo "**Status: FAILED** ✗ (exit code: $exit_code)" >> "$TESTS_LOG"
  fi

  echo "" >> "$TESTS_LOG"
  echo "\`\`\`" >> "$TESTS_LOG"

  if [ -f "$test_log" ]; then
    cat "$test_log" >> "$TESTS_LOG"
  else
    echo "ERROR: Log file $test_log not found!" >> "$TESTS_LOG"
  fi

  echo "\`\`\`" >> "$TESTS_LOG"
  echo "" >> "$TESTS_LOG"
  echo "---" >> "$TESTS_LOG"
  echo "" >> "$TESTS_LOG"
done

# Print summary
echo ""
echo "=== Test Suite Summary ==="
for i in "${!TEST_DIRS[@]}"; do
  test_dir="${TEST_DIRS[$i]}"
  exit_code="${test_results[$i]}"

  if [ $exit_code -eq 0 ]; then
    echo "  ✓ $test_dir: PASSED"
  else
    echo "  ✗ $test_dir: FAILED (exit code: $exit_code)"
  fi
done

echo ""
if [ $overall_result -eq 0 ]; then
  echo "🎉 All tests PASSED!"
else
  echo "💥 Some tests FAILED!"
fi

echo ""
echo "Complete results available in: $TESTS_LOG"

# PITFALL: Manual DB deletion here would cause TypeDB to crash
# (when it tries to checkpoint deleted databases).
# But test DBs *should* be cleaned up by each test server,
# due to delete_on_quit = true in their configs.

# Report any straggler test databases that leaked.
token=$(curl -s -X POST http://127.0.0.1:8000/v1/signin \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"password"}' 2>/dev/null \
  | grep -oP '"token"\s*:\s*"\K[^"]+' 2>/dev/null) || true
if [ -n "$token" ]; then
  stragglers=$(curl -s http://127.0.0.1:8000/v1/databases \
    -H "Authorization: Bearer $token" 2>/dev/null \
    | grep -oP '"name"\s*:\s*"\Kskg-test[^"]*' 2>/dev/null) || true
  if [ -n "$stragglers" ]; then
    count=$(echo "$stragglers" | wc -l)
    echo ""
    echo "⚠  $count straggler test database(s) leaked:"
    echo "$stragglers" | sed 's/^/  - /'
    echo "  Run: ./target/debug/cleanup-test-dbs"
  fi
fi

exit $overall_result
