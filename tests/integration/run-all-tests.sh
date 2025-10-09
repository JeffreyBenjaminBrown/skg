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

# List of test directories to run
TEST_DIRS=(
  "verify-connection"
  "search-for-title-and-visit-link"
  "content-view-and-save"
  "valid_and_invalid_saves"
)

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

# Start all tests in parallel
declare -a pids=()
declare -a test_results=()

for test_dir in "${TEST_DIRS[@]}"; do
  run_single_test "$test_dir" &
  pids+=($!)
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

exit $overall_result
