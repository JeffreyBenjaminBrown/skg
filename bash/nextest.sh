#!/bin/bash
set -e

# Resolve project root (parent of this script's directory)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
source "$SCRIPT_DIR/test-runner-lib.sh"

DEFAULT_NEXTEST_JOBS="$(skg_default_jobs 4)"
NEXTEST_JOBS="$(skg_positive_int_or_default "${SKG_NEXTEST_JOBS:-}" "$DEFAULT_NEXTEST_JOBS")"
NEXTEST_BUILD_JOBS="$(skg_positive_int_or_default \
  "${SKG_NEXTEST_BUILD_JOBS:-}" "$(skg_default_jobs 8)")"

# Clean up any leftover test databases from prior runs
"$PROJECT_ROOT/target/debug/cleanup-test-dbs"

# Run tests, forwarding all arguments
echo "Running nextest with $NEXTEST_JOBS concurrent test(s). Override with SKG_NEXTEST_JOBS."
# --build-jobs: without it, cargo compiles/links with every core.
# The test binaries are large (debug, ~quarter-GB each), so unbounded
# parallel linking is the heaviest load a test run puts on the
# machine; nice/ionice govern priority, not RAM.
skg_low_priority cargo nextest run --build-jobs "$NEXTEST_BUILD_JOBS" \
  --jobs "$NEXTEST_JOBS" "$@"
