#!/bin/bash

# Integration test for skg title-matches functionality
# This script:
# - Starts an independent cargo run process with test config
# - Uses Emacs to send title-matches request for "apples"
# - Tests that Emacs receives search results and can visit links

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Repo common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Title Matches Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

trap cleanup EXIT


make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
