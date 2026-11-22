#!/bin/bash

# Integration test for the herald-rules endpoint.
# This script:
# - Starts an independent skg server with test config
# - Uses Emacs to fetch the herald rule table over TCP
# - Tests that the table installs into the heralds cache, and that the
#   skgrepo label table served beside it labels a repo boundary

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Repo common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Herald Rules Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Set up cleanup trap
trap cleanup EXIT


make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
abbreviation = "m"
path = "$TEST_DIR/data/owned/skg"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
