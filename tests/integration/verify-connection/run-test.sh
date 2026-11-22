#!/bin/bash

# Integration test for skg verify-connection functionality
# This script:
# - Starts an independent cargo run process with test config
# - Uses Emacs to send the verify connection request
# - Tests that Emacs receives the expected result

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Repo common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Verify Connection Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"


# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Set up cleanup trap
trap cleanup EXIT


make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$TEST_DIR/data/owned/skg"
EOF

run_server_and_client_test_then_exit
