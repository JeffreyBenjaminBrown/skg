#!/bin/bash

# Integration test: literal search of titles containing Tantivy
# operator characters ('+', '[', ']', ':', ...).
# Fixtures: three nodes whose titles exercise different specials.
# Each is queried by its literal title text; the test verifies
# the node's id appears in the search buffer.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Search Escape-Chars Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

trap cleanup EXIT


make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

run_server_and_client_test_then_exit
