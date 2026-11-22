#!/bin/bash

# Integration test: a search is answered in one message.
# Searches for "bravo" and verifies that the search buffer, when it
# first appears, already holds what the enrichment draws: leaf-b's
# containers folder, holding container-a, and the node stats.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Search In One Message Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

trap cleanup EXIT


make_temp_config_in_data_folder << EOF
[[repos]]
name = "Mr Cheese"
path = "$TEST_DIR/data/owned/skg-data"
EOF

run_server_and_client_test_then_exit
