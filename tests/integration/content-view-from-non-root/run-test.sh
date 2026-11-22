#!/bin/bash

# Integration test: opening a content view of a node that is itself
# contained by another node shows its containers folder as the first
# child of the view-root, and the server's response
# carries an `info` field that surfaces a minibuffer notice.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG content-view-from-non-root Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

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
