#!/bin/bash

# Integration test: search enrichment (background containerward paths + graphnodestats)
# Searches for "leaf bravo", verifies the search buffer appears,
# then waits for the background enrichment to add the containerward
# path from leaf-b back to container-a, with graphnodestats.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Search Enrichment Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

trap cleanup EXIT


AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the owned/ folder
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[sources]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
