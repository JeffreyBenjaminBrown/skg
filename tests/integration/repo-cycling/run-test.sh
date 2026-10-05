#!/bin/bash

# Integration test for skgrepo cycling in the metadata edit buffer.
# Verifies that S-left / S-right cycle through owned skgrepos only.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Repo Cycling Integration Test ==="

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

enhanced_cleanup() {
  rm -f "$TEST_DIR/data/skgconfig.toml"
  rm -f "$TEST_DIR/data/.skg_init_marker"
  cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
  cleanup
}

trap enhanced_cleanup EXIT


AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG="$TEST_DIR/data/skgconfig.toml"
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "public"
path = "$TEST_DIR/data/owned/public"

[[repos]]
name = "personal"
path = "$TEST_DIR/data/owned/personal"

[[repos]]
name = "private"
path = "$TEST_DIR/data/owned/private"

[[repos]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
EOF

start_skg_server

SKG_CONFIG_FILE="$TEMP_CONFIG" run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
