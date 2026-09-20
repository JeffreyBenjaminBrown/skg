#!/bin/bash

# Integration test for the fork gesture and its confirmation stage.
# - Starts an independent skg server with an OWNED + a FOREIGN source.
# - Emacs opens foreign F as a root and structurally edits its content;
#   Neovim exercises the simpler foreign-child-under-owned-P gesture.
# - The save returns a fork-confirmation buffer (nothing committed).
# - Approval creates the clone and immediately substitutes it into the
#   buffer where the gesture occurred, including when the origin is a root.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Fork Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the save mutates, so reruns start clean.
restore_fork_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/fork/data/owned" \
    "tests/integration/fork/data/foreign" 2>/dev/null || true
  # Remove any clone .skg the test wrote into the owned source.
  find "$TEST_DIR/data/owned/owned" -name '*.skg' ! -name 'P.skg' -delete 2>/dev/null || true
}
trap 'cleanup; restore_fork_fixtures' EXIT
restore_fork_fixtures


AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the owned/ folder
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[sources]]
name = "owned"
path = "$TEST_DIR/data/owned/owned"

[[sources]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
