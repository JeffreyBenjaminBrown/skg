#!/bin/bash

# Integration test for the EXPLICIT fork gesture (skg-fork-node).
# - Starts an independent skg server with a single OWNED source.
# - Emacs opens the owned container Q (whose content is the owned M),
#   runs skg-fork-node on M, and approves: a clone is created (overrides
#   M) and drawn in M's place when Q re-renders.
# - Also checks that forking a DIRTY buffer is refused, and that
#   DECLINING a fork strips the lingering (viewRequests fork) atom and
#   commits nothing.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Explicit-Fork (skg-fork-node) Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the save mutates, so reruns start clean.
restore_fork_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/fork-owned/data/owned" 2>/dev/null || true
  # Remove any clone .skg the test wrote into the owned source.
  find "$TEST_DIR/data/owned" -name '*.skg' \
    ! -name 'Q.skg' ! -name 'M.skg' \
    ! -name 'Q2.skg' ! -name 'M2.skg' -delete 2>/dev/null || true
}
trap 'cleanup; restore_fork_fixtures' EXIT
restore_fork_fixtures

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp)
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
delete_on_quit = true

[[sources]]
name = "owned"
path = "$TEST_DIR/data/owned"
user_owns_it = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
