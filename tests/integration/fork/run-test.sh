#!/bin/bash

# Integration test for the fork gesture and its confirmation stage.
# - Starts an independent skg server with an OWNED + a FOREIGN source.
# - Emacs opens the owned container P (whose content is the foreign N),
#   makes N definitive, edits its title, and saves.
# - The save returns a fork-confirmation buffer (nothing committed).
# - Emacs approves; the clone is created (overrides N) and drawn in N's
#   place in the re-rendered P.

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
  find "$TEST_DIR/data/owned" -name '*.skg' ! -name 'P.skg' -delete 2>/dev/null || true
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

[[sources]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
user_owns_it = false
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
