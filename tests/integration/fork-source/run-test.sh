#!/bin/bash

# Integration test for the fork-confirmation buffer's editable clone
# source. Two OWNED sources (owned, owned2) plus a FOREIGN one.
# - Emacs opens owned P (whose content is foreign N), makes N definitive,
#   edits its title, and saves -> a fork-confirmation buffer.
# - The clone's source is inferred as "owned"; the test rotates it to
#   "owned2" in the confirmation buffer, then approves.
# - The clone must land in "owned2" (the rotated source), not "owned".

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Fork Source-Rotation Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the save mutates, so reruns start clean.
restore_fork_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/fork-source/data/owned" \
    "tests/integration/fork-source/data/owned2" \
    "tests/integration/fork-source/data/foreign" 2>/dev/null || true
  # Remove any clone .skg written into either owned source.
  find "$TEST_DIR/data/owned"  -name '*.skg' ! -name 'P.skg' -delete 2>/dev/null || true
  find "$TEST_DIR/data/owned2" -name '*.skg' -delete 2>/dev/null || true
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
name = "owned2"
path = "$TEST_DIR/data/owned2"
user_owns_it = true

[[sources]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
user_owns_it = false
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
