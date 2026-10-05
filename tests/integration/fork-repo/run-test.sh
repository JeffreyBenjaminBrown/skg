#!/bin/bash

# Integration test for the fork-confirmation buffer's editable clone
# skgrepo. Two OWNED skgrepos (owned, owned2) plus a FOREIGN one.
# - Emacs opens owned P (whose content is foreign N), makes N editable,
#   edits its title, and saves -> a fork-confirmation buffer.
# - The clone's skgrepo is inferred as "owned"; the test rotates it to
#   "owned2" in the confirmation buffer, then approves.
# - The clone must land in "owned2" (the rotated skgrepo), not "owned".

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Fork Repo-Rotation Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the save mutates, so reruns start clean.
restore_fork_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/fork-repo/data/owned" \
    "tests/integration/fork-repo/data/owned2" \
    "tests/integration/fork-repo/data/foreign" 2>/dev/null || true
  # Remove any clone .skg written into either owned skgrepo.
  find "$TEST_DIR/data/owned/owned"  -name '*.skg' ! -name 'P.skg' -delete 2>/dev/null || true
  find "$TEST_DIR/data/owned/owned2" -name '*.skg' -delete 2>/dev/null || true
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

[[repos]]
name = "owned"
path = "$TEST_DIR/data/owned/owned"

[[repos]]
name = "owned2"
path = "$TEST_DIR/data/owned/owned2"

[[repos]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
