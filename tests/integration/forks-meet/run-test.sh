#!/bin/bash

# Integration test: forks of one note meet ('docs/forks.org', "Forks
# of one note meet"), driven through each client's own gestures.
# - Starts an independent skg server over one owned skgrepo: N
#   contains X1 to X4 and hides Z; R and T fork N; R contains X1 to
#   X3; T contains X1, X3, Y, Z and hides X2; Shelf contains N and T.
# - R's view: R's flat forks folder holds N, editable (herald bK), and
#   T, an indirect member showing Y and Z (no 'b' in its herald); the
#   containers folders' members folded. X4 is cut from N and the view
#   saved: X4 stands in N's hidden-in folder.
# - N's view: Y is cut from T and pasted into N's content, and the
#   view saved: T stands first in N's forks folder, a mutual (bKb),
#   claiming, and Y in N's content.
# The test-emacs.el and test-nvim.lua headers say what each checks.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Forks Meet Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the saves mutate, so reruns start clean.
restore_forks_meet_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/forks-meet/data/main" 2>/dev/null || true
}
trap 'cleanup; restore_forks_meet_fixtures' EXIT
restore_forks_meet_fixtures


AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the main/ folder
cat > "$TEMP_CONFIG" << EOF2
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "main"
path = "$TEST_DIR/data/main"
EOF2

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
