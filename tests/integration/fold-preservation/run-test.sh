#!/bin/bash

# Integration test for fold preservation functionality
# This script:
# - Starts an independent cargo run process with test config
# - Uses Emacs to create a buffer with folded content
# - Tests that folding is preserved after save

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Repo common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Fold Preservation Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Cleanup function for test data
cleanup_test_data() {
    echo "=== Cleaning up test data ==="

    # Clean up any UUID files created during test
    find "$TEST_DIR/data/owned/skg-data" -name "*.skg" -delete 2>/dev/null || true
    echo "✓ Cleaned up UUID files"
    rm -rf "$TEST_DIR/data/owned/skg-data/.git"

    # Clean up Tantivy index contents
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
}

# Enhanced cleanup function
enhanced_cleanup() {
    cleanup_test_data
    cleanup  # Call original cleanup from test-lib.sh
}

# Set up cleanup trap
trap enhanced_cleanup EXIT


# Find available port and create dynamic config
AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

# Create a dynamic config with the available port
TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the owned/ folder
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

# The diff view variant: the skgrepo is a gitrepo. At HEAD,
# dvroot (with a body) contains dvchild (with a body); in the worktree
# dvroot's body and dvchild's title are edited, so the "all changes"
# buffer shows both, each with its old text.
set_up_diffView_data() {
    local repo="$TEST_DIR/data/owned/skg-data"
    rm -rf "$repo/.git"
    find "$repo" -name "*.skg" -delete 2>/dev/null || true
    printf 'pid: dvroot\ntitle: the diff view root\nbody: |-\n  root body line one\n  root body line two\ncontains:\n- dvchild\n' > "$repo/dvroot.skg"
    printf 'pid: dvchild\ntitle: the diff view child\nbody: |-\n  child body line one\n  child body line two\n' > "$repo/dvchild.skg"
    ( cd "$repo"
      git init -q
      git config user.email "test@test.com"
      git config user.name "Test"
      git add -A
      git commit -q -m "Initial commit" )
    printf 'pid: dvroot\ntitle: the diff view root\nbody: |-\n  root body line one\n  root body line 2\ncontains:\n- dvchild\n' > "$repo/dvroot.skg"
    printf 'pid: dvchild\ntitle: the diff view child, edited\nbody: |-\n  child body line one\n  child body line two\n' > "$repo/dvchild.skg"
    echo "✓ Diff view data ready"
}

set_up_diffView_data

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
