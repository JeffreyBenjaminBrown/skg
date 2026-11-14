#!/bin/bash

# Integration test: the diff view's motions between changes, end to
# end ('docs/diff-view.org', "Marking changes processed", the six
# motions). Jeff's requirement: in a diff view extended to every
# change, the plain motion (C-c r n c) reaches every change, though it
# skips a write-protected member of a folder of other files'
# relationships.
#
# One owned skgrepo, main. At HEAD m contains a; x contains a; r2, s
# and y stand alone. In the worktree:
#   - m is deleted (a's containers change, and m's ghost is a root);
#   - r2 contains a (a's containers change again);
#   - s subscribes to a (a's subscribers change);
#   - y is retitled and its body mentions a (a's mentioners change).
# So a, under m's ghost, shows its containers, mentioners and
# subscribers folders, each holding a write-protected member whose
# change is drawn where that member's node stands editable.
#
# The data is generated here, in data/, and removed afterward.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Diff View Motions Integration Test ==="

DATA="$TEST_DIR/data"
MAIN="$DATA/owned/main"

set_up_test_data () {
    echo "=== Generating the gitrepo ==="
    rm -rf "$DATA"
    mkdir -p "$MAIN"
    printf 'pid: x\ntitle: x\ncontains:\n- a\n' > "$MAIN/x.skg"
    printf 'pid: a\ntitle: a\n' > "$MAIN/a.skg"
    printf 'pid: m\ntitle: m\ncontains:\n- a\n' > "$MAIN/m.skg"
    printf 'pid: r2\ntitle: r2\n' > "$MAIN/r2.skg"
    printf 'pid: s\ntitle: s\n' > "$MAIN/s.skg"
    printf 'pid: y\ntitle: y\n' > "$MAIN/y.skg"
    ( cd "$MAIN"
      git init -q
      git config user.email "test@test.com"
      git config user.name "Test"
      git add *.skg
      git commit -q -m "Initial commit" )
    echo "=== Changing the worktree ==="
    rm "$MAIN/m.skg"
    printf 'pid: r2\ntitle: r2\ncontains:\n- a\n' > "$MAIN/r2.skg"
    printf 'pid: s\ntitle: s\nsubscribesTo:\n- a\n' > "$MAIN/s.skg"
    printf 'pid: y\ntitle: y2\nbody: |-\n  see [[id:a][a]]\n' > "$MAIN/y.skg"
    echo "✓ Test data ready"
}

enhanced_cleanup() {
    cleanup
    rm -rf "$DATA"
}

trap enhanced_cleanup EXIT

set_up_test_data

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG="$DATA/skgconfig.toml"
cat > "$TEMP_CONFIG" << EOF2
tantivy_folder = ".index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "main"
path = "owned/main"
EOF2

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
