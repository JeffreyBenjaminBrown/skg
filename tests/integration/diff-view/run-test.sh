#!/bin/bash

# Integration test: the diff view, end to end
# ('docs/diff-view.org').
#
# Two owned skgrepos, alpha and beta, and one foreign skgrepo, gamma,
# each its own gitrepo. At HEAD, alpha's r contains t, m, d and x; y
# is a node of alpha too. In the worktree:
#   - t is retitled (a text edit);
#   - m moves from alpha to beta (a move);
#   - d is deleted, and r no longer contains it (a deletion);
#   - x acquires y (a nodeMerge: y's file is gone, and y is an extra
#     ID of x);
#   - gamma's g is retitled (a foreign change, which the total diff
#     view only counts).
#
# The client opens the diff view, marks everything commit-ready
# through the gestures, sees "COMMIT READY", stages and commits, and
# sees the buffers keep their roots, which show no change any more. Then r's file is made to contain d again,
# by hand; the client opens a view of r, where d is an unknown node,
# and investigates it.
#
# The data is generated here, in data/, and removed afterward.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Diff View Integration Test ==="

DATA="$TEST_DIR/data"
ALPHA="$DATA/owned/alpha"
BETA="$DATA/owned/beta"
GAMMA="$DATA/foreign/gamma"

node () { # node FILE PID TITLE [CONTAINED...]
    local file="$1" pid="$2" title="$3"; shift 3
    { echo "pid: $pid"
      echo "title: $title"
      if [ $# -gt 0 ]; then
          echo "contains:"
          for c in "$@"; do echo "- $c"; done
      fi
    } > "$file"
}

init_and_commit () { # init_and_commit DIR
    ( cd "$1"
      git init -q
      git config user.email "test@test.com"
      git config user.name "Test"
      git add *.skg
      git commit -q -m "Initial commit" )
}

set_up_test_data () {
    echo "=== Generating the gitrepos ==="
    rm -rf "$DATA"
    mkdir -p "$ALPHA" "$BETA" "$GAMMA"
    node "$ALPHA/r.skg" r "the root" t m d x
    node "$ALPHA/t.skg" t "a text node"
    node "$ALPHA/m.skg" m "a mover"
    node "$ALPHA/d.skg" d "doomed"
    node "$ALPHA/x.skg" x "the acquirer"
    node "$ALPHA/y.skg" y "the acquiree"
    node "$BETA/b.skg" b "beta's root"
    node "$GAMMA/g.skg" g "a foreign node"
    for repo in "$ALPHA" "$BETA" "$GAMMA"; do init_and_commit "$repo"; done
    echo "=== Changing the worktrees ==="
    node "$ALPHA/t.skg" t "a text node, edited"
    rm "$ALPHA/m.skg"
    node "$BETA/m.skg" m "a mover"
    rm "$ALPHA/d.skg"
    node "$ALPHA/r.skg" r "the root" t m x
    rm "$ALPHA/y.skg"
    { echo "pid: x"
      echo "extra_ids:"
      echo "- y"
      echo "title: the acquirer"
    } > "$ALPHA/x.skg"
    node "$GAMMA/g.skg" g "a foreign node, edited"
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
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = ".index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "alpha"
path = "owned/alpha"

[[repos]]
name = "beta"
path = "owned/beta"

[[repos]]
name = "gamma"
path = "foreign/gamma"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
