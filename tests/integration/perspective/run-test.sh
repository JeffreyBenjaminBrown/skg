#!/bin/bash

# Integration test for cycling a view's perspective
# ('docs/api-and-formats.org', "Set perspective").
# R, in foreign skgrepo "foreign", contains M, in foreign skgrepo
# "origin", and K; A, in "foreign", U, in owned "main", and B, in
# foreign "third", each fork M. The client opens R, drawn from R's
# home, "foreign", which shows A in M's place; switches the view to
# the next perspective, main's, which shows U there; is refused the
# switch while the buffer has unsaved edits; saves, which keeps the
# perspective; switches to the next, third's, which shows B; to the
# next, origin's, which shows M itself; and to the next, which is the
# root homes again. Then it opens M and cycles its root through
# foreign's, main's and third's forks of it, and back. The data is
# copied to a scratch folder first, so the save writes nothing
# tracked.

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Perspective Integration Test ==="

WORK_DIR=$(mktemp -d "${TMPDIR:-/tmp}/skg-perspective-XXXXXX")
cp -r "$TEST_DIR/data/foreign" "$TEST_DIR/data/owned" "$TEST_DIR/data/third" "$TEST_DIR/data/origin" "$WORK_DIR/"
trap 'cleanup; rm -rf "$WORK_DIR"' EXIT

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG="$WORK_DIR/skgconfig.toml"
write_test_config "$WORK_DIR/.index.tantivy" << EOF
[[repos]]
name = "foreign"
path = "$WORK_DIR/foreign"
foreign = true

[[repos]]
name = "main"
path = "$WORK_DIR/owned/main"

[[repos]]
name = "third"
path = "$WORK_DIR/third"
foreign = true

[[repos]]
name = "origin"
path = "$WORK_DIR/origin"
foreign = true
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
