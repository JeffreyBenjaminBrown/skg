#!/bin/bash

# Integration test for cycling a view's perspective, whose overrides it
# follows ('docs/api-and-formats.org', "Set overrides followed").
# Foreign R contains M and K; A, also foreign, U, owned, and B, of a
# second foreign owner, "third", each override M. The client opens R,
# which shows A in M's place; switches the view to the next
# perspective, the user's, which shows U there; is refused the switch
# while the buffer has unsaved edits; saves, which keeps the
# perspective; switches to the next, third's, which shows B; and to
# the next, which is the root authors' again. The data is copied to a
# scratch folder first, so the save writes nothing tracked.

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Override Perspective Integration Test ==="

WORK_DIR=$(mktemp -d "${TMPDIR:-/tmp}/skg-overrides-followed-XXXXXX")
cp -r "$TEST_DIR/data/foreign" "$TEST_DIR/data/owned" "$TEST_DIR/data/third" "$WORK_DIR/"
trap 'cleanup; rm -rf "$WORK_DIR"' EXIT

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG="$WORK_DIR/skgconfig.toml"
cat > "$TEMP_CONFIG" << EOF2
tantivy_folder = "$WORK_DIR/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

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
EOF2

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
