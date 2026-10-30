#!/bin/bash

# Integration test for the relRepo gesture (C-c s r, <localleader>sr):
# the relRepo info request names its recorder, so the prompt offers the
# skgrepos from the relationship's default on; the chosen relRepo is
# requested, and a save records it.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Set relRepo Integration Test ==="

# The fixtures are written here and removed after, since the save
# rewrites them. r's file records its membership of k in r's home,
# public, below k's home: the leak-shaped membership the startup
# warns of and the gesture exists to move.
write_fixtures() {
  mkdir -p "$TEST_DIR/data/owned/public" "$TEST_DIR/data/owned/private" \
           "$TEST_DIR/data/owned/secret"
  cat > "$TEST_DIR/data/owned/public/r.skg" << 'SKG'
pid: r
title: r
contains:
- k
SKG
  cat > "$TEST_DIR/data/owned/private/k.skg" << 'SKG'
pid: k
title: k
SKG
}

enhanced_cleanup() {
  rm -rf "$TEST_DIR/data/owned"
  rm -f "$TEST_DIR/data/skgconfig.toml"
  rm -f "$TEST_DIR/data/telescope-warnings.org"
  rm -f "$TEST_DIR/data/.skg_init_marker"
  cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
  cleanup
}

trap enhanced_cleanup EXIT

rm -rf "$TEST_DIR/data/owned"
write_fixtures
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG="$TEST_DIR/data/skgconfig.toml"
cat > "$TEMP_CONFIG" << EOF2
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "public"
path = "$TEST_DIR/data/owned/public"

[[repos]]
name = "private"
path = "$TEST_DIR/data/owned/private"

[[repos]]
name = "secret"
path = "$TEST_DIR/data/owned/secret"
EOF2

start_skg_server

SKG_CONFIG_FILE="$TEMP_CONFIG" run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
