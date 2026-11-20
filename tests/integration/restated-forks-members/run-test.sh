#!/bin/bash

# Integration test: a member of a forks folder that restates its note
# is drawn with its title and body covered, '<b's title, unchanged>'
# and '<b's body, unchanged>' ('docs/forks.org', "A member that
# restates its note"), while the buffer, and so a save, keeps the real
# text. Run once per client (test-emacs.el, test-nvim.lua). The data is
# written here, fresh for each run, since the test's save changes it.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Restated Forks Members Integration Test ==="

write_data() {
  rm -rf "$TEST_DIR/data"
  mkdir -p "$TEST_DIR/data/main" "$TEST_DIR/data/.index.tantivy"
  cat > "$TEST_DIR/data/main/F.skg" << 'EOF'
pid: F
title: Same words
body: |-
  A long body
  that its forks restate.
forks:
- G
EOF
  cat > "$TEST_DIR/data/main/G.skg" << 'EOF'
pid: G
title: Same words
body: |-
  A long body
  that its forks restate.
EOF
}

remove_data() {
  rm -rf "$TEST_DIR/data"
}

trap 'cleanup; remove_data' EXIT
write_data

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the main/ folder
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "main"
path = "$TEST_DIR/data/main"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
