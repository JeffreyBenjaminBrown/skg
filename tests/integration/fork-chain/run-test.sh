#!/bin/bash

# Integration test for the motivating chain-of-forks case: fork a
# foreign node into a public clone, then
# explicitly fork that clone (the shown substitute) into a private clone.
# The result is an owned chain D forks C forks N; viewing N's
# container shows the chain end D, marked (inPlaceOf N), and the save
# accepts the chain-end carrier.
#
# Repos: public + private (owned) and foreign.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Chain-of-Forks (fork-of-a-fork) Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the save mutates, so reruns start clean.
restore_chain_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/fork-chain/data/public" \
    "tests/integration/fork-chain/data/foreign" 2>/dev/null || true
  # Remove any clones the test wrote into the owned skgrepos.
  find "$TEST_DIR/data/owned/public" -name '*.skg' ! -name 'P.skg' -delete 2>/dev/null || true
  find "$TEST_DIR/data/owned/private" -name '*.skg' -delete 2>/dev/null || true
}
trap 'cleanup; restore_chain_fixtures' EXIT
restore_chain_fixtures


AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the owned/ folder
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
foreign = true

[[repos]]
name = "public"
path = "$TEST_DIR/data/owned/public"

[[repos]]
name = "private"
path = "$TEST_DIR/data/owned/private"
EOF

start_skg_server

run_client_test

# Disk verification (step 5): the public clone C forks the foreign N
# and names no private id; the private clone D forks C.
echo "Verifying clone .skg files on disk..."
PUB_CLONE=$(find "$TEST_DIR/data/owned/public" -name '*.skg' ! -name 'P.skg' | head -1)
PRIV_CLONE=$(find "$TEST_DIR/data/owned/private" -name '*.skg' | head -1)
if [ -z "$PUB_CLONE" ] || [ -z "$PRIV_CLONE" ]; then
  echo "✗ FAIL: expected one clone in public and one in private"
  echo "  public: $PUB_CLONE   private: $PRIV_CLONE"
  exit 1
fi
PRIV_ID=$(basename "$PRIV_CLONE" .skg)
if grep -q "$PRIV_ID" "$PUB_CLONE"; then
  echo "✗ FAIL: the public clone names the private id $PRIV_ID:"
  cat "$PUB_CLONE"
  exit 1
fi
if ! grep -q "^forks:" "$PUB_CLONE" || ! grep -q "N" "$PUB_CLONE"; then
  echo "✗ FAIL: the public clone should fork the foreign N:"
  cat "$PUB_CLONE"
  exit 1
fi
echo "✓ public clone forks N and names no private id"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
