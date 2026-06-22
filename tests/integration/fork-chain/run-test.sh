#!/bin/bash

# Integration test for the MOTIVATING override-chain case (the crux of
# the override-chains arc): fork a FOREIGN node into a public clone, then
# explicitly fork that clone (the drawn substitute) into a PRIVATE clone.
# The result is a user-owned chain D overrides C overrides N; viewing N's
# container draws the chain end D, marked (overridesHere N), and the save
# accepts the chain-end carrier.
#
# Sources: public + private (owned) and foreign (read-only).

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Override-Chain (fork-of-a-fork) Integration Test ==="
echo "Test directory: $TEST_DIR"

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Restore the fixtures the save mutates, so reruns start clean.
restore_chain_fixtures() {
  git -C "$PROJECT_ROOT" checkout -- \
    "tests/integration/fork-chain/data/public" \
    "tests/integration/fork-chain/data/foreign" 2>/dev/null || true
  # Remove any clones the test wrote into the owned sources.
  find "$TEST_DIR/data/public" -name '*.skg' ! -name 'P.skg' -delete 2>/dev/null || true
  find "$TEST_DIR/data/private" -name '*.skg' -delete 2>/dev/null || true
}
trap 'cleanup; restore_chain_fixtures' EXIT
restore_chain_fixtures

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp)
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
delete_on_quit = true

[[sources]]
name = "public"
path = "$TEST_DIR/data/public"
user_owns_it = true

[[sources]]
name = "private"
path = "$TEST_DIR/data/private"
user_owns_it = true

[[sources]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
user_owns_it = false
EOF

start_skg_server

run_emacs_test "test-emacs.el"

# Disk verification (step 5): the public clone C overrides the foreign N
# and names NO private id; the private clone D overrides C.
echo "Verifying clone .skg files on disk..."
PUB_CLONE=$(find "$TEST_DIR/data/public" -name '*.skg' ! -name 'P.skg' | head -1)
PRIV_CLONE=$(find "$TEST_DIR/data/private" -name '*.skg' | head -1)
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
if ! grep -q "overrides_view_of" "$PUB_CLONE" || ! grep -q "N" "$PUB_CLONE"; then
  echo "✗ FAIL: the public clone should override the foreign N:"
  cat "$PUB_CLONE"
  exit 1
fi
echo "✓ public clone overrides N and names no private id"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
