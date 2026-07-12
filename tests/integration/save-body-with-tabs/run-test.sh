#!/bin/bash

# Integration test: verify that saving a content view whose body
# contains both real newlines and tab characters produces a YAML
# block-literal (|-) on disk — not a double-quoted single line full
# of "\n" escape sequences.
#
# Bug before the fix: serde_yaml's default emitter falls back to
# a double-quoted scalar whenever the body contains any tab, so
# tab-bearing bodies landed on disk as unreadable one-liners.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Save-Body-With-Tabs Integration Test ==="
echo "Test directory: $TEST_DIR"

FIXTURES="bwt-root"

backup_and_reset_test_data() {
    echo "=== Backing up test data ==="
    for f in $FIXTURES; do
        local src="$TEST_DIR/data/owned/skg-data/$f.skg"
        if [ -f "$src" ]; then
            cp "$src" "$src.backup.$(date +%s)"
            echo "✓ Backed up $f.skg"
        fi
    done
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
    for f in $FIXTURES; do
        local bak
        bak=$(find "$TEST_DIR/data/owned/skg-data" -name "$f.skg.backup.*" 2>/dev/null | head -1)
        if [ -n "$bak" ] && [ -f "$bak" ]; then
            cp "$bak" "$TEST_DIR/data/owned/skg-data/$f.skg"
            echo "✓ Restored $f.skg"
        fi
        find "$TEST_DIR/data/owned/skg-data" -name "$f.skg.backup.*" -delete 2>/dev/null || true
    done
}

enhanced_cleanup() { cleanup_test_data; cleanup; }
trap enhanced_cleanup EXIT

backup_and_reset_test_data
check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the owned/ folder
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
delete_on_quit = true

[[sources]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
