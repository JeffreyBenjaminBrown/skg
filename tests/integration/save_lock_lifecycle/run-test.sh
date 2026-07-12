#!/bin/bash

# Integration test for the save-lock lifecycle (plan_v2 §20.2a).
# This script:
# - Backs up and resets test data (a.skg, b.skg, solo.skg) to a clean state
# - Verifies TypeDB server is running
# - Starts an independent skg server with test config
# - Uses Emacs to open three overlapping views, save one, and verify that the
#   truly-non-collateral view becomes editable once the save stream settles
#   (the relax step narrowed the lock; the terminal save-result cleared the rest)
# - Cleans up test artifacts and restores original state

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Save-Lock Lifecycle Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

backup_and_reset_test_data() {
    echo "=== Backing up and resetting test data ==="
    for file in a b solo; do
        if [ -f "$TEST_DIR/data/owned/skg-data/$file.skg" ]; then
            BACKUP_FILE="$TEST_DIR/data/owned/skg-data/$file.skg.backup.$(date +%s)"
            cp "$TEST_DIR/data/owned/skg-data/$file.skg" "$BACKUP_FILE"
            echo "✓ Backed up $file.skg to $(basename "$BACKUP_FILE")"
        fi
    done

    # a contains b (so a view of b shows a as containerward -> b is collateral
    # to saving a). solo is standalone (truly non-collateral to saving a).
    cat > "$TEST_DIR/data/owned/skg-data/a.skg" << 'EOF'
title: a
pid: a
contains:
  - b
EOF
    echo "✓ Reset a.skg to clean state"

    cat > "$TEST_DIR/data/owned/skg-data/b.skg" << 'EOF'
title: b
pid: b
contains: []
EOF
    echo "✓ Reset b.skg to clean state"

    cat > "$TEST_DIR/data/owned/skg-data/solo.skg" << 'EOF'
title: solo
pid: solo
contains: []
EOF
    echo "✓ Reset solo.skg to clean state"
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
    for file in a b solo; do
        BACKUP_FILE=$(find "$TEST_DIR/data/owned/skg-data" -name "$file.skg.backup.*" | head -1)
        if [ -n "$BACKUP_FILE" ] && [ -f "$BACKUP_FILE" ]; then
            cp "$BACKUP_FILE" "$TEST_DIR/data/owned/skg-data/$file.skg"
            echo "✓ Restored $file.skg from backup"
        fi
        find "$TEST_DIR/data/owned/skg-data" -name "$file.skg.backup.*" -delete
    done
}

enhanced_cleanup() {
    cleanup_test_data
    cleanup  # Call original cleanup from test-lib.sh
}

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
