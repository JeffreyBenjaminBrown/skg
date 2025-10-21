#!/bin/bash

# Integration test for focus and folded markers functionality
# This script:
# - Backs up and resets test data to clean state (6 .skg files)
# - Verifies TypeDB server is running
# - Starts an independent cargo run process with test config
# - Uses Emacs to test focus and folded marker addition during save
# - Cleans up any test artifacts and restores original state

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Focus and Folded Markers Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Backup and reset functions
backup_and_reset_test_data() {
    echo "=== Backing up and resetting test data ==="

    # Backup and reset each of the 6 files
    for i in 1 2 3 4 5 6; do
        if [ -f "$TEST_DIR/data/skg-data/$i.skg" ]; then
            BACKUP_FILE="$TEST_DIR/data/skg-data/$i.skg.backup.$(date +%s)"
            cp "$TEST_DIR/data/skg-data/$i.skg" "$BACKUP_FILE"
            echo "✓ Backed up $i.skg to $(basename "$BACKUP_FILE")"
        fi

        # Reset to clean state
        cat > "$TEST_DIR/data/skg-data/$i.skg" << EOF
title: "$i"
ids:
  - "$i"
contains: []
EOF
        echo "✓ Reset $i.skg to clean state"
    done
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="

    # Clean up Tantivy index contents but keep the directory
    cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"

    # Restore original files if backups exist
    for i in 1 2 3 4 5 6; do
        BACKUP_FILE=$(find "$TEST_DIR/data/skg-data" -name "$i.skg.backup.*" | head -1)
        if [ -n "$BACKUP_FILE" ] && [ -f "$BACKUP_FILE" ]; then
            cp "$BACKUP_FILE" "$TEST_DIR/data/skg-data/$i.skg"
            rm -f "$BACKUP_FILE"
            echo "✓ Restored $i.skg from backup"
        fi
    done
}

# Enhanced cleanup function
enhanced_cleanup() {
    cleanup_test_data
    cleanup  # Call original cleanup from test-lib.sh
}

trap enhanced_cleanup EXIT

# Setup test environment
backup_and_reset_test_data

check_typedb_server

# Find available port and create dynamic config
AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

# Create a dynamic config with the available port
TEMP_CONFIG=$(mktemp)
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
skg_folder = "$TEST_DIR/data/skg-data"
tantivy_folder = "$TEST_DIR/data/index.tantivy"
port = $AVAILABLE_PORT
delete_on_quit = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
