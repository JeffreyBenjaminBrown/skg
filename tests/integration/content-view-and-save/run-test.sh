#!/bin/bash

# Integration test for skg content view and save functionality
# This script:
# - Backs up and resets test data to clean state
# - Verifies TypeDB server is running
# - Starts an independent cargo run process with test config
# - Uses Emacs to test content view creation and buffer saving
# - Cleans up any test artifacts and restores original state

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Content View and Save Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Backup and reset functions
backup_and_reset_test_data() {
    echo "=== Backing up and resetting test data ==="

    # Clean up any existing UUID files from previous test runs
    find "$TEST_DIR/data/skg-data" -name "*.skg" ! -name "1.skg" -delete 2>/dev/null || true
    echo "✓ Cleaned up existing UUID files"

    # Backup original 1.skg file
    if [ -f "$TEST_DIR/data/skg-data/1.skg" ]; then
        BACKUP_FILE="$TEST_DIR/data/skg-data/1.skg.backup.$(date +%s)"
        cp "$TEST_DIR/data/skg-data/1.skg" "$BACKUP_FILE"
        echo "✓ Backed up 1.skg to $(basename "$BACKUP_FILE")"
    fi

    # Reset 1.skg to clean state
    cat > "$TEST_DIR/data/skg-data/1.skg" << 'EOF'
title: "1"
ids:
  - "1"
contains: []
EOF
    echo "✓ Reset 1.skg to clean state"
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="

    # Clean up any UUID files created during test
    UUID_FILES=$(find "$TEST_DIR/data/skg-data" -name "*.skg" ! -name "1.skg" 2>/dev/null || true)
    if [ -n "$UUID_FILES" ]; then
        echo "$UUID_FILES" | while read -r file; do
            rm -f "$file"
            echo "✓ Deleted UUID file: $(basename "$file")"
        done
    fi

    # Clean up Tantivy index contents but keep the directory
    cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"

    # Restore original 1.skg if backup exists
    BACKUP_FILE=$(find "$TEST_DIR/data/skg-data" -name "1.skg.backup.*" | head -1)
    if [ -n "$BACKUP_FILE" ] && [ -f "$BACKUP_FILE" ]; then
        cp "$BACKUP_FILE" "$TEST_DIR/data/skg-data/1.skg"
        rm -f "$BACKUP_FILE"
        echo "✓ Restored 1.skg from backup"
    fi
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
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
