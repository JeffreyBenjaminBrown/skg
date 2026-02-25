#!/bin/bash

# Integration test for collateral buffer updates: title edit + add child
# This script:
# - Backs up and resets test data (a.skg and b.skg) to clean state
# - Verifies TypeDB server is running
# - Starts an independent skg server with test config
# - Uses Emacs to test that editing a title and adding a child in one buffer
#   propagates to another open buffer via collateral completion
# - Cleans up test artifacts and restores original state

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Collateral Add Content Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Backup and reset functions
backup_and_reset_test_data() {
    echo "=== Backing up and resetting test data ==="

    # Clean up any UUID files from previous test runs
    find "$TEST_DIR/data/skg-data" -name "*.skg" ! -name "a.skg" ! -name "b.skg" -delete 2>/dev/null || true
    echo "✓ Cleaned up existing UUID files"

    # Backup original a.skg
    if [ -f "$TEST_DIR/data/skg-data/a.skg" ]; then
        BACKUP_A="$TEST_DIR/data/skg-data/a.skg.backup.$(date +%s)"
        cp "$TEST_DIR/data/skg-data/a.skg" "$BACKUP_A"
        echo "✓ Backed up a.skg to $(basename "$BACKUP_A")"
    fi

    # Backup original b.skg
    if [ -f "$TEST_DIR/data/skg-data/b.skg" ]; then
        BACKUP_B="$TEST_DIR/data/skg-data/b.skg.backup.$(date +%s)"
        cp "$TEST_DIR/data/skg-data/b.skg" "$BACKUP_B"
        echo "✓ Backed up b.skg to $(basename "$BACKUP_B")"
    fi

    # Reset a.skg to clean state
    cat > "$TEST_DIR/data/skg-data/a.skg" << 'EOF'
title: a
ids:
  - a
contains:
  - b
EOF
    echo "✓ Reset a.skg to clean state"

    # Reset b.skg to clean state
    cat > "$TEST_DIR/data/skg-data/b.skg" << 'EOF'
title: b
ids:
  - b
contains:
  - a
EOF
    echo "✓ Reset b.skg to clean state"
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="

    # Clean up any UUID files created during test
    UUID_FILES=$(find "$TEST_DIR/data/skg-data" -name "*.skg" ! -name "a.skg" ! -name "b.skg" 2>/dev/null || true)
    if [ -n "$UUID_FILES" ]; then
        echo "$UUID_FILES" | while read -r file; do
            rm -f "$file"
            echo "✓ Deleted UUID file: $(basename "$file")"
        done
    fi

    # Clean up Tantivy index contents but keep the directory
    cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"

    # Restore original a.skg if backup exists
    BACKUP_A=$(find "$TEST_DIR/data/skg-data" -name "a.skg.backup.*" | head -1)
    if [ -n "$BACKUP_A" ] && [ -f "$BACKUP_A" ]; then
        cp "$BACKUP_A" "$TEST_DIR/data/skg-data/a.skg"
        echo "✓ Restored a.skg from backup"
    fi
    find "$TEST_DIR/data/skg-data" -name "a.skg.backup.*" -delete

    # Restore original b.skg if backup exists
    BACKUP_B=$(find "$TEST_DIR/data/skg-data" -name "b.skg.backup.*" | head -1)
    if [ -n "$BACKUP_B" ] && [ -f "$BACKUP_B" ]; then
        cp "$BACKUP_B" "$TEST_DIR/data/skg-data/b.skg"
        echo "✓ Restored b.skg from backup"
    fi
    find "$TEST_DIR/data/skg-data" -name "b.skg.backup.*" -delete
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
tantivy_folder = "$TEST_DIR/data/index.tantivy"
port = $AVAILABLE_PORT
delete_on_quit = true

[[sources]]
nickname = "main"
path = "$TEST_DIR/data/skg-data"
user_owns_it = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
