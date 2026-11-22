#!/bin/bash

# Integration test for aliases-view request functionality
# This script:
# - Starts an independent cargo run process with test config
# - Uses Emacs to test aliases-view request and integration
# - Cleans up test artifacts

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Repo common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Aliases View Request Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Backup and reset functions
backup_and_reset_test_data() {
    echo "=== Backing up and resetting test data ==="

    # Create skg-data directory if it doesn't exist
    mkdir -p "$TEST_DIR/data/owned/skg-data"

    # Backup and reset test files
    for file in test-node; do
        if [ -f "$TEST_DIR/data/owned/skg-data/$file.skg" ]; then
            BACKUP_FILE="$TEST_DIR/data/owned/skg-data/$file.skg.backup.$(date +%s)"
            cp "$TEST_DIR/data/owned/skg-data/$file.skg" "$BACKUP_FILE"
            echo "✓ Backed up $file.skg to $(basename "$BACKUP_FILE")"
        fi

        # Create a test node with aliases
        cat > "$TEST_DIR/data/owned/skg-data/$file.skg" << EOF
title: 'Test Node'
pid: '$file'
aliases:
- 'first alias'
- 'second alias'
EOF
        echo "✓ Created $file.skg with test aliases"
    done
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="

    # Clean up Tantivy index contents but keep the directory
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

    # Restore original files if backups exist
    for file in test-node; do
        BACKUP_FILE=$(find "$TEST_DIR/data/owned/skg-data" -name "$file.skg.backup.*" | head -1)
        if [ -n "$BACKUP_FILE" ] && [ -f "$BACKUP_FILE" ]; then
            cp "$BACKUP_FILE" "$TEST_DIR/data/owned/skg-data/$file.skg"
            echo "✓ Restored $file.skg from backup"
        else
            # If no backup, remove the test file
            rm -f "$TEST_DIR/data/owned/skg-data/$file.skg"
            echo "✓ Removed test file $file.skg"
        fi
        find "$TEST_DIR/data/owned/skg-data" -name "$file.skg.backup.*" -delete
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


make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
