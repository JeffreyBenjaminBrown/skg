#!/bin/bash

# Integration test for delete-then-edit-under-deleted collateral updates
# This script:
# - Backs up and resets test data (five .skg fixtures) to clean state
# - Verifies TypeDB server is running
# - Starts an independent skg server with test config
# - Uses Emacs to test node deletion via editRequest and subsequent
#   editing under a DeletedNode across two collateral buffers
# - Cleans up test artifacts and restores original state

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Collateral Delete Then Edit Under It Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

FIXTURE_FILES=("1" "11" "subee" "subee-1" "also-hidden")

# Backup and reset functions
backup_and_reset_test_data() {
    echo "=== Backing up and resetting test data ==="

    # Clean up any UUID files from previous test runs
    for f in "${FIXTURE_FILES[@]}"; do
        true  # fixture files are handled below
    done
    find "$TEST_DIR/data/skg-data" -name "*.skg" \
        ! -name "1.skg" \
        ! -name "11.skg" \
        ! -name "subee.skg" \
        ! -name "subee-1.skg" \
        ! -name "also-hidden.skg" \
        -delete 2>/dev/null || true
    echo "✓ Cleaned up existing UUID files"

    # Backup and reset each fixture file
    for f in "${FIXTURE_FILES[@]}"; do
        if [ -f "$TEST_DIR/data/skg-data/$f.skg" ]; then
            cp "$TEST_DIR/data/skg-data/$f.skg" \
               "$TEST_DIR/data/skg-data/$f.skg.backup.$(date +%s)"
            echo "✓ Backed up $f.skg"
        fi
    done

    # Reset 1.skg
    cat > "$TEST_DIR/data/skg-data/1.skg" << 'EOF'
title: "1"
ids:
  - "1"
contains:
  - "11"
EOF

    # Reset 11.skg
    cat > "$TEST_DIR/data/skg-data/11.skg" << 'EOF'
title: "11"
ids:
  - "11"
aliases:
  - eleven
subscribes_to:
  - subee
hides_from_its_subscriptions:
  - subee-1
  - also-hidden
EOF

    # Reset subee.skg
    cat > "$TEST_DIR/data/skg-data/subee.skg" << 'EOF'
title: subee
ids:
  - subee
contains:
  - subee-1
EOF

    # Reset subee-1.skg
    cat > "$TEST_DIR/data/skg-data/subee-1.skg" << 'EOF'
title: subee-1
ids:
  - subee-1
EOF

    # Reset also-hidden.skg
    cat > "$TEST_DIR/data/skg-data/also-hidden.skg" << 'EOF'
title: also-hidden
ids:
  - also-hidden
EOF

    echo "✓ Reset all fixture files to clean state"
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="

    # Clean up any UUID files created during test
    UUID_FILES=$(find "$TEST_DIR/data/skg-data" -name "*.skg" \
        ! -name "1.skg" \
        ! -name "11.skg" \
        ! -name "subee.skg" \
        ! -name "subee-1.skg" \
        ! -name "also-hidden.skg" \
        ! -name "*.backup.*" \
        2>/dev/null || true)
    if [ -n "$UUID_FILES" ]; then
        echo "$UUID_FILES" | while read -r file; do
            rm -f "$file"
            echo "✓ Deleted UUID file: $(basename "$file")"
        done
    fi

    # Clean up Tantivy index contents but keep the directory
    cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"

    # Restore each fixture file from backup
    for f in "${FIXTURE_FILES[@]}"; do
        BACKUP=$(find "$TEST_DIR/data/skg-data" -name "$f.skg.backup.*" | head -1)
        if [ -n "$BACKUP" ] && [ -f "$BACKUP" ]; then
            cp "$BACKUP" "$TEST_DIR/data/skg-data/$f.skg"
            echo "✓ Restored $f.skg from backup"
        fi
        find "$TEST_DIR/data/skg-data" -name "$f.skg.backup.*" -delete
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
