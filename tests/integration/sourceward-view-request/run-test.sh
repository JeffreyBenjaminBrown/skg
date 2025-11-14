#!/bin/bash

# Integration test for sourceward-view request functionality
# This script prepares test data, runs the skg server, invokes
# an Emacs batch test, and restores the environment afterwards.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Sourceward View Request Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

SKG_FILES=(1 11 12 l-11)

backup_and_reset_test_data() {
    echo "=== Backing up and preparing test data ==="

    for file in "${SKG_FILES[@]}"; do
        local path="$TEST_DIR/data/skg-data/$file.skg"
        if [ -f "$path" ]; then
            local backup="$path.backup.$(date +%s)"
            cp "$path" "$backup"
            echo "✓ Backed up $file.skg to $(basename "$backup")"
        fi
    done

    cat > "$TEST_DIR/data/skg-data/1.skg" <<'DATA'
title: '1'
ids:
- '1'
contains:
- '11'
- '12'
DATA

    cat > "$TEST_DIR/data/skg-data/11.skg" <<'DATA'
title: '11'
ids:
- '11'
contains: []
DATA

    cat > "$TEST_DIR/data/skg-data/12.skg" <<'DATA'
title: '12'
ids:
- '12'
contains: []
DATA

    cat > "$TEST_DIR/data/skg-data/l-11.skg" <<'DATA'
title: '[[id:11][a link to 11]]'
ids:
- 'l-11'
contains: []
DATA

    echo "✓ Test data prepared"
}

cleanup_test_data() {
    echo "=== Restoring test data ==="

    cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"

    for file in "${SKG_FILES[@]}"; do
        local backup=$(find "$TEST_DIR/data/skg-data" -name "$file.skg.backup.*" | head -1)
        if [ -n "$backup" ] && [ -f "$backup" ]; then
            cp "$backup" "$TEST_DIR/data/skg-data/$file.skg"
            rm -f "$backup"
            echo "✓ Restored $file.skg from backup"
        fi
    done
}

enhanced_cleanup() {
    cleanup_test_data
    cleanup
}

trap enhanced_cleanup EXIT

backup_and_reset_test_data

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp)
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" <<EOF
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
