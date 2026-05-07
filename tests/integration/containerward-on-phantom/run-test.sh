#!/bin/bash

# Integration test: containerward view request on a removed-here phantom.
#
# Graph (at HEAD):
#   a contains [b, c]
#   b contains [c]
#
# The test opens a view from a, removes c from under b, saves
# (so b.skg loses c from its contains list), then toggles diff mode.
# Under b, c appears as a removed-here phantom (was in b's contains
# at HEAD, still exists on disk).  The test requests a containerward
# view on that phantom c and verifies the path is integrated.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Containerward-on-Phantom Integration Test ==="

SKG_DATA="$TEST_DIR/data/skg-data"
FIXTURE_FILES="a.skg b.skg c.skg"

backup_and_reset_test_data() {
    echo "=== Setting up git repo from fixture files ==="

    rm -rf "$SKG_DATA/.git"

    for f in $FIXTURE_FILES; do
        cp "$SKG_DATA/$f" "$SKG_DATA/$f.backup"
    done

    restore_fixtures

    cd "$SKG_DATA"
    git init -q
    git config user.email "test@test.com"
    git config user.name "Test"
    git add *.skg
    git commit -q -m "Initial commit"

    echo "✓ Git repo ready with 3 nodes committed"
}

restore_fixtures() {
    for f in $FIXTURE_FILES; do
        if [ -f "$SKG_DATA/$f.backup" ]; then
            cp "$SKG_DATA/$f.backup" "$SKG_DATA/$f"
        fi
    done
}

cleanup_test_data() {
    rm -rf "$SKG_DATA/.git"
    restore_fixtures
    for f in $FIXTURE_FILES; do
        rm -f "$SKG_DATA/$f.backup"
    done
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
}

enhanced_cleanup() {
    cleanup_test_data
    cleanup
}

trap enhanced_cleanup EXIT

backup_and_reset_test_data

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
name = "main"
path = "$SKG_DATA"
user_owns_it = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
