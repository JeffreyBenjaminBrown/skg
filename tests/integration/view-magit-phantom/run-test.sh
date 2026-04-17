#!/bin/bash

# Integration test: skg-magit-goto-in-parent on a removed-here phantom.
#
# Graph at HEAD: a contains [b].
# Worktree:      a contains [].  (b removed from a's content)
# Both a.skg and b.skg still exist on disk.
#
# The test opens a content view from a, toggles diff mode,
# verifies b appears as a removed-here phantom,
# calls skg-magit-goto-in-parent on that phantom, and checks that
# magit navigates to a.skg (the parent) at the deletion line.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG View-Magit-Phantom Integration Test ==="

SKG_DATA="$TEST_DIR/data/skg-data"
FIXTURE_FILES="a.skg b.skg"

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

    # Modify a.skg: remove b from its contains list.
    # This creates an unstaged change where b is removed-here.
    cat > "$SKG_DATA/a.skg" << 'EOF'
title: a
pid: a
EOF

    echo "✓ Git repo ready: a.skg modified (b removed from contains)"
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
