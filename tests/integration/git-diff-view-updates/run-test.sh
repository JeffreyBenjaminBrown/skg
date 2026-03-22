#!/bin/bash

# Integration test for git diff view updates.
#
# Graph:
#   a contains [b]
#   b contains [c, d, e]
#   f is an island (no containment)
#
# From view-b, the user:
#   - deletes c (editRequest delete)
#   - removes d from b's children (but leaves d in the graph)
#   - changes e's title to "e, edited"
#   - appends f as a new child of b
#   - makes d a child of f
#
# After saving, git diff mode is toggled on.
# We verify diff markers in both view-b and view-a.
# Then we commit e's title change, re-save, verify textChanged gone.
# Then we toggle diff mode off and verify clean output.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Git Diff View Updates Integration Test ==="

SKG_DATA="$TEST_DIR/data/skg-data"
FIXTURE_FILES="a.skg b.skg c.skg d.skg e.skg f.skg"

backup_and_reset_test_data() {
    echo "=== Setting up git repo from fixture files ==="

    # Remove any leftover git repo or extra files from previous runs
    rm -rf "$SKG_DATA/.git"
    find "$SKG_DATA" -name "*.skg" ! -name "a.skg" ! -name "b.skg" \
         ! -name "c.skg" ! -name "d.skg" ! -name "e.skg" ! -name "f.skg" \
         -delete 2>/dev/null || true

    # Back up fixtures (save may modify them)
    for f in $FIXTURE_FILES; do
        cp "$SKG_DATA/$f" "$SKG_DATA/$f.backup"
    done

    # Reset fixtures to clean state
    restore_fixtures

    # Init git repo and commit the fixture files
    cd "$SKG_DATA"
    git init -q
    git config user.email "test@test.com"
    git config user.name "Test"
    git add *.skg
    git commit -q -m "Initial commit"

    echo "✓ Git repo ready with 6 nodes committed"
}

restore_fixtures() {
    for f in $FIXTURE_FILES; do
        if [ -f "$SKG_DATA/$f.backup" ]; then
            cp "$SKG_DATA/$f.backup" "$SKG_DATA/$f"
        fi
    done
}

cleanup_test_data() {
    # Remove git repo
    rm -rf "$SKG_DATA/.git"
    # Restore fixture files from backups
    restore_fixtures
    # Remove backups
    for f in $FIXTURE_FILES; do
        rm -f "$SKG_DATA/$f.backup"
    done
    # Remove any UUID files created during test
    find "$SKG_DATA" -name "*.skg" ! -name "a.skg" ! -name "b.skg" \
         ! -name "c.skg" ! -name "d.skg" ! -name "e.skg" ! -name "f.skg" \
         -delete 2>/dev/null || true
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

export SKG_DATA_DIR="$SKG_DATA"
run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
