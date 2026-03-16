#!/bin/bash

# Integration test for skg-view-magit
# Tests: from a content view, skg-view-magit opens magit-status
# and navigates to the correct file with the right staging message.
#
# Setup: a git repo with two .skg files committed, then one modified.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG View-Magit Integration Test ==="

SKG_DATA="$TEST_DIR/data/skg-data"

backup_and_reset_test_data() {
    echo "=== Setting up git repo with fixtures ==="

    # Reset fixture files
    cat > "$SKG_DATA/x.skg" << 'EOF'
title: "x"
pid: "x"
contains:
  - "y"
EOF
    cat > "$SKG_DATA/y.skg" << 'EOF'
title: "y"
pid: "y"
EOF

    # Init git repo and commit
    cd "$SKG_DATA"
    rm -rf .git
    git init -q
    git config user.email "test@test.com"
    git config user.name "Test"
    git add *.skg
    git commit -q -m "Initial commit"

    # Modify x.skg (unstaged change)
    cat > "$SKG_DATA/x.skg" << 'EOF'
title: "x received a title change"
pid: "x"
contains:
  - "y"
EOF

    echo "✓ Git repo ready with unstaged change to x.skg"
}

cleanup_test_data() {
    rm -rf "$SKG_DATA/.git"
    cat > "$SKG_DATA/x.skg" << 'EOF'
title: "x"
pid: "x"
contains:
  - "y"
EOF
    cat > "$SKG_DATA/y.skg" << 'EOF'
title: "y"
pid: "y"
EOF
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
nickname = "main"
path = "$SKG_DATA"
user_owns_it = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
