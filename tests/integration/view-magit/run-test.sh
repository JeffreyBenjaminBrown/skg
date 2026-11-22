#!/bin/bash

# Integration test for skg-goto-in-magit
# Tests: from a content view, skg-goto-in-magit opens magit-status
# and navigates to the correct file with the right staging message.
#
# Setup: a gitrepo with two .skg files committed, then one modified.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG View-Magit Integration Test ==="

SKG_DATA="$TEST_DIR/data/owned/skg-data"

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

    # Init gitrepo and commit
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


make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$SKG_DATA"
EOF

run_server_and_client_test_then_exit
