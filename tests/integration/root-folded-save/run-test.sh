#!/bin/bash

# Reproduces the bug where a single-root content view, opened with
# org-startup-folded=t and saved immediately (point on root, all
# descendants hidden), comes back with the root headline missing.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Root-Folded-Save Integration Test ==="
echo "Test directory: $TEST_DIR"

FIXTURES="rfs-root rfs-c1 rfs-c2 rfs-c3 rfs-g1"

backup_and_reset_test_data() {
    echo "=== Backing up test data ==="
    for f in $FIXTURES; do
        local src="$TEST_DIR/data/owned/skg-data/$f.skg"
        if [ -f "$src" ]; then
            cp "$src" "$src.backup.$(date +%s)"
            echo "✓ Backed up $f.skg"
        fi
    done
}

cleanup_test_data() {
    echo "=== Cleaning up test data ==="
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
    for f in $FIXTURES; do
        local bak
        bak=$(find "$TEST_DIR/data/owned/skg-data" -name "$f.skg.backup.*" 2>/dev/null | head -1)
        if [ -n "$bak" ] && [ -f "$bak" ]; then
            cp "$bak" "$TEST_DIR/data/owned/skg-data/$f.skg"
            echo "✓ Restored $f.skg"
        fi
        find "$TEST_DIR/data/owned/skg-data" -name "$f.skg.backup.*" -delete 2>/dev/null || true
    done
}

enhanced_cleanup() { cleanup_test_data; cleanup; }
trap enhanced_cleanup EXIT

backup_and_reset_test_data

make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$TEST_DIR/data/owned/skg-data"
EOF

run_server_and_client_test_then_exit
