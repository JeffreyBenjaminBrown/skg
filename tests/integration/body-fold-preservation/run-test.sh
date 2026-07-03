#!/bin/bash

# Integration test for *body* fold preservation.
# Verifies that when the user folds a node's body before saving,
# the body remains folded (invisible) in the buffer the server returns.
# Headlines folded inside a folded subtree are also checked.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Body-Fold-Preservation Integration Test ==="

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

cleanup_test_data() {
    find "$TEST_DIR/data/skg-data" -name "*.skg" -delete 2>/dev/null || true
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
}

enhanced_cleanup() { cleanup_test_data; cleanup; }
trap enhanced_cleanup EXIT

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo ""
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
path = "$TEST_DIR/data/skg-data"
user_owns_it = true
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
