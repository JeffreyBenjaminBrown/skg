#!/bin/bash

# Integration test for saving a new node from an empty content view buffer.
# The test prepares empty data directories, starts the skg server, and runs
# an Emacs script that opens an empty buffer, inserts a headline, saves it,
# and verifies the resulting .skg file.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Empty Content Save Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

prepare_data_dirs() {
  echo "=== Preparing data directories ==="
  rm -rf "$TEST_DIR/data"
  mkdir -p "$TEST_DIR/data/skg"
  mkdir -p "$TEST_DIR/data/index.tantivy"
  echo "✓ Created empty data directories"
}

cleanup_test_data() {
  echo "=== Cleaning up test data ==="
  if [ -d "$TEST_DIR/data/skg" ]; then
    rm -f "$TEST_DIR/data/skg/"*
    echo "✓ Emptied skg data directory"
  fi
  cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"
}

enhanced_cleanup() {
  cleanup_test_data
  cleanup
}

trap enhanced_cleanup EXIT

prepare_data_dirs

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG=$(mktemp)
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
skg_folder = "$TEST_DIR/data/skg"
tantivy_folder = "$TEST_DIR/data/index.tantivy"
port = $AVAILABLE_PORT
delete_on_quit = true
EOF

start_skg_server

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
