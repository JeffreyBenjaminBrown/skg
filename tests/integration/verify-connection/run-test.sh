#!/bin/bash

# Integration test for skg verify-connection functionality
# This script:
# - Verifies TypeDB server is running
# - Starts an independent cargo run process with test config
# - Uses Emacs to send the verify connection request
# - Tests that Emacs receives the expected result

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Verify Connection Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"


# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/index.tantivy"

# Set up cleanup trap
trap cleanup EXIT

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
