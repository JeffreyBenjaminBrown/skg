#!/bin/bash

# Integration test for the override-choice menu and its bypass.
# This script:
# - Starts an independent skg server with test config
# - Uses Emacs to visit an overridden node (expects the menu, under
#   the server-assigned "override-menu:Z" URI, with the minibuffer
#   notice delivered via the to-minibuffer field)
# - Exercises same-title nodes from different sources; the Emacs client also
#   revisits the menu through switch-to-view and checks close/reopen lifecycle

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Source common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Override Menu Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Set up cleanup trap
trap cleanup EXIT


# Find available port and create dynamic config
AVAILABLE_PORT=$(find_available_port)
echo ""
echo "Using port $AVAILABLE_PORT for test server..."

# Create a dynamic config with the available port
TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml") # inside data/ so the data root (the config-file dir) contains the owned/ folder
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false

[[sources]]
name = "public"
path = "$TEST_DIR/data/owned/skg"

[[sources]]
name = "Cheese"
path = "$TEST_DIR/data/cheese"

[[sources]]
name = "private"
path = "$TEST_DIR/data/owned/private"
EOF

start_skg_server

run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
