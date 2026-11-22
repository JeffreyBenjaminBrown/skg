#!/bin/bash

# Integration regression for direct visits to forked nodes.
# This script:
# - Starts an independent skg server with test config
# - Uses Emacs and nvim to visit a forked node and require the raw root
#   without its fork as an independent sibling
# - Exercises same-title nodes from different skgrepos; the Emacs client also
#   revisits the raw view through switch-to-view and checks close/reopen lifecycle

set -e  # Exit on any error

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

# Repo common test library
source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Forked Direct Visit Integration Test ==="
echo "Test directory: $TEST_DIR"
echo "Project root: $PROJECT_ROOT"

# Clean up Tantivy index to prevent bloat
cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

# Set up cleanup trap
trap cleanup EXIT


make_temp_config_in_data_folder << EOF
[[repos]]
name = "Cheese"
path = "$TEST_DIR/data/cheese"
foreign = true

[[repos]]
name = "public"
path = "$TEST_DIR/data/owned/skg"

[[repos]]
name = "private"
path = "$TEST_DIR/data/owned/private"
EOF

run_server_and_client_test_then_exit
