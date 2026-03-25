#!/bin/bash

# Integration test for source cycling in the metadata edit buffer.
# Verifies that S-left / S-right cycle through owned sources only.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Source Cycling Integration Test ==="

cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"

enhanced_cleanup() {
  rm -f "$TEST_DIR/data/skgconfig.toml"
  rm -f "$TEST_DIR/data/.skg_init_marker"
  cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
  cleanup
}

trap enhanced_cleanup EXIT

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

TEMP_CONFIG="$TEST_DIR/data/skgconfig.toml"
DB_NAME=$(generate_db_name)
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
delete_on_quit = true

[[sources]]
name = "public"
path = "$TEST_DIR/data/public"
user_owns_it = true

[[sources]]
name = "personal"
path = "$TEST_DIR/data/personal"
user_owns_it = true

[[sources]]
name = "private"
path = "$TEST_DIR/data/private"
user_owns_it = true

[[sources]]
name = "foreign"
path = "$TEST_DIR/data/foreign"
user_owns_it = false
EOF

start_skg_server

SKG_CONFIG_FILE="$TEMP_CONFIG" run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
