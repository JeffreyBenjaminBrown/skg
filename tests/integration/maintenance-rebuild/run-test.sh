#!/bin/bash

# End-to-end full rebuild through the durable maintenance protocol.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Full Rebuild Maintenance Integration Test ==="

TEST_WORK_ROOT=""

cleanup_rebuild_test() {
    cleanup
    case "$TEST_WORK_ROOT" in
        "$TEST_DIR"/work-*) rm -rf -- "$TEST_WORK_ROOT" ;;
    esac
}

trap cleanup_rebuild_test EXIT


TEST_WORK_ROOT=$(mktemp -d "$TEST_DIR/work-XXXXXX")
SOURCE_ROOT="$TEST_WORK_ROOT/notes"
REPLACEMENT_SOURCE_ROOT="$TEST_WORK_ROOT/replacement-notes"
mkdir -p "$SOURCE_ROOT" "$REPLACEMENT_SOURCE_ROOT"
cat > "$SOURCE_ROOT/x.skg" << 'EOF'
title: "title before rebuild"
pid: "x"
EOF
cat > "$REPLACEMENT_SOURCE_ROOT/x.skg" << 'EOF'
title: "title after rebuild"
pid: "x"
EOF
cat > "$REPLACEMENT_SOURCE_ROOT/y.skg" << 'EOF'
title: "queryable after invalid preflight"
pid: "y"
EOF

AVAILABLE_PORT=$(find_available_port)
TEMP_CONFIG="$TEST_WORK_ROOT/skgconfig.toml"
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$TEST_WORK_ROOT/.index.tantivy"
maintenance_archive_folder = "maintenance-archives"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
default_source_set = "main"

[[sources]]
name = "main"
path = "notes"
EOF

export SKG_TEST_CONFIG="$TEMP_CONFIG"

start_skg_server
run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
