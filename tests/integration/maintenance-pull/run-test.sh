#!/bin/bash

# End-to-end client-owned pull through the durable maintenance protocol.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Client-Owned Pull Integration Test ==="

TEST_WORK_ROOT=""

cleanup_pull_test() {
    cleanup
    case "$TEST_WORK_ROOT" in
        "$TEST_DIR"/work-*) rm -rf -- "$TEST_WORK_ROOT" ;;
    esac
}

trap cleanup_pull_test EXIT

check_typedb_server

TEST_WORK_ROOT=$(mktemp -d "$TEST_DIR/work-XXXXXX")
REMOTE_REPO="$TEST_WORK_ROOT/remote.git"
CLIENT_REPO="$TEST_WORK_ROOT/owned/notes"
UPSTREAM_REPO="$TEST_WORK_ROOT/upstream"
mkdir -p "$TEST_WORK_ROOT/owned"

git init --bare -q "$REMOTE_REPO"
git clone -q "$REMOTE_REPO" "$CLIENT_REPO"
git -C "$CLIENT_REPO" config user.email "test@test.com"
git -C "$CLIENT_REPO" config user.name "Test"
cat > "$CLIENT_REPO/x.skg" << 'EOF'
title: "title before pull"
pid: "x"
EOF
cat > "$CLIENT_REPO/.gitignore" << 'EOF'
DEPENDENCIES.toml
EOF
git -C "$CLIENT_REPO" add x.skg .gitignore
git -C "$CLIENT_REPO" commit -q -m "initial node"
git -C "$CLIENT_REPO" push -q -u origin HEAD

git clone -q "$REMOTE_REPO" "$UPSTREAM_REPO"
git -C "$UPSTREAM_REPO" config user.email "test@test.com"
git -C "$UPSTREAM_REPO" config user.name "Test"
cat > "$UPSTREAM_REPO/x.skg" << 'EOF'
title: "title after pull"
pid: "x"
EOF
git -C "$UPSTREAM_REPO" add x.skg
git -C "$UPSTREAM_REPO" commit -q -m "update node upstream"
git -C "$UPSTREAM_REPO" push -q

AVAILABLE_PORT=$(find_available_port)
DB_NAME=$(generate_db_name)
TEMP_CONFIG="$TEST_WORK_ROOT/skgconfig.toml"
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TEST_WORK_ROOT/.index.tantivy"
maintenance_archive_folder = "maintenance-archives"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
delete_on_quit = true

[[sources]]
name = "main"
path = "owned/notes"
EOF

export SKG_TEST_CONFIG="$TEMP_CONFIG"
export SKG_PULL_REPO="$CLIENT_REPO"

start_skg_server
run_client_test

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
