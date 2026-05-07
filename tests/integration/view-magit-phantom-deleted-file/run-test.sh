#!/bin/bash

# Integration test: skg-goto-in-magit on a phantom whose file has
# been deleted from disk, from a relative-config-path launch.
#
# HEAD:        a contains [b]; a.skg and b.skg both committed.
# Worktree:    a no longer contains b; b.skg deleted from disk.
#
# The test opens a content view from a, toggles diff mode, verifies
# b shows up as a phantom, and calls skg-goto-in-magit on it. It
# then checks that the magit buffer is rooted at the skg-data repo
# (NOT the outer skg project repo, which is the failure mode when
# the server returns a mis-prefixed path) and that point is on the
# b.skg entry.
#
# The bug this guards against is a relative-config-path scenario
# where config.data_root stayed relative through config loading. In
# get_file_path's response, canonicalize on a deleted file fails
# (file gone), the raw path stays relative, strip_prefix against an
# absolute data_root fails, and the fallback emits the full relative
# path -- which the client then expands against skg-config-dir (also
# the absolute config dir), producing a double-prefixed nonexistent
# directory. magit's toplevel-walk then lands on the outer project
# repo. To reproduce that, this test launches the server with a
# relative config path from $TEST_DIR.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG View-Magit-Phantom-Deleted-File Integration Test ==="

SKG_DATA="$TEST_DIR/data/skg-data"
FIXTURE_FILES="a.skg b.skg"
TEMP_CONFIG="$TEST_DIR/data/skgconfig.toml.test"

backup_and_reset_test_data() {
    echo "=== Setting up git repo from fixture files ==="

    rm -rf "$SKG_DATA/.git"

    for f in $FIXTURE_FILES; do
        cp "$SKG_DATA/$f" "$SKG_DATA/$f.backup"
    done

    restore_fixtures

    cd "$SKG_DATA"
    git init -q
    git config user.email "test@test.com"
    git config user.name "Test"
    git add *.skg
    git commit -q -m "Initial commit"

    # Simulate a "delete b" edit: remove b from a's contains list,
    # and delete b.skg from disk. Both changes are unstaged.
    cat > "$SKG_DATA/a.skg" << 'EOF'
title: a
pid: a
EOF
    rm -f "$SKG_DATA/b.skg"

    echo "✓ Git repo ready: a.skg modified; b.skg deleted (both unstaged)"
}

restore_fixtures() {
    for f in $FIXTURE_FILES; do
        if [ -f "$SKG_DATA/$f.backup" ]; then
            cp "$SKG_DATA/$f.backup" "$SKG_DATA/$f"
        fi
    done
}

cleanup_test_data() {
    rm -rf "$SKG_DATA/.git"
    restore_fixtures
    for f in $FIXTURE_FILES; do
        rm -f "$SKG_DATA/$f.backup"
    done
    rm -f "$TEMP_CONFIG"
    cleanup_tantivy_index "$TEST_DIR/data/.index.tantivy"
}

enhanced_cleanup() {
    cleanup_test_data
    cleanup
}

trap enhanced_cleanup EXIT

backup_and_reset_test_data

check_typedb_server

AVAILABLE_PORT=$(find_available_port)
echo "Using port $AVAILABLE_PORT for test server..."

DB_NAME=$(generate_db_name)
# NOTE: source path is relative ("skg-data"), and the config itself
# lives one directory below $TEST_DIR so that a relative-config-path
# launch yields config.data_root = "data" (also relative) -- the
# scenario this test is designed to exercise.
cat > "$TEMP_CONFIG" << EOF
db_name = "$DB_NAME"
tantivy_folder = "$TEST_DIR/data/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
delete_on_quit = true

[[sources]]
name = "main"
path = "skg-data"
user_owns_it = true
EOF

echo ""
echo "Starting skg server with a relative config path from $PROJECT_ROOT..."
# Launching from $PROJECT_ROOT is required so the server can read
# schema.tql (which it loads from CWD). The config path is passed
# relative to $PROJECT_ROOT, which is what makes config.data_root
# relative -- the condition under which the path-duplication bug
# manifests.
cd "$PROJECT_ROOT"
RELATIVE_CONFIG="${TEMP_CONFIG#$PROJECT_ROOT/}"
target/debug/skg "$RELATIVE_CONFIG" > "$TEST_DIR/server.log" 2>&1 &
CARGO_PID=$!
echo "✓ Started skg server (PID: $CARGO_PID) with config: $RELATIVE_CONFIG"

# Wait for "Server ready." -- mirrors start_skg_server in test-lib.sh.
max_attempts=300
attempt=0
while [ $attempt -lt $max_attempts ]; do
  if grep -q "Server ready\." "$TEST_DIR/server.log" 2>/dev/null; then
    echo "✓ Server is ready on port $AVAILABLE_PORT"
    break
  fi
  if ! kill -0 $CARGO_PID 2>/dev/null; then
    echo "ERROR: Server process died during startup"
    cat "$TEST_DIR/server.log"
    exit 1
  fi
  sleep 0.2
  attempt=$(( attempt + 1 ))
done
if [ $attempt -ge $max_attempts ]; then
  echo "ERROR: Server did not become ready after 60 seconds"
  tail -30 "$TEST_DIR/server.log"
  exit 1
fi

run_emacs_test "test-emacs.el"

echo ""
echo "=== Test Complete ==="
exit $TEST_RESULT
