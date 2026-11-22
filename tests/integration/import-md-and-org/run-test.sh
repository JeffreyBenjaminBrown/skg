#!/bin/bash
# Isolated real-server import test, run once per editor client.
set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"
source "$TEST_DIR/../test-lib.sh"

TEMP_ROOT="$(mktemp -d)"
TEMP_CONFIG=""
CARGO_PID=""
finish () {
  cleanup
  if [ -n "$TEMP_ROOT" ] && [ -d "$TEMP_ROOT" ]; then
    rm -rf "$TEMP_ROOT"
  fi
}
trap finish EXIT

mkdir -p "$TEMP_ROOT/input" "$TEMP_ROOT/owned/repo" "$TEMP_ROOT/index"
cp "$TEST_DIR/input/guide.md" "$TEMP_ROOT/input/guide.md"
cp "$TEST_DIR/input/notes.org" "$TEMP_ROOT/input/notes.org"
export SKG_TEST_INPUT_DIR="$TEMP_ROOT/input"
export SKG_TEST_REPO_DIR="$TEMP_ROOT/owned/repo"

AVAILABLE_PORT="$(find_available_port)"
TEMP_CONFIG="$(mktemp "$TEMP_ROOT/skgconfig-XXXXXX.toml")"
write_test_config "$TEMP_ROOT/index" << EOF
[[repos]]
name = "main"
path = "owned/repo"
EOF

start_skg_server
run_client_test
exit "$TEST_RESULT"
