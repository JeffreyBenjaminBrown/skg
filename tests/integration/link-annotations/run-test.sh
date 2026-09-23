#!/bin/bash
set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"
source "$TEST_DIR/../test-lib.sh"

DATA_DIR=$(mktemp -d "$TEST_DIR/data/live-XXXXXX")
cp -R "$TEST_DIR/data/public" "$DATA_DIR/public"
cp -R "$TEST_DIR/data/private" "$DATA_DIR/private"
AVAILABLE_PORT=$(find_available_port)
TEMP_CONFIG=$(mktemp "$TEST_DIR/data/skgconfig-tmp-XXXXXX.toml")
cat > "$TEMP_CONFIG" << EOF
tantivy_folder = "$DATA_DIR/.index.tantivy"
port = $AVAILABLE_PORT
beep_when_server_becomes_available = false
default_source_set = "public"

[[sources]]
name = "public"
abbreviation = "PUB"
path = "$DATA_DIR/public"

[[sources]]
name = "private"
abbreviation = "PRIV"
path = "$DATA_DIR/private"
EOF
trap 'cleanup; rm -rf "$DATA_DIR"' EXIT
export SKG_TEST_DATA_DIR="$DATA_DIR"

start_skg_server
run_client_test
exit "$TEST_RESULT"
