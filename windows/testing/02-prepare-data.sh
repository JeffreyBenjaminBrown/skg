#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

rm -rf "$HOST_DATA_ROOT"
mkdir -p "$HOST_DATA_ROOT/owned/public" "$HOST_DATA_ROOT/owned/private"
git -C "$HOST_DATA_ROOT/public" init
git -C "$HOST_DATA_ROOT/private" init

cat > "$HOST_DATA_ROOT/skgconfig.toml" <<EOF
db_name = "skg-win-test-$SKG_TEST_PORT"
tantivy_folder = ".index.tantivy"
port = $SKG_TEST_PORT
beep_when_server_becomes_available = false

[[sources]]
name = "public"
path = "owned/public"

[[sources]]
name = "private"
path = "owned/private"
EOF

echo "Prepared $HOST_DATA_ROOT for port $SKG_TEST_PORT"
