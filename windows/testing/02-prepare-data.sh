#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

rm -rf "$HOST_DATA_ROOT"
mkdir -p "$HOST_DATA_ROOT/public" "$HOST_DATA_ROOT/private"
git -C "$HOST_DATA_ROOT/public" init
git -C "$HOST_DATA_ROOT/private" init

cat > "$HOST_DATA_ROOT/skgconfig.toml" <<EOF
db_name = "skg-win-test-$SKG_TEST_PORT"
tantivy_folder = ".index.tantivy"
port = $SKG_TEST_PORT
beep_when_server_becomes_available = false

[[sources]]
name = "public"
path = "public"
user_owns_it = true

[[sources]]
name = "private"
path = "private"
user_owns_it = true
EOF

echo "Prepared $HOST_DATA_ROOT for port $SKG_TEST_PORT"
