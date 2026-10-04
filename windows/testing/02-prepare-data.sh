#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

rm -rf "$HOST_DATA_ROOT"
mkdir -p "$HOST_DATA_ROOT/owned/public" "$HOST_DATA_ROOT/owned/private"
git -C "$HOST_DATA_ROOT/public" init
git -C "$HOST_DATA_ROOT/private" init

cat > "$HOST_DATA_ROOT/skgconfig.toml" <<EOF
tantivy_folder = ".index.tantivy"
port = $SKG_TEST_PORT
beep_when_server_becomes_available = false

[[repos]]
name = "public"
path = "owned/public"

[[repos]]
name = "private"
path = "owned/private"
EOF

echo "Prepared $HOST_DATA_ROOT for port $SKG_TEST_PORT"
