#!/usr/bin/env bash
set -euo pipefail

SKG_CONFIG="${SKG_CONFIG:-/data/skgconfig.toml}"
DATA_ROOT="$(dirname "$SKG_CONFIG")"
TYPEDB_DATA_DIR="/opt/typedb/server/data"
if [ ! -e "$TYPEDB_DATA_DIR" ]; then
  TYPEDB_DATA_DIR="/opt/typedb/core/server/data"
fi

mkdir -p "$DATA_ROOT/logs"

HARD_NOFILE="$(ulimit -Hn)"
if [ -n "$HARD_NOFILE" ] && [ "$HARD_NOFILE" != "unlimited" ]; then
  ulimit -Sn "$HARD_NOFILE" 2>/dev/null || true
fi

typedb_process_exists() {
  pgrep -f 'typedb_server_bin' >/dev/null 2>&1
}

typedb_grpc_is_up() {
  (exec 3<>/dev/tcp/127.0.0.1/1729) 2>/dev/null
}

skg_process_exists() {
  pgrep -f "/usr/local/bin/skg $SKG_CONFIG" >/dev/null 2>&1
}

start_typedb() {
  pkill -9 -f typedb_server_bin 2>/dev/null || true
  sleep 1
  rm -rf "$TYPEDB_DATA_DIR"/skg-test* 2>/dev/null || true
  nohup typedb server > "$DATA_ROOT/logs/typedb.log" 2>&1 < /dev/null &
  for _ in $(seq 1 30); do
    if typedb_process_exists && typedb_grpc_is_up; then
      return 0
    fi
    sleep 1
  done
  echo "ERROR: TypeDB did not become healthy within 30 seconds." >&2
  tail -20 "$DATA_ROOT/logs/typedb.log" >&2 || true
  return 1
}

if typedb_process_exists && typedb_grpc_is_up; then
  echo "TypeDB is already running."
else
  echo "Starting TypeDB."
  start_typedb
fi

if skg_process_exists; then
  echo "Skg is already running for $SKG_CONFIG."
else
  echo "Starting Skg for $SKG_CONFIG."
  cd "${SKG_PROJECT_ROOT:-/opt/skg}"
  nohup /usr/local/bin/skg "$SKG_CONFIG" \
    >> "$DATA_ROOT/logs/skg-stdout-stderr.log" 2>&1 < /dev/null &
fi

echo "Logs:"
echo "  $DATA_ROOT/logs/typedb.log"
echo "  $DATA_ROOT/logs/server-to-user.log"
echo "  $DATA_ROOT/logs/skg-stdout-stderr.log"
