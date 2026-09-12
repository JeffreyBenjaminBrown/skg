#!/usr/bin/env bash
set -euo pipefail

SKG_CONFIG="${SKG_CONFIG:-/data/skgconfig.toml}"
DATA_ROOT="$(dirname "$SKG_CONFIG")"

mkdir -p "$DATA_ROOT/logs"

HARD_NOFILE="$(ulimit -Hn)"
if [ -n "$HARD_NOFILE" ] && [ "$HARD_NOFILE" != "unlimited" ]; then
  ulimit -Sn "$HARD_NOFILE" 2>/dev/null || true
fi

skg_process_exists() {
  pgrep -f "/usr/local/bin/skg $SKG_CONFIG" >/dev/null 2>&1
}

if skg_process_exists; then
  echo "Skg is already running for $SKG_CONFIG."
else
  echo "Starting Skg for $SKG_CONFIG."
  cd "${SKG_PROJECT_ROOT:-/opt/skg}"
  nohup /usr/local/bin/skg "$SKG_CONFIG" \
    >> "$DATA_ROOT/logs/skg-stdout-stderr.log" 2>&1 < /dev/null &
fi

echo "Logs:"
echo "  $DATA_ROOT/logs/server-to-user.log"
echo "  $DATA_ROOT/logs/skg-stdout-stderr.log"
