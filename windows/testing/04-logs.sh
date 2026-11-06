#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

# The server writes server-to-user.log to a folder per server run,
# CONFIG.running/START/; the container's config is /data/skgconfig.toml.
run_folder="$HOST_DATA_ROOT/skgconfig.toml.running"
newest_server_run_subfolder="$(
  find "$run_folder" -mindepth 1 -maxdepth 1 -type d 2>/dev/null \
    | LC_ALL=C sort | tail -1 || true)"

for log in \
  "${newest_server_run_subfolder:-$run_folder/NO-RUN-SUBFOLDER}/server-to-user.log" \
  "$HOST_DATA_ROOT/logs/skg-stdout-stderr.log"
do
  echo
  echo "=== $log ==="
  if [ -f "$log" ]; then
    tail -80 "$log"
  else
    echo "missing"
  fi
done
