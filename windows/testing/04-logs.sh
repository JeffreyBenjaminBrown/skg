#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

for log in \
  "$HOST_DATA_ROOT/logs/server-to-user.log" \
  "$HOST_DATA_ROOT/logs/skg-stdout-stderr.log" \
  "$HOST_DATA_ROOT/logs/typedb.log"
do
  echo
  echo "=== $log ==="
  if [ -f "$log" ]; then
    tail -80 "$log"
  else
    echo "missing"
  fi
done
