#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

docker run --rm --platform linux/amd64 "$IMAGE_NAME" bash -lc '
  set -e
  echo "=== PATH ==="
  echo "$PATH"
  echo
  echo "=== skg ==="
  command -v skg || ls -l /usr/local/bin/skg
  echo
  echo "=== typedb ==="
  command -v typedb
  typedb server --help | head -20
  echo
  echo "=== schema ==="
  ls -l /opt/skg/schema.tql
'
