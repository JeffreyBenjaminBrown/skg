#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

"$(dirname "$0")/02-prepare-data.sh"
stop_existing_container

docker run --name "$CONTAINER_NAME" --rm -d \
  --platform linux/amd64 \
  --ulimit nofile=524288:524288 \
  -p "$SKG_TEST_PORT:$SKG_TEST_PORT" \
  -v "$HOST_DATA_ROOT:/data" \
  "$IMAGE_NAME" sleep infinity >/dev/null

docker exec \
  -e SKG_CONFIG=/data/skgconfig.toml \
  "$CONTAINER_NAME" \
  /opt/skg/windows/container/run-servers.sh

if wait_for_port "$SKG_TEST_PORT"; then
  echo "Skg is listening on 127.0.0.1:$SKG_TEST_PORT"
else
  echo "ERROR: Skg did not listen on 127.0.0.1:$SKG_TEST_PORT" >&2
  "$(dirname "$0")/04-logs.sh" >&2 || true
  exit 1
fi

"$(dirname "$0")/04-logs.sh"
