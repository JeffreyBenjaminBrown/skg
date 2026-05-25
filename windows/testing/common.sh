#!/usr/bin/env bash
set -euo pipefail

repo_root() {
  cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd
}

IMAGE_NAME="${IMAGE_NAME:-jeffreybbrown/skg:latest}"
CONTAINER_NAME="${CONTAINER_NAME:-skg-win-test}"
HOST_DATA_ROOT="${HOST_DATA_ROOT:-$(repo_root)/windows/testing/tmp/skg-win-test}"
SKG_TEST_PORT="${SKG_TEST_PORT:-1739}"

container_exists() {
  docker inspect "$CONTAINER_NAME" >/dev/null 2>&1
}

container_running() {
  [ "$(docker inspect -f '{{.State.Running}}' "$CONTAINER_NAME" 2>/dev/null || true)" = "true" ]
}

stop_existing_container() {
  if container_exists; then
    docker stop "$CONTAINER_NAME" >/dev/null
  fi
}

wait_for_port() {
  local port="$1"
  for _ in $(seq 1 60); do
    if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
      exec 3>&-
      exec 3<&-
      return 0
    fi
    sleep 1
  done
  return 1
}
