#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"
cd "$(repo_root)"

docker build -f windows/Dockerfile -t "$IMAGE_NAME" .
