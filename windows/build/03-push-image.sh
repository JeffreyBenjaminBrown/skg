#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

docker push "$IMAGE_NAME"
