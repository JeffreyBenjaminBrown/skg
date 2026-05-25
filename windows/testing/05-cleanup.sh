#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/common.sh"

stop_existing_container
echo "Stopped $CONTAINER_NAME if it existed."
