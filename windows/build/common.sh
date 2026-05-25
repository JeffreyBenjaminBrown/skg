#!/usr/bin/env bash
set -euo pipefail

IMAGE_NAME="${IMAGE_NAME:-jeffreybbrown/skg:latest}"

repo_root() {
  cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd
}
