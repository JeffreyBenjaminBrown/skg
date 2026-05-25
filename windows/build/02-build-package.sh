#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
./windows/build-package.sh
