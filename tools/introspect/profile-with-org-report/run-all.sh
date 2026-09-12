#!/usr/bin/env bash

set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

PROFILE_SKIP_NATIVE=1 "$profile_dir/run.sh"
"$profile_dir/run-callgrind.sh"
