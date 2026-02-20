#!/bin/bash

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$(cd "$TEST_DIR/.." && pwd)/profiling-lib.sh"
run_profiling_test
