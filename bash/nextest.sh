#!/bin/bash
set -e

# Resolve project root (parent of this script's directory)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Clean up any leftover test databases from prior runs
"$PROJECT_ROOT/target/debug/cleanup-test-dbs"

# Run tests, forwarding all arguments
cargo nextest run "$@"
