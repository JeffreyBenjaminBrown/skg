#!/bin/bash
# USAGE: bash/export-docs.sh [OUTPUT-DIR]
#   Exports every document that 'skg-system/' marks for export, under
#   bash/skg-system.skgconfig.toml (a config that owns skg-system/
#   alone), into OUTPUT-DIR, by default the top of this repository:
#   so 'docs/' and 'coding-advice/' are written from the notes
#   ('coding-advice/generated-docs.org'). Needs no server. An existing
#   file is overwritten; one no longer exported is left alone.
#   It builds the debug binary first, unless SKG_BIN names one.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUTPUT="$(mkdir -p "${1:-$ROOT}" && cd "${1:-$ROOT}" && pwd)"
cd "$ROOT"
if [ -z "${SKG_BIN:-}" ]; then
  cargo build -q --bin skg
  SKG_BIN="$ROOT/target/debug/skg"
fi
"$SKG_BIN" export-org bash/skg-system.skgconfig.toml skg-system "$OUTPUT"
