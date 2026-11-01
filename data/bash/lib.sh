#!/usr/bin/env bash
# Shared helpers for the data/bash/*.sh repo-loop scripts.
#
# Source this file, then loop over one of:
#   skg_all_repos     -- every repo in skgconfig.toml
#   skg_owned_repos   -- only repos whose entry does not say foreign = true
# Each prints one repo path (relative to the data dir) per line.
#
# Operate on a repo with "$SKG_DATA_DIR/$repo", e.g.
#   for repo in $(skg_owned_repos); do
#     git -C "$SKG_DATA_DIR/$repo" status
#   done
#
# The repo lists are derived from skgconfig.toml on every call, so adding,
# removing, or re-owning a repo there is reflected immediately -- there is
# no separate list of repositories to keep in sync.

# Resolve paths relative to this library's own location. These stay relative
# -- they mirror however the script was invoked (relative in -> relative out)
# rather than forcing absolute paths -- yet still resolve correctly from any
# cwd, since they track the script file's location, not the current directory.
SKG_LIB_DIR="$(dirname "${BASH_SOURCE[0]}")"
SKG_DATA_DIR="$SKG_LIB_DIR/.."
SKG_CONFIG="${SKG_CONFIG:-$SKG_DATA_DIR/skgconfig.toml}"

# Print the `path` of each [[repos]] block in skgconfig.toml.
# $1 = "all" (every repo) or "owned". A repo is owned unless its
# entry says foreign = true, as the server reads it.
_skg_repos() {
  if [ ! -f "$SKG_CONFIG" ]; then
    echo "lib.sh: config not found: $SKG_CONFIG" >&2
    return 1
  fi
  awk -v mode="$1" '
    function flush() {
      if (have && path != "") {
        if (mode == "all" || ! foreign) print path
      }
      have = 0; path = ""; foreign = 0
    }
    # A [[repos]] header opens a new block...
    /^[[:space:]]*\[\[repos\]\][[:space:]]*$/ { flush(); have = 1; next }
    # ...any other table header ([x] or [[x]]) closes the current one.
    /^[[:space:]]*\[/                           { flush(); next }
    have && /^[[:space:]]*path[[:space:]]*=/ {
      if (match($0, /"[^"]*"/)) path = substr($0, RSTART + 1, RLENGTH - 2)
    }
    have && /^[[:space:]]*foreign[[:space:]]*=[[:space:]]*true([^[:alnum:]_]|$)/ {
      foreign = 1
    }
    END { flush() }
  ' "$SKG_CONFIG"
}

skg_all_repos()   { _skg_repos all; }
skg_owned_repos() { _skg_repos owned; }
