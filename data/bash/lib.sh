#!/usr/bin/env bash
# Shared helpers for the data/bash/*.sh repo-loop scripts.
#
# Source this file, then loop over one of:
#   skg_all_repos     -- every source in skgconfig.toml
#   skg_owned_repos   -- only sources with user_owns_it = true
# Each prints one repo path (relative to the data dir) per line.
#
# Operate on a repo with "$SKG_DATA_DIR/$repo", e.g.
#   for repo in $(skg_owned_repos); do
#     git -C "$SKG_DATA_DIR/$repo" status
#   done
#
# The repo lists are derived from skgconfig.toml on every call, so adding,
# removing, or re-owning a source there is reflected immediately -- there is
# no separate list of repositories to keep in sync.

# Resolve paths relative to this library's own location. These stay relative
# -- they mirror however the script was invoked (relative in -> relative out)
# rather than forcing absolute paths -- yet still resolve correctly from any
# cwd, since they track the script file's location, not the current directory.
SKG_LIB_DIR="$(dirname "${BASH_SOURCE[0]}")"
SKG_DATA_DIR="$SKG_LIB_DIR/.."
SKG_CONFIG="${SKG_CONFIG:-$SKG_DATA_DIR/skgconfig.toml}"

# Print the `path` of each [[sources]] block in skgconfig.toml.
# $1 = "all" (every source) or "owned". A source is owned iff its
# path sits under the config's owned_folder (default "owned") --
# the author-folder layout; the per-source user_owns_it key is
# retired.
_skg_sources() {
  if [ ! -f "$SKG_CONFIG" ]; then
    echo "lib.sh: config not found: $SKG_CONFIG" >&2
    return 1
  fi
  local owned_folder
  owned_folder="$(awk '
    /^[[:space:]]*owned_folder[[:space:]]*=/ {
      if (match($0, /"[^"]*"/)) {
        print substr($0, RSTART + 1, RLENGTH - 2); exit } }
  ' "$SKG_CONFIG")"
  owned_folder="${owned_folder:-owned}"
  awk -v mode="$1" -v owned_folder="$owned_folder" '
    function flush() {
      if (have && path != "") {
        owns = (path == owned_folder \
                || index(path, owned_folder "/") == 1)
        if (mode == "all" || owns) print path
      }
      have = 0; path = ""
    }
    # A [[sources]] header opens a new block...
    /^[[:space:]]*\[\[sources\]\][[:space:]]*$/ { flush(); have = 1; next }
    # ...any other table header ([x] or [[x]]) closes the current one.
    /^[[:space:]]*\[/                           { flush(); next }
    have && /^[[:space:]]*path[[:space:]]*=/ {
      if (match($0, /"[^"]*"/)) path = substr($0, RSTART + 1, RLENGTH - 2)
    }
    END { flush() }
  ' "$SKG_CONFIG"
}

skg_all_repos()   { _skg_sources all; }
skg_owned_repos() { _skg_sources owned; }

# Open one process-wide reload bracket on the Skg server.  The caller keeps
# this control connection open until skg_end_reload_batch; EOF is itself a
# safe cleanup signal if the script is interrupted.  Failure to reach Skg is
# deliberately nonfatal: repository work must still be possible offline.
skg_begin_reload_batch() {
  local port request_id response token
  port="$(sed -n 's/^[[:space:]]*port[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' \
           "$SKG_CONFIG" | head -n1)"
  [ -n "$port" ] || return 1
  exec {SKG_RELOAD_BATCH_FD}<>"/dev/tcp/127.0.0.1/$port" || return 1
  request_id="pull-all-$$-$RANDOM-begin"
  printf '((request . "begin reload batch") (request-id . "%s"))\n' \
    "$request_id" >&"$SKG_RELOAD_BATCH_FD"
  response="$(_skg_read_lp_body "$SKG_RELOAD_BATCH_FD")" || {
    exec {SKG_RELOAD_BATCH_FD}>&-
    unset SKG_RELOAD_BATCH_FD
    return 1
  }
  token="$(printf '%s' "$response" \
    | sed -n 's/.*(batch-token \([^() ]*\)).*/\1/p')"
  [ -n "$token" ] || {
    exec {SKG_RELOAD_BATCH_FD}>&-
    unset SKG_RELOAD_BATCH_FD
    return 1
  }
  SKG_RELOAD_BATCH_TOKEN="$token"
}

skg_end_reload_batch() {
  [ -n "${SKG_RELOAD_BATCH_FD:-}" ] || return 0
  if [ -n "${SKG_RELOAD_BATCH_TOKEN:-}" ]; then
    local request_id
    request_id="pull-all-$$-$RANDOM-end"
    printf '((request . "end reload batch") (batch-token . "%s") (request-id . "%s"))\n' \
      "$SKG_RELOAD_BATCH_TOKEN" "$request_id" >&"$SKG_RELOAD_BATCH_FD" || true
    _skg_read_lp_body "$SKG_RELOAD_BATCH_FD" >/dev/null || true
  fi
  exec {SKG_RELOAD_BATCH_FD}>&-
  unset SKG_RELOAD_BATCH_FD SKG_RELOAD_BATCH_TOKEN
}

_skg_read_lp_body() {
  local fd="$1" line length=""
  while IFS= read -r line <&"$fd"; do
    line="${line%$'\r'}"
    [ -z "$line" ] && break
    case "$line" in
      'Content-Length: '*) length="${line#Content-Length: }" ;;
    esac
  done
  [ -n "$length" ] || return 1
  local body=""
  IFS= read -r -N "$length" body <&"$fd" || return 1
  printf '%s' "$body"
}
