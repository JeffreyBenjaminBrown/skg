#!/usr/bin/env bash
# History of the <id>.skg file across every repo you own.
#
# Usage: id-histories.sh <id> [git-log-args...]
#   Everything after <id> is passed straight through to `git log`
#   (e.g. -p for diffs, --oneline, --follow). Nothing is assumed.
#   Repos with no history for that id stay silent.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

if [ -z "${1:-}" ]; then
  echo "usage: id-histories.sh <id> [git-log-args...]" >&2
  exit 1
fi
id="$1"; shift

for repo in $(skg_owned_repos); do
  dir="$SKG_DATA_DIR/$repo"
  [ -d "$dir/.git" ] || continue
  history="$(git -C "$dir" log "$@" -- "$id.skg" 2>/dev/null)"
  if [ -n "$history" ]; then
    echo "=== $repo ==="
    echo "$history"
  fi
done
