#!/usr/bin/env bash
# Pull every repo (yours and others').
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

if skg_begin_reload_batch; then
  echo "Skg reload batch opened."
  trap 'status=$?; skg_end_reload_batch; exit "$status"' EXIT
else
  echo "Skg server unavailable; pulling without a reload batch." >&2
fi

for repo in $(skg_all_repos); do
  echo "STAY for a second -- this will prompt for password."
  echo
  echo "$repo"
  git -C "$SKG_DATA_DIR/$repo" pull
done
