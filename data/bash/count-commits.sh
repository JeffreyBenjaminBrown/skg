#!/usr/bin/env bash
# Count Jeffrey's commits in every repo (yours and others').
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_all_repos); do
  printf '%s ' "$repo"
  git -C "$SKG_DATA_DIR/$repo" log | grep -e "^Author: Jeffrey" | wc
done
