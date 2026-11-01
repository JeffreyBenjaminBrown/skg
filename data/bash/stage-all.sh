#!/usr/bin/env bash
# Stage all changes (new, modified, deleted) in every repo you own.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_owned_repos); do
  echo
  echo "$repo"
  git -C "$SKG_DATA_DIR/$repo" add -A
  git -C "$SKG_DATA_DIR/$repo" status --short
done
