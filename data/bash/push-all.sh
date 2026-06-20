#!/usr/bin/env bash
# Push every repo you own.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_owned_repos); do
  echo
  echo "$repo"
  git -C "$SKG_DATA_DIR/$repo" push
done
