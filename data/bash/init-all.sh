#!/usr/bin/env bash
# git init every repo you own.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_owned_repos); do
  echo && echo "===============" && echo "REPO: " "$repo"
  git -C "$SKG_DATA_DIR/$repo" init
done
