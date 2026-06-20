#!/usr/bin/env bash
# Show remote + working-tree status for every repo you own.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_owned_repos); do
  echo && echo "===============" && echo "REPO: " "$repo"
  git -C "$SKG_DATA_DIR/$repo" remote -v | head -n 1 | sed 's/.*github.com:/github:/' | sed 's/ (.*//'
  git -C "$SKG_DATA_DIR/$repo" status
done
