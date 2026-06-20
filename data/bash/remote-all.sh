#!/usr/bin/env bash
# git remote -v for every repo (yours and others').
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_all_repos); do
  echo && echo "===============" && echo "REPO: " "$repo"
  git -C "$SKG_DATA_DIR/$repo" remote -v
done
