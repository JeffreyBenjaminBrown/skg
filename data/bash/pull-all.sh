#!/usr/bin/env bash
# Pull every repo (yours and others').
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_all_repos); do
  echo "STAY for a second -- this will prompt for password."
  echo
  echo "$repo"
  git -C "$SKG_DATA_DIR/$repo" pull
done
