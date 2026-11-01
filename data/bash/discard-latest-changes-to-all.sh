#!/usr/bin/env bash
# Discard uncommitted changes in every repo you own, resetting each
# working tree (and index) back to its last commit.
#
# This throws away both staged and unstaged modifications to tracked
# files, and deletes untracked files and directories -- there is no undo.
# Each owned repo is left exactly at its last commit.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

for repo in $(skg_owned_repos); do
  echo
  echo "$repo"
  git -C "$SKG_DATA_DIR/$repo" reset --hard
  git -C "$SKG_DATA_DIR/$repo" clean -fd
done
