#!/usr/bin/env bash
# Put Claude's skgrepos on GitHub: one GitHub repo per skgrepo.
#
# Run from the root of the skg code repo, on the host:
#     tools/team/push-graphs-to-github.sh
#
# Before the first run:
#   1. gh auth login
#   2. Create the organization named in OWNER below, in the browser:
#      https://github.com/account/organizations/new  (choose the Free plan).
#      Or set OWNER to your own GitHub user name instead.
#
# It is safe to run again: a repo that already has the remote is only pushed.
# Every repo is created PRIVATE. To make one public later:
#     gh repo edit OWNER/NAME --visibility public --accept-visibility-change-consequences

set -e

OWNER=kgs-in-skg

# GitHub name   local folder
REPOS="
claude-public   data/claude/public
claude-private  data/claude/private
"
# Each specialist's skgrepo becomes OWNER/team-NAME.
for folder in data/claude/team/*/notes; do
  name=$(basename "$(dirname "$folder")")
  REPOS="$REPOS
team-$name  $folder"
done

echo "$REPOS" | while read -r name folder; do
  [ -z "$name" ] && continue
  echo "== $OWNER/$name  <-  $folder"

  if [ -n "$(git -C "$folder" status --porcelain --untracked-files=no)" ]; then
    echo "   (it has uncommitted changes; only what is committed is pushed)"
  fi

  if git -C "$folder" remote | grep -q '^origin$'; then
    git -C "$folder" push origin main
  else
    gh repo create "$OWNER/$name" --private --source "$folder" --remote origin --push
  fi
done

echo "Done."
