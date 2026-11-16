#!/usr/bin/env bash
# Put Claude's skgrepos on GitHub: one GitHub repo per skgrepo.
#
# Run from the root of the skg code repo, on the host:
#     tools/team/push-graphs-to-github.sh
#
# Before the first run:
#   1. gh auth login
#   2. tools/team/pacer.sh pause     (so no session is mid-save)
#
# It is safe to run again: a repo that already has the remote is only pushed.
#
# Every repo is created PUBLIC, except claude-private, which is PRIVATE:
# it is the only one that may mention Jeff's private nodes. Before
# anything is created, tools/team/public-leak-check.sh must find no
# mention of a private node in what is about to become public; if it
# finds one, this script stops and creates nothing.
# To change one repo's visibility later:
#     gh repo edit OWNER/NAME --visibility private --accept-visibility-change-consequences

set -e

OWNER=skg-kgs      # https://github.com/skg-kgs

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

echo "Checking that nothing about to be public mentions a private node ..."
tools/team/public-leak-check.sh

echo "$REPOS" | while read -r name folder; do
  [ -z "$name" ] && continue
  echo "== $OWNER/$name  <-  $folder"

  if [ -n "$(git -C "$folder" status --porcelain --untracked-files=no)" ]; then
    echo "   (it has uncommitted changes; only what is committed is pushed)"
  fi

  if git -C "$folder" remote | grep -q '^origin$'; then
    git -C "$folder" push origin main
  else
    visibility=--public
    [ "$name" = claude-private ] && visibility=--private
    gh repo create "$OWNER/$name" $visibility --source "$folder" --remote origin --push
  fi
done

echo "Done."
