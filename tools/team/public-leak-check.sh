#!/usr/bin/env bash
# public-leak-check.sh -- does Claude's public skgrepo name a node that is not public?
#
#   tools/team/public-leak-check.sh
#
# Claude's server reads Jeff's private skgrepos. A fork copies the
# original's title, body and list of children, so a fork of one of
# Jeff's private notes, or a public note that contains or links to one,
# would publish something of his once 'data/claude/public' is pushed.
# This prints every ID in a '.skg' file of 'data/claude/public' (in a
# list, or in an '[[id:...]]' link) that is the ID of no node in a
# public skgrepo: Jeff's public one, Claude's public one, or a
# specialist's. It exits 1 if it printed any. Run it before committing
# 'data/claude/public'.
set -euo pipefail
DATA="$(cd "$(dirname "$0")/../../data" && pwd)"
found=0
for file in "$DATA"/claude/public/*.skg; do
  for id in $(grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' "$file" | sort -u); do
    [ -f "$DATA/jeff/public/$id.skg" ] && continue
    [ -f "$DATA/claude/public/$id.skg" ] && continue
    ls "$DATA"/claude/team/*/"$id.skg" >/dev/null 2>&1 && continue
    where="in no skgrepo"
    for repo in "$DATA"/jeff/*/ "$DATA"/claude/private/; do
      [ -f "$repo$id.skg" ] && where="in ${repo#"$DATA"/}"
    done
    echo "$(basename "$file") ($(sed -n 's/^title: //p' "$file" | cut -c1-40)) names $id, which is $where"
    found=1
  done
done
exit $found
