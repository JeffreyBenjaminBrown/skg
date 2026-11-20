#!/usr/bin/env bash
# public-leak-check.sh -- does Claude's public skgrepo name a node that is not public?
#
#   tools/team/public-leak-check.sh
#
# Claude's server reads Jeff's private skgrepos. A fork copies the
# original's title, body and list of children, so a fork of one of
# Jeff's private notes, or a public note that contains one, would
# publish something of his once 'data/claude/public' is pushed.
#
# A node is public if its file is in Jeff's public skgrepo, Claude's
# public one, a specialist's, or the code repo's 'skg-system/'; an ID
# listed under a public node's 'extra_ids' counts as that node's. For
# each '.skg' file of 'data/claude/public', this prints every ID that:
#   - its title or body names (an '[[id:...]]' link, or a bare ID),
#     unless the ID is public or a note of Jeff's 'deciduous' skgrepo
#     (Jeff allows links to those);
#   - its 'forks' list or its attribution's 'original' names, if that
#     ID is a note of a non-public skgrepo. These are records of where
#     the fork came from, not links, so an original that no longer
#     exists anywhere is no leak;
#   - any other list names ('contains', 'hides', ...), unless public.
# Its own 'pid' and 'extra_ids' are skipped. It also prints an
# attribution whose 'skgrepo' is not one of the public skgrepos of
# 'data/claude/skgconfig.toml'. It exits 1 if it printed anything.
# Run it before committing 'data/claude/public'.
set -euo pipefail
DATA="$(cd "$(dirname "$0")/../../data" && pwd)"
CODE_ROOT="$(dirname "$DATA")"

public_folders=( "$DATA/jeff/public" "$DATA/claude/public" )
for folder in "$DATA"/claude/team/*/notes "$CODE_ROOT/skg-system"; do
  [ -d "$folder" ] && public_folders+=( "$folder" )
done
linkable_folders=( "$DATA/jeff/deciduous" )

declare -A public_id
while read -r id; do public_id[$id]=1; done < <(
  for folder in "${public_folders[@]}"; do
    [ -d "$folder" ] || continue
    find "$folder" -maxdepth 1 -name '*.skg' -printf '%f\n' | sed 's/\.skg$//'
    # Extra IDs: the list items under a top-level 'extra_ids:'.
    find "$folder" -maxdepth 1 -name '*.skg' -print0 \
      | xargs -0 -r grep -l '^extra_ids:' \
      | xargs -r awk '/^[a-z_]+:/ { key = $1 }
                      key == "extra_ids:" && /^- / { print $2 }' || true
  done )

is_in () { # ID FOLDER... -- is ID the file of a note in one of FOLDERs?
  local id=$1; shift
  local folder
  for folder in "$@"; do [ -f "$folder/$id.skg" ] && return 0; done
  return 1
}

where_is () { # ID -- the private skgrepo folder holding ID, or "no skgrepo"
  local where="no skgrepo" repo
  for repo in "$DATA"/jeff/*/ "$DATA"/claude/private/; do
    [ -f "$repo$1.skg" ] && where="${repo#"$DATA"/}"
  done
  echo "$where"
}

# The names of public skgrepos in Claude's config: those whose path is
# one of the public folders.
declare -A public_skgrepo
while read -r name path; do
  resolved="$(realpath -m "$DATA/claude/$path")"
  for folder in "${public_folders[@]}"; do
    [ "$resolved" = "$(realpath -m "$folder")" ] && public_skgrepo[$name]=1
  done
done < <(awk -F' *= *' '
  /^\[\[repos\]\]/ { if (name != "") print name, path; name = ""; path = "" }
  $1 == "name" { gsub(/"/, "", $2); name = $2 }
  $1 == "path" { gsub(/"/, "", $2); path = $2 }
  END { if (name != "") print name, path }' "$DATA/claude/skgconfig.toml")

# Each ID a '.skg' file of 'data/claude/public' names, as "FILE KIND
# ID": KIND is 'own' (pid, extra_ids), 'text' (title, body), 'record'
# (forks, attribution's original), 'skgrepo' (attribution's skgrepo, a
# name, not an ID) or 'list' (anything else).
named_ids () {
  find "$DATA/claude/public" -maxdepth 1 -name '*.skg' -print0 \
    | xargs -0 -r awk '
    FNR == 1 { key = "" }
    /^[a-z_]+:/ { key = $1; sub(/:$/, "", key) }
    key == "attribution" && /^  skgrepo:/ { print FILENAME, "skgrepo", $2; next }
    key == "attribution" && /^  original:/ { print FILENAME, "record", $2; next }
    { kind = "list"
      if (key == "pid" || key == "extra_ids") kind = "own"
      else if (key == "title" || key == "body") kind = "text"
      else if (key == "forks") kind = "record"
      else if (key == "attribution") next
      line = $0
      while (match(line, /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/)) {
        print FILENAME, kind, substr(line, RSTART, RLENGTH)
        line = substr(line, RSTART + RLENGTH) } }' | sort -u
}

label () { echo "$(basename "$1") ($(sed -n 's/^title: //p' "$1" | cut -c1-40))"; }

found=0
while read -r file kind id; do
  [ "$kind" = own ] && continue
  if [ "$kind" = skgrepo ]; then
    [ -n "${public_skgrepo[$id]:-}" ] && continue
    echo "$(label "$file") is a fork of a note of skgrepo $id, which is not public"
    found=1; continue
  fi
  [ -n "${public_id[$id]:-}" ] && continue
  where="$(where_is "$id")"
  case "$kind" in
    text) is_in "$id" "${linkable_folders[@]}" && continue ;;
    record) [ "$where" = "no skgrepo" ] && continue
            echo "$(label "$file") is a fork of $id, which is in $where"
            found=1; continue ;;
  esac
  echo "$(label "$file") names $id, which is in $where"
  found=1
done < <(named_ids)
exit $found
