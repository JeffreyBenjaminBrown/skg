#!/usr/bin/env bash
# Put Claude's skgrepos on GitHub: one GitHub repo per skgrepo. Then
# put data/claude/ there too, as OWNER/claude-config: Claude's config and
# each specialist's, with each skgrepo as a submodule.
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
# claude-config is PRIVATE too: the config names Jeff's private skgrepos.
#
# To set data/claude/ up on another machine, from the code repo's root:
#   git clone --recurse-submodules git@github.com:skg-kgs/claude-config.git data/claude
#   git -C data/claude submodule foreach -q 'git checkout -q main && git config user.name Claude && git config user.email noreply@anthropic.com'
# A cloned submodule has no branch and no author, and skg cannot commit
# without an author. The servers also need data/jeff/ beside it: a
# config whose foreign skgrepo is missing is refused.
#
# To change one repo's visibility later:
#     gh repo edit OWNER/NAME --visibility private --accept-visibility-change-consequences

set -e

OWNER=skg-kgs      # https://github.com/skg-kgs
CONFIGS=data/claude   # a Git repo of its own; tools/team/make-config-repo.sh made it

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

echo "== $OWNER/claude-config  <-  $CONFIGS"
# A submodule pins one commit of its skgrepo, and every commit to a graph
# moves past its pin. So the pins are recorded here, just before the
# push, and nowhere else. Each submodule's URL is its GitHub repo, relative
# to claude-config's own; a new specialist's skgrepo is added here too.
echo "$REPOS" | while read -r name folder; do
  [ -z "$name" ] && continue
  path="${folder#$CONFIGS/}"
  git -C "$CONFIGS" config -f .gitmodules "submodule.$path.path" "$path"
  git -C "$CONFIGS" config -f .gitmodules "submodule.$path.url" "../$name.git"
  git -C "$CONFIGS" add --no-warn-embedded-repo "$path"   # as 'git submodule add' does
done
( cd "$CONFIGS"
  git add -u   # changed configs, and anything tracked that is gone
  git add .gitmodules skgconfig.toml team/*/skgconfig.toml
  git commit -q -m "The configs, and the commit each graph is at, as pushed" \
    || echo "   (nothing had changed since the last push)" )

if git -C "$CONFIGS" remote | grep -q '^origin$'; then
  git -C "$CONFIGS" push origin main
else
  gh repo create "$OWNER/claude-config" --private --source "$CONFIGS" --remote origin --push
fi

echo "Done."
