set -euo pipefail

# Imports an org-roam directory into skg.
# Runs on the host; delegates the cargo import to a Docker container.
#
# Usage:
#   bash/import-org-roam.sh SOURCE DEST NICKNAME
#
# All paths are relative to the project root (CWD).
#   SOURCE    — path to the org-roam directory
#   DEST      — path to the destination skg source directory
#   NICKNAME  — short name for the source
#
# The Docker container name is prompted for interactively at runtime,
# since running this inside the container would leave no API keys on
# the host to sign the resulting git commit.

if [ "$#" -ne 3 ]; then
  echo "Usage: bash/import-org-roam.sh SOURCE DEST NICKNAME"
  exit 1
fi

SOURCE="$1"
DEST="$2"
NICKNAME="$3"

read -rp "IMPORTANT: Name of the docker container to run this in? " CONTAINER
if [ -z "$CONTAINER" ]; then
  echo "No container name given; aborting."
  exit 1
fi

#
# Step 1: Clean stale data
#

read -rp "Remove preexisting .git and *.skg from $DEST? [y/N] " yn
if [[ "$yn" =~ ^[Yy]$ ]]; then
  rm -rf "$DEST/.git"
  rm -f "$DEST"/*.skg
  echo "Cleaned $DEST."
fi

#
# Step 2: Cargo import (inside Docker)
#

echo "Running cargo import-org-roam in container $CONTAINER ..."
docker exec -w /home/ubuntu "$CONTAINER" \
  cargo run --bin skg -- import-org-roam "$SOURCE" "$DEST" "$NICKNAME"
echo "Import complete."

#
# Step 3: Git init + commit
#

TEMPLATE=$(mktemp)
cat > "$TEMPLATE" <<'EOF'
imported from org-roam:
  [FILL IN THIS DETAIL]
as of commit
  [FILL IN THIS DETAIL]
EOF

git -C "$DEST" init
git -C "$DEST" add -A
if git -C "$DEST" commit -e -F "$TEMPLATE"; then
  echo "Committed."
else
  echo "Commit aborted or failed; continuing without a commit."
fi
rm -f "$TEMPLATE"

#
# Step 4: Reconcile and rebuild through an interactive recovery archive
#

read -rp "Rebuild databases now? [y/N] " yn
if [[ "$yn" =~ ^[Yy]$ ]]; then
  echo "A raw rebuild request is no longer safe or supported."
  echo "Run M-x skg-rebuild-dbs in Emacs or :SkgRebuildDbs in Neovim."
  echo "The client will archive dirty work, lock the exact buffer census, and reconcile retained views."
fi
