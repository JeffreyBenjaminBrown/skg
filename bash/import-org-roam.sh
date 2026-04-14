set -euo pipefail

# Imports an org-roam directory into skg.
# Runs on the host; delegates the cargo import to a Docker container.
#
# Usage:
#   bash/import-org-roam.sh SOURCE DEST NICKNAME CONTAINER
#
# All paths are relative to the project root (CWD).
#   SOURCE    — path to the org-roam directory
#   DEST      — path to the destination skg source directory
#   NICKNAME  — short name for the source
#   CONTAINER — Docker container name (e.g. skg2)

if [ "$#" -ne 4 ]; then
  echo "Usage: bash/import-org-roam.sh SOURCE DEST NICKNAME CONTAINER"
  exit 1
fi

SOURCE="$1"
DEST="$2"
NICKNAME="$3"
CONTAINER="$4"

SKG_CONFIG="${SKG_CONFIG:-data/skgconfig.toml}"
SKG_PORT="$(grep -m1 '^[[:space:]]*port' "$SKG_CONFIG" \
  | sed 's/.*=[[:space:]]*//' | sed 's/[[:space:]]*#.*//')"

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
# Step 4: Rebuild DBs
#

read -rp "Rebuild databases now? [y/N] " yn
if [[ "$yn" =~ ^[Yy]$ ]]; then
  echo "Sending rebuild-dbs request to port $SKG_PORT ..."
  echo '((request . "rebuild dbs"))' | nc -w1 localhost "$SKG_PORT"
  echo "Rebuild request sent."
  echo "Run M-x skg-close-all-skg-buffers to close stale views."
fi
