#!/bin/bash
# USAGE: bash/docs-server.sh PORT [--deb] [--hot-reload]
#   Starts, in the foreground, a server that owns this checkout's
#   'skg-system/' and nothing else, on PORT, so that an agent can edit
#   the docs' notes in its worktree ('coding-advice/generated-docs.org').
#   It writes '.skg-system-server/skgconfig.toml' (Git ignores the
#   folder), a copy of bash/skg-system.skgconfig.toml with PORT, and
#   runs bash/start-servers.sh on it, by its absolute path, so that a
#   start in one worktree stops no server of another. The options are
#   bash/start-servers.sh's. To stop it:
#     bash/stop-servers.sh "$PWD/.skg-system-server/skgconfig.toml"
#   Never run it in a checkout whose skg-system/ another server owns
#   (Jeff's, in /home/ubuntu/host).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PORT="${1:-}"
case "$PORT" in
  ''|*[!0-9]*) echo "usage: bash/docs-server.sh PORT [--deb] [--hot-reload]" >&2
               exit 2 ;;
esac
shift
mkdir -p "$ROOT/.skg-system-server"
CONFIG="$ROOT/.skg-system-server/skgconfig.toml"
sed "s/^port = .*/port = $PORT/" "$ROOT/bash/skg-system.skgconfig.toml" > "$CONFIG"
cd "$ROOT"
exec bash bash/start-servers.sh "$CONFIG" "$@"
