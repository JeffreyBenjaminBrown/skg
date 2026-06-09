# PITFALL: Run this from the project root.

# Reads the port from data/skgconfig.toml by default,
# but the user can specify another path.
SKG_CONFIG="${1:-data/skgconfig.toml}"
if [ ! -f "$SKG_CONFIG" ]; then
  echo "Run this from the project root (can't find $SKG_CONFIG)." >&2
  exit 1
fi
SKG_PORT="$(grep -m1 '^[[:space:]]*port' "$SKG_CONFIG" \
  | sed 's/.*=[[:space:]]*//' | sed 's/[[:space:]]*#.*//')"
# Identity is derived from the checkout directory, so this single committed
# file works unchanged in every clone (skg, skg2, skg3, ...) with no
# per-instance edits to drift out of sync -- that drift is what broke skg3.
HOST="$(pwd)"
CONTAINER_NAME="$(basename "$HOST")"

# Claude Code state (the session transcripts `--resume` reads, prompt history,
# OAuth credentials, config) AND our shared user-level config both live in
# my-dot-claude -- a small git repo that sits INSIDE the project tree at
# $HOST/my-dot-claude. Because the whole project is bind-mounted to
# /home/ubuntu/host below, that state rides along on the one mount and survives
# `docker rm` + rebuild; no separate .claude mount is needed.
#
# CLAUDE_CONFIG_DIR (=/home/ubuntu/host/my-dot-claude) and PIPEWIRE_RUNTIME_DIR
# are now baked into the image (docker.nix), so they are NOT passed with -e
# here. NOTE: this recipe therefore assumes the NEW image -- relaunch with an
# older image that lacks the baked env and Claude falls back to the ephemeral
# ~/.claude, so state won't persist.

docker run --name "$CONTAINER_NAME" -it -d                 \
  -v /run/user/1000/pipewire-0:/run/user/1000/pipewire-0   \
  -p "$SKG_PORT:$SKG_PORT"                                 \
  --platform linux/amd64                                   \
  --user 1000:1000                                         \
  --dns 8.8.8.8 --dns 1.1.1.1                              \
  --ulimit nofile=524288:524288                            \
  -w /home/ubuntu/host                                     \
  -v "$HOST":/home/ubuntu/host                             \
  jeffreybbrown/hode:latest

  # PITFALL: Do not mount over /home/ubuntu itself. That hides the
  #   image-provided ~/.bashrc and the writable ~/.local/npm-global
  #   prefix used by Codex and Claude bootstrap/update logic. Claude state
  #   does NOT need a mount of its own: CLAUDE_CONFIG_DIR points at
  #   $HOST/my-dot-claude, already inside the /home/ubuntu/host bind-mount,
  #   so history/credentials persist there across rebuilds.
  #   (Verified earlier: with CLAUDE_CONFIG_DIR set, Claude writes
  #   .claude.json, projects/, sessions/, and credentials all under that dir.)
  # --ulimit raises the file-descriptor cap for every process in the
  #   container. The default of 1024 is too low for TypeDB under
  #   concurrent test load: each RocksDB database opens many handles,
  #   and exceeding 1024 panics TypeDB's gRPC thread, leaving a
  #   half-alive server that breaks the test suite.
  # The DNS bits, somehow, permit Claude Code to
  #   work through my phone's mobile hotspot.
