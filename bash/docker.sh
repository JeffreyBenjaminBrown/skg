# PITFALL: Run this from the project root.

# Reads the port from data/skgconfig.toml by default,
# but the user can specify another path.
SKG_CONFIG="${1:-data/skgconfig.toml}"
SKG_PORT="$(grep -m1 '^[[:space:]]*port' "$SKG_CONFIG" \
  | sed 's/.*=[[:space:]]*//' | sed 's/[[:space:]]*#.*//')"
CONTAINER_NAME=skg
HOST=/home/rust/skg

# Where Claude Code keeps its state: session transcripts (what --resume
# reads), prompt history, OAuth credentials, and config. By default this
# lives in the container's writable layer under /home/ubuntu, which is
# DISCARDED on `docker rm` + rebuild -- that's why sessions can't be
# resumed after rebuilding. Bind-mounting a host directory there makes it
# survive. Override the host location with:
#   CLAUDE_STATE=/some/host/dir ./bash/docker.sh
CLAUDE_STATE="${CLAUDE_STATE:-$(dirname "$HOST")/skg-claude}"
# The bind-mount source must exist before `docker run`; create it as the
# host user so it's owned by uid 1000 and writable by the container user.
mkdir -p "$CLAUDE_STATE"

docker run --name "$CONTAINER_NAME" -it -d               \
  -v /run/user/1000/pipewire-0:/run/user/1000/pipewire-0 \
  -e PIPEWIRE_RUNTIME_DIR=/run/user/1000                 \
  -e CLAUDE_CONFIG_DIR=/home/ubuntu/.claude              \
  -p "$SKG_PORT:$SKG_PORT"                               \
  --platform linux/amd64                                 \
  --user 1000:1000                                       \
  --dns 8.8.8.8 --dns 1.1.1.1                            \
  --ulimit nofile=524288:524288                          \
  -w /home/ubuntu/host                                   \
  -v "$HOST":/home/ubuntu/host                           \
  -v "$CLAUDE_STATE":/home/ubuntu/.claude                \
  jeffreybbrown/hode:latest

  # PITFALL: Do not mount over /home/ubuntu itself. That hides the
  #   image-provided ~/.bashrc and the writable ~/.local/npm-global
  #   prefix used by Codex and Claude bootstrap/update logic.
  #   Mounting the *subpath* /home/ubuntu/.claude is fine: the image
  #   bakes nothing there, so it shadows nothing, and it is what lets
  #   Claude history/credentials survive a rebuild. CLAUDE_CONFIG_DIR is
  #   set to that same path so Claude's config file (normally
  #   ~/.claude.json, a sibling of ~/.claude) is written INSIDE the
  #   mount too -- otherwise it stays at $HOME and is lost on rebuild.
  #   (Verified: with CLAUDE_CONFIG_DIR set, Claude puts .claude.json,
  #   projects/, sessions/, and credentials all under that one dir.)
  # --ulimit raises the file-descriptor cap for every process in the
  #   container. The default of 1024 is too low for TypeDB under
  #   concurrent test load: each RocksDB database opens many handles,
  #   and exceeding 1024 panics TypeDB's gRPC thread, leaving a
  #   half-alive server that breaks the test suite.
  # The DNS bits, somehow, permit Claude Code to
  #   work through my phone's mobile hotspot.
