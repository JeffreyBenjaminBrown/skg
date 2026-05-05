# PITFALL: Run this from the project root.

# Reads the port from data/skgconfig.toml by default,
# but the user can specify another path.
SKG_CONFIG="${1:-data/skgconfig.toml}"
SKG_PORT="$(grep -m1 '^[[:space:]]*port' "$SKG_CONFIG" \
  | sed 's/.*=[[:space:]]*//' | sed 's/[[:space:]]*#.*//')"
CONTAINER_NAME=skg
HOST=/home/rust/skg

docker run --name "$CONTAINER_NAME" -it -d               \
  -v /run/user/1000/pipewire-0:/run/user/1000/pipewire-0 \
  -e PIPEWIRE_RUNTIME_DIR=/run/user/1000 \
  -p "$SKG_PORT:$SKG_PORT"               \
  --platform linux/amd64                 \
  --user 1000:1000                       \
  --dns 8.8.8.8 --dns 1.1.1.1            \
  --ulimit nofile=524288:524288          \
  -w /home/ubuntu/host                   \
  -v "$HOST":/home/ubuntu/host           \
  jeffreybbrown/hode:latest
  # PITFALL: Do not mount over /home/ubuntu itself. That hides the
  #   image-provided ~/.bashrc and the writable ~/.local/npm-global
  #   prefix used by Codex and Claude bootstrap/update logic.
  # --ulimit raises the file-descriptor cap for every process in the
  #   container. The default of 1024 is too low for TypeDB under
  #   concurrent test load: each RocksDB database opens many handles,
  #   and exceeding 1024 panics TypeDB's gRPC thread, leaving a
  #   half-alive server that breaks the test suite.
  # The DNS bits, somehow, permit Claude Code to
  #   work through my phone's mobile hotspot.
