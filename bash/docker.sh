# Reads the port from data/skgconfig.toml by default,
# but the user can specify another path.
SKG_CONFIG="${1:-data/skgconfig.toml}"
SKG_PORT="$(grep -m1 '^[[:space:]]*port' "$SKG_CONFIG" \
  | sed 's/.*=[[:space:]]*//' | sed 's/[[:space:]]*#.*//')"

CONTAINER_NAME=skg
HOST=/home/rust/skg

docker run --name "$CONTAINER_NAME" -it -d               \
  -v /run/user/1000/pipewire-0:/run/user/1000/pipewire-0 \
  -e PIPEWIRE_RUNTIME_DIR=/run/user/1000                 \
  -p "$SKG_PORT:$SKG_PORT"      \
  --platform linux/amd64        \
  --user 1000:1000              \
  --dns 8.8.8.8 --dns 1.1.1.1  \
  -v "$HOST":/home/ubuntu       \
  jeffreybbrown/hode:latest
  # The DNS bits, somehow, permit Claude Code to
  # work through my phone's mobile hotspot.
