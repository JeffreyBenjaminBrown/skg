exit # This is not a script, just snippets.

CONTAINER_NAME=skg
docker start $CONTAINER_NAME
docker exec -it $CONTAINER_NAME bash

HOST=/home/jeff/hodal/skg
docker run --name "$CONTAINER_NAME" -it -d               \
  -v /run/user/1000/pipewire-0:/run/user/1000/pipewire-0 \
  -e PIPEWIRE_RUNTIME_DIR=/run/user/1000 \
  --ipc=host                             \
  --network host              \
  --platform linux/amd64      \
  --user 1000:1000            \
  --dns 8.8.8.8 --dns 1.1.1.1 \
  -v "$HOST":/home/ubuntu     \
  jeffreybbrown/hode:latest
  # The DNS bits, somehow, permit Claude Code to
  # work through my phone's mobile hotspot.
  # --network host binds each port to the host's port of the same number --
# e.g. 7687=7687 (Neo4j Bolt).
  # The Pipewire and IPC lines are for sound.

docker stop $CONTAINER_NAME && docker rm $CONTAINER_NAME
