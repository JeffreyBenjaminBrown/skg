exit # This is not a script, just snippets.

CONTAINER_NAME=skg
docker start $CONTAINER_NAME
docker exec -it $CONTAINER_NAME bash

HOST=/home/jeff/hodal/skg
docker run --name "$CONTAINER_NAME" -it -d               \
  -v /run/user/1000/pipewire-0:/run/user/1000/pipewire-0 \
  -e PIPEWIRE_RUNTIME_DIR=/run/user/1000                 \
  --network host              \
  --platform linux/amd64      \
  --user 1000:1000            \
  --dns 8.8.8.8 --dns 1.1.1.1 \
  -v "$HOST":/home/ubuntu     \
  jeffreybbrown/hode:latest
  # The DNS bits, somehow, permit Claude Code to
  # work through my phone's mobile hotspot.
  # --newtowrk host binds each port to the host's port of the same number --
  # e.g. 1729=1729 (TypeDB).

docker stop $CONTAINER_NAME && docker rm $CONTAINER_NAME

STARTING_AT=$(date)
echo $(date)
docker build -t jeffreybbrown/hode:new .
echo $(date)

DOCKER_IMAGE_SUFFIX="2025-03-13.tantivy"
docker tag jeffreybbrown/hode:new jeffreybbrown/hode:latest
docker tag jeffreybbrown/hode:new jeffreybbrown/hode:$DOCKER_IMAGE_SUFFIX
docker rmi jeffreybbrown/hode:new

docker push jeffreybbrown/hode:$DOCKER_IMAGE_SUFFIX
docker push jeffreybbrown/hode:latest
