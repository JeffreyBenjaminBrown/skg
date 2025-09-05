exit # This is not a script, just snippets.

CONTAINER_NAME=skg
NATIVE=/home/jeff/hodal/skg
docker run --name $CONTAINER_NAME -it -d \
  -v $NATIVE:/home/ubuntu                \
  -p 1729:1729                           \
  -p 1730:1730                           \
  --platform linux/amd64                 \
  --user 1000:1000                       \
  jeffreybbrown/hode:latest # CAREFUL! new? latest?

docker stop $CONTAINER_NAME && docker rm $CONTAINER_NAME

docker start $CONTAINER_NAME
docker exec -it $CONTAINER_NAME bash


## duplicated on purpose, for ease of copying
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
