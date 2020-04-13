#!/bin/sh

# Example non-Linux platforms for rhub:
#  "macos-elcapitan-release"
#  "windows-x86_64-devel"

set -e

dir_script=$(dirname $(readlink -f "$0"))
dir_root=$dir_script/../
dir_package=$(basename $(dirname $dir_script))
image_name=index.docker.io/nathaneastwood/rci:latest
container_name=${dir_package}_check

docker login --username nathaneastwood --password $DOCKER_PASSWORD
docker pull $image_name

docker run -id --name $container_name $image_name bash
docker cp $dir_root $container_name:/root

docker exec \
  --workdir /root/$dir_package \
  --env POORMAN_TOKEN_CODECOV=$POORMAN_TOKEN_CODECOV \
  $container_name \
  Rscript "$@"

docker stop $container_name
docker rm $container_name
