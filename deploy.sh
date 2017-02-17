#!/usr/bin/env bash
# script for building the docker image and pushing it to Dockerhub
set -e

sbt assembly
docker build -t peterzel/repliss .
docker push peterzel/repliss

ssh zeller@lamport <<'ENDSSH'
#commands to run on lamport
docker pull peterzel/repliss
docker stop repliss
docker rm repliss
docker run \
        --detach \
        --restart always \
        --publish "8081:8080" \
        --volume /home/zeller/repliss:/opt/repliss/model/ \
	--name repliss \
        peterzel/repliss
ENDSSH
echo "done"