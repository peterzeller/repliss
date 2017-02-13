#!/bin/bash
docker run \
        --detach \
        --restart always \
        --publish "8081:8080" \
        --volume /home/peter/tmp/repliss:/opt/repliss/model/ \
        repliss
