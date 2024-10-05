#!/bin/bash

image_name="btnl_challenge"
container_name="$image_name-ghci-local"


docker run \
    --rm \
    -it \
    --name "$container_name" \
    $image_name
