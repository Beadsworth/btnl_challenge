#!/bin/bash

image_name="advent_of_code_2023"
container_name="$image_name-ghci-local"


docker run \
    --rm \
    -it \
    --name "$container_name" \
    $image_name
