#!/bin/bash

# format day arg
image_name="btnl_challenge"
container_name="$image_name-local"
output_dir="output"
prof_dir="vwap/prof"

container_base_dir="/vwap"
container_output_dir="$container_base_dir/$output_dir"
container_prof_dir="/$prof_dir"

cat | docker run \
    --rm \
    -i \
    --name "$container_name" \
    -v "$(pwd)/$prof_dir":"$container_prof_dir" \
    $image_name \
    "$container_base_dir/prof.sh"
