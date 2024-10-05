#!/bin/bash

# format day arg
printf -v day "%02d" $1
image_name="btnl_challenge"
container_name="$image_name-day$day-local"
solution_dir="solutions/day$day"

container_base_dir="/opt/btnl_challenge"
container_soln_dir="$container_base_dir/Solution"

docker run \
    --rm \
    -it \
    --name "$container_name" \
    -v "$(pwd)/$solution_dir":"$container_soln_dir" \
    $image_name \
    "$container_base_dir/run_solution.sh" "$container_soln_dir"
