#!/bin/bash
set -euo pipefail

# format day arg
image_name="btnl_challenge"
container_name="$image_name-local"

docker kill "$container_name"
