#!/bin/bash
set -euo pipefail

image_name="btnl_challenge"

docker build -t $image_name .
