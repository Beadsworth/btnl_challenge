#!/bin/bash
set -euo pipefail

OUTPUT_DIR="./package"
OUTPUT_PATH="$OUTPUT_DIR/beadsworth.tar.gz"

# create package dir if needed
if [[ ! -d "$OUTPUT_DIR" ]]
then
    mkdir -p "$OUTPUT_DIR"
fi

tar -czvf "$OUTPUT_PATH" .
