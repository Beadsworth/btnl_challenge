#!/bin/bash
start_time="$(date +"%y_%m_%d-%H_%M_%S")"
container_base_dir="/vwap"
prof_dir="/prof"
prof_path="$prof_dir/$start_time.prof"

# create prof dir if needed
if [[ ! -d $prof_dir ]]
then
    mkdir -p $prof_dir
fi

cabal run main -- +RTS -p -po"$prof_path"
