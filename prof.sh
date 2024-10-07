#!/bin/bash
start_time="$(date +"%y_%m_%d-%H_%M_%S")"
container_base_dir="/vwap"
prof_dir="/prof"
prof_sub_dir="$prof_dir/$start_time"
prof_path="$prof_sub_dir/profiling"

# create prof dir if needed
if [[ ! -d $prof_sub_dir ]]
then
    mkdir -p $prof_sub_dir
fi

cabal run VWAP -- +RTS -p -po"$prof_path" -s"$prof_path.s"
