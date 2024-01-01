#!/bin/bash
start_time="$(date +"%y_%m_%d-%H_%M_%S")"
container_base_dir="/opt/advent_of_code"
solution_dir=$1
prof_dir="$solution_dir/prof"
prof_path="$prof_dir/solve.$start_time"

# create prof dir if needed
if [[ ! -d $prof_dir ]]
then
    mkdir -p $prof_dir
fi

cabal build
cabal run solve -- +RTS -p -po"$prof_path"
