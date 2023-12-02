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

/root/.ghcup/bin/ghc -O2 --make -o "$solution_dir/solve" "$container_base_dir/solve" -prof -auto-all -fforce-recomp && "$solution_dir/solve" +RTS -p -po"$prof_path"
