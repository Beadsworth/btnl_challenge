#!/bin/bash
start_time="$(date +"%y_%m_%d-%H_%M_%S")"
solution_dir=$1
prof_dir="$solution_dir/prof"
prof_path="$prof_dir/soln.$start_time"

# create prof dir if needed
if [[ ! -d $prof_dir ]]
then
    mkdir -p $prof_dir
fi

/root/.ghcup/bin/ghc -O2 --make $solution_dir/soln -prof -auto-all -fforce-recomp && $solution_dir/soln +RTS -p -po"$prof_path"
