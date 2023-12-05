# Advent of Code 2023

The project uses docker to run `ghc` and `ghci`.

Solutions, input files, and test files should be saved in the corresponding `./solutions/dayXX` directory.

## Building

To build in top-level changes into the docker image (i.e. not solutions), run:

```
./build.sh
```

## Solving

To solve a problem (solutions are mounted in an editable volume), run:

```
./run_solution.sh $day
```

where `day=` the current day you are working on (int).

To run an interactive `ghci` session, run:

```
./ghci.sh
```

## Inside the container

The working directory is `/opt/advent_of_code/`.
The current day's solution directory will be "volumed" to `/opt/advent_of_code/Solutions/`.
When you run `solve.sh` outside the container, `solve.hs` will import the current day's packages, then compile & run inside the container.