# BTNL Challenge
## James Beadsworth
- email: james.beadsworth@gmail.com
- linkedin: [https://www.linkedin.com/in/james-beadsworth/](https://www.linkedin.com/in/james-beadsworth/)

## About
This tool reads a `.csv` file full of `Match` transactions and generates a vwap JSON report.


## Using Docker
For the sake of portability, I am building/testing/running haskell in docker.  If you prefer working with cabal natively, I have added a section for that below.


### Building in Docker

To build inside docker, run:

```
# execute from same directory as README.md
./build.sh
```


### Testing in Docker

After building, you can run simple automated tests inside docker:

```
# execute from same directory as README.md
./test.sh
```

ChatGPT was used to generate a simple test framework.  The ChatGPT logs can be found here:

[ChatGPT Logs](https://chatgpt.com/share/67039c63-e8f8-8008-9b99-1da6131124b9)


### Running in Docker

After building, you can run vwap like so:

```
# execute from same directory as README.md
cat some_file.csv | ./run.sh
```

This will pipe your `.csv` file into `cabal run`


## Building natively with cabal

If desired, you can perform a native build using `cabal`:

```
# move to sub-directory
cd vwap

# build executable
cabal build

# run executable
cat some_file.csv | cabal run

# you can find the bin file with this:
cabal list-bin VWAP
```


## Design

### Method & Execution
A list of transactions could potentially be very long.  I tried to reduce memory usage by streaming stdin and processing the data match-by-match.

Basic program flow:
1. start with an empty Map (dictionary)
2. read a Match from stdin
3. accumulate new Match row into the Map.  At this point, I'm just accumulating sigma-pq and volume
4. after stdin is completely consumed, calculate vwap = sigma-pq / volume
5. format JSON and print to stdout


### Performance
On Apple M1 Pro, I process 1 million rows in ~4 seconds with ~200MB peak memory consumption.

Execution time might improve if I took advantage of more laziness.  Most time is spent decoding/parsing `.csv` rows, so that would be the first place to look for optimizations.

If testing is done through `docker`, these profiling statistics are output to the `./prof` directory:

```
# execute from same directory as README.md
./test.sh
```

### Usage Notes (& assumptions)
- output JSON will be unordered
    - a tool such as `jq` can be used for ordering, validation, etc.
- input `.csv` file is expected to have no formatting errors
- vwap performs little (if any) `.csv` validation
    - integer overflow is definetly possible and should be fixed later
- an empty `.csv` will return an empty JSON object
- a poorly formatted `.csv` (including a file with whitespace-only characters) will throw an error
- a product with zero volume will report `vwap == 0`


### Room for improvement
- validate incoming `.csv`
- Better security for docker
    - specify non-root user
    - prefer to not run ghcup install from curl
- Create docker base image with ghcup pre-installed
    - this could be stored on an in-house artifact repository
