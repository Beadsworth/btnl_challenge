# Docker installation inspired by:
# this is a debian-12 img with ghcup installed
FROM beadsworth/haskell-base


WORKDIR /vwap/


RUN cabal update


# Add just the .cabal file to capture dependencies
COPY ./vwap/req.cabal /vwap/req.cabal


# all profiling
RUN cabal configure --enable-profiling --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4 --enable-library-profiling


COPY ./vwap /vwap/
COPY ./testing /testing


RUN cabal build


CMD ["ghci"]
