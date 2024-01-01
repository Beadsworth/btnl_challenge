# TODO: check out this installation guide
# https://stackoverflow.com/a/71513191
FROM debian:11


SHELL ["/bin/bash", "-c"]


RUN apt-get update
RUN apt-get install -y \
build-essential \
curl \
git \
libffi-dev \
libffi7 \
libgmp-dev \
libgmp10 \
libncurses-dev \
libncurses5 \
libtinfo5 \
libnuma-dev \
vim


RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
BOOTSTRAP_HASKELL_GHC_VERSION=latest \
BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
BOOTSTRAP_HASKELL_ADJUST_BASHRC=P \
sh


# add ghc to PATH
RUN echo "source /root/.ghcup/env" >> /root/.bashrc
RUN ln -s /root/.ghcup/bin/cabal /usr/bin/cabal
RUN ln -s /root/.ghcup/bin/ghc /usr/bin/ghc
RUN ln -s /root/.ghcup/bin/ghci /usr/bin/ghci


WORKDIR /opt/advent_of_code/

# allow cabal to profile
# RUN sed -i 's/-- library-profiling:/-- library-profiling: True/' /root/.config/cabal/config


RUN cabal update


# Add just the .cabal file to capture dependencies
COPY ./req.cabal /opt/advent_of_code/req.cabal


# all profiling
RUN cabal configure --enable-profiling --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4 --enable-library-profiling


COPY LICENSE /opt/advent_of_code/
COPY run_solution.sh /opt/advent_of_code/
COPY solve.hs /opt/advent_of_code/


CMD ["ghci"]
