# TODO: check out this installation guide
# https://stackoverflow.com/a/71513191
FROM debian:11

SHELL ["/bin/bash", "-c"]

WORKDIR /opt/advent_of_code/solution/


RUN apt-get update
RUN apt-get install -y \
build-essential \
curl \
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


RUN echo "source /root/.ghcup/env" >> /root/.bashrc
#RUN cabal update

# Add just the .cabal file to capture dependencies
#COPY ./req.cabal /opt/project_euler/req.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
#RUN cabal build --only-dependencies -j4

# Add and Install Application Code
#COPY . /opt/project_euler
#RUN cabal install

COPY run_solution.sh /opt/advent_of_code/


CMD ["/root/.ghcup/bin/ghci"]
