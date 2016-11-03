# FROM fpco/stack-build:lts-7.1
FROM fpco/stack-build:lts-6.23

RUN apt-get update && apt-get -y install mlton ocaml-native-compilers gcc time

# TODO: Check SHA like Nix does:
RUN cd / && wget -nv https://github.com/cisco/ChezScheme/archive/v9.4.tar.gz && \
    tar xf v9.4.tar.gz && rm -f v9.4.tar.gz

RUN cd /ChezScheme-9.4/ && ./configure && time make install

ADD ./deps /tree-velocity/BintreeBench/deps

RUN /tree-velocity/BintreeBench/deps/rustup.sh --revision=1.12.0

# This gets 6.3, too old:
# RUN apt-get install -y racket
RUN cd /tmp/ && \
  wget -nv http://download.racket-lang.org/releases/6.6/installers/racket-6.6-x86_64-linux.sh && \
  chmod +x racket-6.6-x86_64-linux.sh && \
  ./racket-6.6-x86_64-linux.sh --in-place --dest /racket/ && \
  ln -s /racket/bin/* /usr/local/bin/ && \
  rm -rf racket-6.6-x86_64-linux.sh

# ------------------------------------------------------------

ADD . /BintreeBench

# Build all the benchmarks:
RUN scheme --version && rustc --version && racket --version && \
    cd /BintreeBench && make 

# For testing purposes, make sure they all run:
RUN cd /BintreeBench && make run_small
