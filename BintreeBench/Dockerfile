# FROM fpco/stack-build:lts-7.1
FROM fpco/stack-build:lts-6.19

RUN apt-get update && apt-get -y install mlton ocaml-native-compilers gcc time

# TODO: Check SHA like Nix does:
RUN cd / && wget https://github.com/cisco/ChezScheme/archive/v9.4.tar.gz && \
    tar xf v9.4.tar.gz && rm -f v9.4.tar.gz

RUN cd /ChezScheme-9.4/ && ./configure && time make install

ADD ./deps /tree-velocity/deps

# RUN curl -sSf https://static.rust-lang.org/rustup.sh | sh -s -- --revision=1.14.0-beta
# RUN /tree-velocity/deps/rustup.sh --revision=1.13
# RUN /tree-velocity/deps/rustup.sh --revision=1.13.0
# RUN /tree-velocity/deps/rustup.sh --revision=1.13.0-beta
RUN /tree-velocity/deps/rustup.sh --revision=1.12.0
# RUN /tree-velocity/deps/rustup.sh --revision=1.0.0-beta
# RUN /tree-velocity/deps/rustup.sh --revision=1.4.0

# RUN which -a rustc && rustc --version

RUN apt-get install -y racket

# ------------------------------------------------------------

ADD . /tree-velocity

# Build all the benchmarks:
RUN scheme --version && rustc --version && \
    cd /tree-velocity && make 

# For testing purposes, make sure they all run:
RUN cd /tree-velocity/ && make run_small
