# This gives us Racket 6.6:
FROM ubuntu:16.04

# Method 1: failed - need matching build
# ----------------------------------------
# RUN apt-get update && apt-get install -y racket git
# RUN git clone --recursive http://github.com/racket/racket.git && \
#     cd /racket && git checkout v6.6 && ls

# Method 2:
# ----------------------------------------

RUN apt-get update && apt-get install -y curl


# http://plt.eecs.northwestern.edu/snapshots/current/installers/racket-test-current-x86_64-linux-precise.sh
RUN curl -L -o installer.sh http://download.racket-lang.org/releases/6.6/installers/racket-6.6-x86_64-linux.sh && \
    sh installer.sh --in-place --dest /racket/

ENV PATH "$PATH:/racket/bin"

# ADD harvest.sh /usr/bin/
ADD harvest_all.sh /usr/bin/

RUN cd /racket && harvest_all.sh

# TODO: grab output files and move them to an output volume.
