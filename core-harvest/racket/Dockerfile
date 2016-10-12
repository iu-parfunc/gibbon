# This gives us Racket 6.6:
FROM ubuntu:16.10

RUN apt-get update && apt-get install -y racket git

RUN git clone --recursive http://github.com/racket/racket.git && \
    cd /racket && git checkout v6.6 && ls

# ADD harvest.sh /usr/bin/
ADD harvest_all.sh /usr/bin/

RUN cd /racket && harvest_all.sh

# TODO: grab output files and move them to one place / or an output
# volume.

