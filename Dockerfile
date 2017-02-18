
# Use the big image from the bintree benchmark as our base.
# Thus you must bild that first:
FROM bintree-bench

RUN apt-get update && apt-get install -y valgrind

# Cache Gibbon build and test dependencies in the container
WORKDIR /trees
COPY ./gibbon-compiler/stack.yaml ./gibbon-compiler/gibbon.cabal /trees/gibbon-compiler/
RUN cd /trees/gibbon-compiler/ && stack build gibbon --test --only-dependencies

# ADD ./.git /trees/.git/
ADD . /trees

RUN cd /trees && DOCKER=1 ./run_all_tests.sh
