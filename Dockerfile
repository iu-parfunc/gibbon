
# Use the big image from the bintree benchmark as our base.
# Thus you must bild that first:
FROM bintree-bench

RUN apt-get update && apt-get install -y valgrind

# ADD ./.git /trees/.git/
ADD . /trees

RUN cd /trees && DOCKER=1 ./run_all_tests.sh
