
# We don't check in input data sets to git. We fetch them.

DATASET=racket_expanded_files_v1.0.tgz

default: expanded_racket

fetch: ${DATASET}

$(DATASET):
	wget http://cs.indiana.edu/rrnewton/files/${DATASET}

expanded_racket: ${DATASET}
	mkdir -p $@
	cd $@; tar xf ../${DATASET}
	echo "Here are some of the larger files:"
	find $@/ -type f -printf "%s\t%p\n" | sort -n | tail

