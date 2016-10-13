
# We don't check in input data sets to git. We fetch them.

# 1.0: initial cut from Racket main distro
# 1.1: remove files containing '#<'.  Also fix tarball to include expanded_racket subdir.
DATASET=racket_expanded_files_v1.1.tgz

default: expanded_racket

fetch: ${DATASET}

$(DATASET):
	wget http://cs.indiana.edu/rrnewton/files/${DATASET}

expanded_racket: ${DATASET}
	tar xf ./${DATASET}
	echo "Here are some of the larger files:"
	find $@/ -type f -printf "%s\t%p\n" | sort -n | tail

