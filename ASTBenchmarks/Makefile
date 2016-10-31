
# We don't check in input data sets to git. We fetch them.

# 1.0: initial cut from Racket main distro
# 1.1: remove files containing '#<'.  Also fix tarball to include expanded_racket subdir.
# 1.2: larger dataset, from Racket 6.6 main distro.
DATASET=racket_expanded_files_v1.2.tgz

default: cleaned_racket

fetch: ${DATASET}

$(DATASET):
	wget http://cs.indiana.edu/~rrnewton/files/${DATASET}

expanded_racket: ${DATASET}
	mkdir -p $@
	cd $@; time tar xf ../${DATASET}
	echo "\nEliminating files with offending character sequences:"
	grep -risl '#<' ./expanded_racket/ > offenders.txt
	echo "Number of offenders: "`wc -l offenders.txt`
	du -sh $@
	for file in `cat offenders.txt`; do rm $$file; done
	du -sh $@
	echo "\nDone."
	$(MAKE) largest

cleaned_racket: expanded_racket rewrite.rkt
	mkdir -p $@
	raco make rewrite.rkt
	cd expanded_racket; \
         for f in `find -name "*.sexp"`; do \
          echo "Build directory "`dirname "../cleaned_racket/$$f"`; \
          mkdir -p `dirname "../cleaned_racket/$$f"`; \
	  cat "$$f" | racket ../rewrite.rkt > "../cleaned_racket/$$f"; \
         done;

largest:
	echo "Here are some of the larger files:"
	find expanded_racket/ -type f -printf "%s\t%p\n" | sort -n | tail


.PHONY: default fetch largest
