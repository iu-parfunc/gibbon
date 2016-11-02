
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
	find $@ -size 0 > offenders_empty.txt
	for file in `cat offenders_empty.txt`; do rm $$file; done
	echo "\nOffenders and zero-sized removed.  Done."
	$(MAKE) largest

wget_cleaned:
	wget http://cs.indiana.edu/~rrnewton/files/racket_cleaned_asts_v1.3.tgz


cleaned_racket: rewrite.rkt
	mkdir -p $@
	cd expanded_racket; \
         for f in `find -name "*.sexp"`; do \
          mkdir -p `dirname "../cleaned_racket/$$f"`; \
	  echo "$$f" "../cleaned_racket/$$f"; \
         done | racket ../rewrite.rkt;
	$(MAKE) largest_clean

largest:
	echo "Here are some of the larger files:"
	find expanded_racket/ -type f -printf "%s\t%p\n" | sort -n | tail

largest_clean:
	echo "Here are some of the larger, cleaned files:"
	find cleaned_racket -type f -printf "%s\t%p\n" | sort -n | tail


.PHONY: default fetch largest
