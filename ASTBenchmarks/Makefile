
default: cleaned_racket

# We don't check in input data sets to git. We fetch them.

# 1.0: initial cut from Racket main distro
# 1.1: remove files containing '#<'.  Also fix tarball to include expanded_racket subdir.
# 1.2: larger dataset, from Racket 6.6 main distro.
DATASET=racket_expanded_files_v1.2.tgz

fetch: ${DATASET}

$(DATASET):
	wget http://cs.indiana.edu/~rrnewton/files/${DATASET}

# Updated to no-list version, v1.6 [2016.11.04]:
wget_cleaned:
	wget http://cs.indiana.edu/~rrnewton/files/racket_cleaned_asts_v1.6.tgz
	tar xf racket_cleaned_asts_v1.6.tgz

# Updated to version with .gpkd file exts, v1.7 [2017.01.09]
wget_binary:
	wget http://cs.indiana.edu/~rrnewton/files/racket_binary_packed_asts_v1.7.tgz
	tar xf racket_binary_packed_asts_v1.7.tgz

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


binary_racket: 
	mkdir -p $@
	rm -f ./conversion_list.txt
	cd cleaned_racket; \
          for f in `find -name "*.sexp"`; do \
            mkdir -p `dirname "../binary_racket/$$f"`; \
            echo "$$f" ../binary_racket/`dirname $$f`/`basename $$f .sexp`.gpkd >> ../conversion_list.txt; \
          done
	cd cleaned_racket; \
         cat ../conversion_list.txt | racket ../binary_pack.rkt;

.PHONY: default fetch largest
