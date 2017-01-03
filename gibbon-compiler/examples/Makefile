
default:
	rm -f *.err
	$(MAKE) test

ALL_TESTS= $(shell ls test*.sexp)
ANSWERS= $(ALL_TESTS:.sexp=.ans) 
# ANSWERS= $(INTERP1_TESTS:.sexp=.ans) $(INTERP2_TESTS:.sexp=.ans) $(PASSING_TESTS:.sexp=.ans) $(PACKED_TESTS:.sexp=.ans)

# Here's a hacky way to try several things until one works:
PROCS = $(shell nproc || \
          getconf _NPROCESSORS_ONLN || \
          stack exec -- ghc -e GHC.Conc.getNumProcessors || \
          python -c 'import multiprocessing as m; print m.cpu_count()' )
# On Mac: sysctl -n hw.ncpu
# On Windows?


test: prebuild build
# Having weird trouble with race conditons on answers:
	$(MAKE) answers
	wc -l $(ANSWERS)
	@echo "Answers available, now proceeding..."
	$(MAKE) parse_all
	$(MAKE) core_tests

# Assumes answers and build already available:
core_tests:
	$(MAKE) run_tests 
	$(MAKE) run_tests_packed
	$(MAKE) run_tests_interp1
	$(MAKE) run_tests_interp2

#	$(MAKE) racket_valid
# Need methodology for this.
#	$(MAKE) racket_invalid


EXTRA_SUFFIX ?= .ptr
# How far to compile, --toC, --exe, --run:
RUN_MODE     ?= --run
# Additional args besides RUN_MODE:
TREECARGS ?= --optc=-g


# Not necessary, because we build each module before we run it:
prebuild: test24_input.bin
	raco make -j $(PROCS) -v $(ALL_TESTS) 


test24_input.bin: test24_input.dat test24_packit.rkt test24_defs.sexp
	racket test24_packit.rkt

#----------------------------------------
# Test direct Racket execution:
#----------------------------------------
racket_valid: treelang_lib
	@echo "\nGenerating canonical answers from Racket:"
	@echo "-----------------------------------------"
	for f in `ls test*.sexp`; do racket $$f | tee $$f.ans; done

racket_invalid: treelang_lib
	for f in `find ./error/ -name '*.sexp'`; do racket $$f | tee $$f.ans; done

# ----------------------------------------

TREEC= DEBUG=1 stack exec -- gibbon

# Make sure the compiler is built
build:
	stack build

parse_all: build
	@echo "\n Parsing all tests, even those that don't run yet."
	@echo "--------------------------------------------------"
	rm -f *.parsed
	$(MAKE) all_parsed


all_parsed:  $(ALL_TESTS:.sexp=.parsed)

%.parsed : %.sexp
#	@echo "Parsing $^"
	$(TREEC) --parse $^ 2> $@

# Failing tests for the pointer-based C backend (default):
FAILING_TESTS ?= \
  test13_build.sexp test13b_build.sexp \
  test20_bintree.sexp test20b_bintree.sexp test20c_bintree.sexp  test20d_bintree.sexp test20e_bintree.sexp test20f_bintree.sexp \
  test11_fundata.sexp test11c_funrec.sexp test11e_funrec.sexp test12c_traverse.sexp \
  test25b_racketcore.sexp  \
  test16_forlist.sexp test17_forfold.sexp 
# Last Line - added because they are failing under valgrind [2016.11.11]

# test 6i -- WRONG ANSWER, returning 101 !! [2016.11.09]
# test 6f -- seeing weird discrepanancies where sometimes a tag is not initialized [2016.11.09] 


# test11d_funrec.sexp -- segfaults
# 12b - segfaults

# NOTE:
# test11b_fundata.sexp -- map lookup failed on datacon
# test11d_funrec.sexp -- is giving the WRONG ANSWER
# test11f_funrec.sexp -- ''

PASSING_TESTS = $(filter-out $(FAILING_TESTS),$(ALL_TESTS))

OUT_FILES=$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).out)
EXE_FILES=$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).exe)

# Run the tests that work.  Assumes "build" has already executed.
run_tests: 
	@echo "\n Compiling and running tests through our compiler ($(EXTRA_SUFFIX)):"
	@echo "------------------------------------------------------------------"
	@echo "Running test set: "$(PASSING_TESTS)
	rm -f $(OUT_FILES)
	$(MAKE) all_tests

all_tests: answers $(OUT_FILES)

# Assumes stack build. Builds AND RUNS all the tests:
# Take care to not collide between different EXTRA_SUFFIX modes.
%$(EXTRA_SUFFIX).out : %.sexp
	$(TREEC) $(RUN_MODE) $(TREECARGS) --cfile=$(@:.out=.c) --exefile=$(@:.out=.exe) $^ > $@ || ./err.sh $(basename $^)$(EXTRA_SUFFIX)
	@./diff.sh $(basename $^).ans $@ || ./err.sh $(basename $^)$(EXTRA_SUFFIX)

# Just build the tests
build_tests: $(EXE_FILES)

%$(EXTRA_SUFFIX).exe : %.sexp
	$(TREEC) --exe $(TREECARGS) $^ --cfile=$(@:.exe=.c) --exefile=$@ || ./err.sh $(basename $^)$(EXTRA_SUFFIX)


PACKED_FAILING ?=  test12_skip.sexp  test16_forlist.sexp test17_forfold.sexp \
test20b_bintree.sexp test20f_bintree.sexp test24_defs.sexp test25b_racketcore.sexp 

# test20f - wip
# test20b_bintree.sexp   -- requires copy insertion
# test13_build.sexp -- problem with unit functions
# test12_skip.sexp       -- Requires copy insertion!! unbound var thru to C code
# test16_forlist.sexp    -- No lists yet
# test17_forfold.sexp    -- No lists yet

# This is the default mode:
run_tests_pointer: run_tests

# Run the tests that work under the packed regime:
run_tests_packed: answers
	$(MAKE) run_tests RUN_MODE="--run" TREECARGS="$(TREECARGS) --packed" FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd"


INTERP2_FAILING = test00b_printBool.sexp test06a_two_cases.sexp test06b_case.sexp test06_case.sexp test06c_nested.sexp \
     test06d_rec.sexp test06e_rec.sexp test06f_rec.sexp test06g_rec.sexp \
  test06h_rec.sexp test06i_casecase.sexp test11b_fundata.sexp test11c_funrec.sexp \
  test11d_funrec.sexp test11e_funrec.sexp test11f_funrec.sexp  \
  test12b_traverse.sexp test12c_traverse.sexp test12_skip.sexp \
  test16_forlist.sexp test17_forfold.sexp test20_bintree.sexp \
  text08_dict.sexp test08b_dict.sexp test08c_dict.sexp test08d_sharedict.sexp \
  test13_build.sexp test13b_build.sexp  \
  test20b_bintree.sexp test20c_bintree.sexp test20d_bintree.sexp test20e_bintree.sexp test20f_bintree.sexp \
  test25b_racketcore.sexp test25_rackcore.sexp \
  test07b_iterate.sexp test25b_racketcore.sexp \
  test11_fundata.sexp test10_desugar.sexp test10b_desugar.sexp test24_defs.sexp \
  test09_recur.sexp test08_dict.sexp test02b_datacon.sexp test02c_case.sexp 
# Last line are new regressions...

# Running through interpreter, no --packed/cursorize:
run_tests_interp2: answers
	$(MAKE) run_tests RUN_MODE="--interp2"  FAILING_TESTS="$(INTERP2_FAILING)" EXTRA_SUFFIX=".intrp2"


# Populated [2016.12.10]
INTERP1_FAILING = test16_forlist.sexp test17_forfold.sexp test25b_racketcore.sexp

run_tests_interp1: answers
	$(MAKE) run_tests RUN_MODE="--interp1"  FAILING_TESTS="$(INTERP1_FAILING)" EXTRA_SUFFIX=".intrp1"


# --------------------------------------------------------------------------------

%.ans : %.sexp
	raco make -v $^
	racket $^ > $@ || rm -f $@
#	racket $^ | tee $@

answers: $(ANSWERS)

# --------------------------------------------------------------------------------

valgrind_packed:
	$(MAKE) run_valgrind TREECARGS="--packed --optc=-g " FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd" 

valgrind:
	$(MAKE) run_valgrind TREECARGS=" --optc=-g " EXTRA_SUFFIX=".ptr" 

run_valgrind: $(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).valgrind)

%$(EXTRA_SUFFIX).valgrind : %.sexp %$(EXTRA_SUFFIX).exe
#	@echo "\n Valgrind, testing $(basename $(basename $@))"
#	@echo "--------------------------------------------------"
	valgrind -q --error-exitcode=1 ./$(basename $(basename $@))$(EXTRA_SUFFIX).exe
	@touch $@

# --------------------------------------------------------------------------------


treelang_lib:
	raco make -v ../../gibbon/main.rkt 

clean:
	rm -f *.exe *.o *.c  *.out *.err *.valgrind *.parsed *.intrp* test24_input.bin
	rm -rf ./compiled
	cd error/; rm -f *.exe *.o *.c  *.out *.err *.valgrind *.parsed

distclean: clean
	rm -f *.ans
	cd error/; rm -f *.ans

.PHONY: prebuild build run_racket treelang_lib answers run_tests core_tests
.PHONY: run_tests_interp2 run_tests_packed clean distclean
