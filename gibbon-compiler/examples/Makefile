
default:
	rm -f *.err
	$(MAKE) test

ALL_TESTS= $(shell ls test*.sexp)

# Temporary output dir:
BLD=./build_tmp/

ANSWERS= $(addprefix $(BLD),$(ALL_TESTS:.sexp=.ans))


# Number of processors:
# Here's a hacky way to try several things until one works.
PROCS = $(shell nproc || \
          getconf _NPROCESSORS_ONLN || \
          stack exec -- ghc -e GHC.Conc.getNumProcessors || \
          python -c 'import multiprocessing as m; print m.cpu_count()' )
# On Mac: "sysctl -n hw.ncpu" also "getconf _NPROCESSORS_ONLN" works
# On Windows?

GIBBON= DEBUG=1 stack exec -- gibbon


test: prebuild build
	$(MAKE) benchtest
# Having weird trouble with race conditons on answers:
	$(MAKE) run_answers
	wc -l $(ANSWERS)
	@echo "Answers available, now proceeding..."
	$(MAKE) parse_all
	$(MAKE) core_tests

# Test the benchmarking mode
benchtest: test24_input.bin
	@echo "\n Testing Gibbon's benchmarking mode."
	@echo "--------------------------------------------------"
	$(GIBBON) --bench pass test24_defs.sexp
	@echo
	./test24_defs.exe --bench 10 10 --bench-input test24_input.bin 
	$(GIBBON) -b pass --bench-input test24_input.bin  test24_defs.sexp
	@echo
# Next is this:
#	@echo "\nNow in packed mode:"
#	$(GIBBON) --packed --bench pass --bench-input test24_input.bin test24_defs.sexp

	@echo "\nNow with the full Racket Core grammar:"
	$(GIBBON) --bench foo --bench-input test25b_input.bin test25b_racketcore.sexp


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
GIBBONARGS ?= --optc=-g


# Not necessary, because we build each module before we run it:
prebuild: $(BLD) test24_input.bin
	raco make -j $(PROCS) -v $(ALL_TESTS) 

$(BLD):
	mkdir -p $(BLD)

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
	$(GIBBON) --parse $^ 2> $@

# Failing tests for the pointer-based C backend (default):
FAILING_TESTS ?= \
  test13_build.sexp test13b_build.sexp \
  test20_bintree.sexp test20b_bintree.sexp test20c_bintree.sexp  test20d_bintree.sexp test20e_bintree.sexp test20f_bintree.sexp \
  test11_fundata.sexp test11c_funrec.sexp test11e_funrec.sexp test12c_traverse.sexp \
  test25d_racketcore.sexp \
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

OUT_FILES=$(addprefix $(BLD),$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).out))
EXE_FILES=$(addprefix $(BLD),$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).exe))

# Run the tests that work.  Assumes "build" has already executed.
run_tests: $(BLD)
	@echo "\n Compiling and running tests through our compiler ($(EXTRA_SUFFIX)):"
	@echo "------------------------------------------------------------------"
	@echo "Running test set: "$(PASSING_TESTS)
	@echo "\nFirst clear output files:"
	rm -f $(OUT_FILES)
	@echo ""
	$(MAKE) all_tests

all_tests: $(BLD) answers $(OUT_FILES)

# Assumes stack build. Builds AND RUNS all the tests:
# Take care to not collide between different EXTRA_SUFFIX modes.
build_tmp/%$(EXTRA_SUFFIX).out : %.sexp
	$(GIBBON) $(RUN_MODE) $(GIBBONARGS) --cfile=$(@:.out=.c) --exefile=$(@:.out=.exe) $^ > $@ || ./err.sh $(BLD)/$(basename $^)$(EXTRA_SUFFIX)
	@./diff.sh $(BLD)/$(basename $^).ans $@ || ./err.sh $(BLD)/$(basename $^)$(EXTRA_SUFFIX)

# Just build the tests
build_tests: $(EXE_FILES)

%$(EXTRA_SUFFIX).exe : %.sexp
	$(GIBBON) --exe $(GIBBONARGS) $^ --cfile=$(@:.exe=.c) --exefile=$@ || ./err.sh $(BLD)/$(basename $^)$(EXTRA_SUFFIX)


PACKED_FAILING ?=  test12_skip.sexp  test16_forlist.sexp test17_forfold.sexp \
test20b_bintree.sexp test20f_bintree.sexp test24_defs.sexp test25e_countnodes.sexp

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
	$(MAKE) run_tests RUN_MODE="--run" GIBBONARGS="$(GIBBONARGS) --packed" FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd"


INTERP2_FAILING = test00b_printBool.sexp test06a_two_cases.sexp test06b_case.sexp test06_case.sexp test06c_nested.sexp \
     test06d_rec.sexp test06e_rec.sexp test06f_rec.sexp test06g_rec.sexp \
  test06h_rec.sexp test06i_casecase.sexp test07b_iterate.sexp test11b_fundata.sexp test11c_funrec.sexp \
  test11d_funrec.sexp test11e_funrec.sexp test11f_funrec.sexp  \
  test12b_traverse.sexp test12c_traverse.sexp test12_skip.sexp \
  test16_forlist.sexp test17_forfold.sexp test20_bintree.sexp \
  text08_dict.sexp test08b_dict.sexp test08c_dict.sexp test08d_sharedict.sexp \
  test13_build.sexp test13b_build.sexp  \
  test20b_bintree.sexp test20c_bintree.sexp test20d_bintree.sexp test20e_bintree.sexp test20f_bintree.sexp \
  test25b_racketcore.sexp test25_rackcore.sexp \
  test25c_racketcore.sexp test25d_racketcore.sexp test25e_countnodes.sexp \
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

build_tmp/%.ans : %.sexp
# Optional, could prebuild before running:
#	raco make -v $^
	racket $^ > $@ || rm -f $@
#	racket $^ | tee $@

run_answers:
	@echo "\nGenerating canonical answers from Racket:"
	@echo "-----------------------------------------"
	$(MAKE) answers

answers: $(ANSWERS)


# Valgrind should be part of our regression testing:
# --------------------------------------------------------------------------------

valgrind_packed:
	$(MAKE) run_valgrind GIBBONARGS="--packed --optc=-g " FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd" 

valgrind:
	$(MAKE) run_valgrind GIBBONARGS=" --optc=-g " EXTRA_SUFFIX=".ptr" 

VALGRIND_FILES=$(addprefix $(BLD),$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).valgrind))

run_valgrind: $(VALGRIND_FILES)

%$(EXTRA_SUFFIX).valgrind : %.sexp %$(EXTRA_SUFFIX).exe
#	@echo "\n Valgrind, testing $(basename $(basename $@))"
#	@echo "--------------------------------------------------"
	valgrind -q --error-exitcode=1 $(BLD)/$(basename $(basename $@))$(EXTRA_SUFFIX).exe
	@touch $@

# --------------------------------------------------------------------------------


treelang_lib:
	raco make -v ../../gibbon/main.rkt 

clean:
	(cd $(BLD); rm -f *.exe *.o *.c  *.out *.err *.valgrind *.parsed *.intrp* )
	rm -rf test24_input.bin ./compiled 
	(cd error/; rm -f *.exe *.o *.c  *.out *.err *.valgrind *.parsed)

distclean: clean
	rm -f *.ans
	cd error/; rm -f *.ans

.PHONY: prebuild build run_racket treelang_lib answers run_tests core_tests
.PHONY: run_tests_interp2 run_tests_packed clean distclean benchtest
