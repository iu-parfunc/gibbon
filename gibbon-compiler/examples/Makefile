
default:
	rm -f *.err $(BLD)/*.err
	$(MAKE) test

test:
	$(MAKE) smalltest
	@echo
	@echo "For extra assurance, running valgrind!"
	rm -f $(BLD)/*.valgrind
	$(MAKE) valgrind
	$(MAKE) valgrind_packed


ALL_TESTS= $(shell ls test*.sexp)

# Temporary output dir:
BLD=./build_tmp

ANSWERS= $(addprefix $(BLD)/,$(ALL_TESTS:.sexp=.ans))


# Number of processors:
# Here's a hacky way to try several things until one works.
PROCS = $(shell getconf _NPROCESSORS_ONLN || \
                nproc || \
                stack exec -- ghc -e GHC.Conc.getNumProcessors || \
                python -c 'import multiprocessing as m; print m.cpu_count()' )
# On Mac: "sysctl -n hw.ncpu" also "getconf _NPROCESSORS_ONLN" works
# On Windows?

GIBBON= DEBUG=1 stack exec -- gibbon

GIBBONEXEC=$(shell stack exec -- which gibbon)

smalltest: prebuild build
	$(MAKE) benchtest
# Having weird trouble with race conditons on answers:
	$(MAKE) run_answers
	wc -l $(ANSWERS)
	@echo
	@echo "Answers available, now proceeding..."
# [2017.01.11] Fairly obsolete:
#	$(MAKE) parse_all
	$(MAKE) core_tests

# Test the benchmarking mode
benchtest: test24_input.gpkd
	@echo "\n Testing Gibbon's benchmarking mode."
	@echo "--------------------------------------------------"
	$(GIBBON) --bench-fun pass --bench-print test24_defs.sexp
	@echo
	./test24_defs.exe --bench-input test24_input.gpkd  10 10
	@echo "Same benchmark, but in one command:"
	$(GIBBON) -b pass --bench-print --bench-input test24_input.gpkd  test24_defs.sexp
	@echo
	@echo "\nNow with the full Racket Core grammar:"
	$(GIBBON) --bench-fun foo --bench-print --bench-input test25b_input.gpkd test25b_racketcore.sexp
# Printing doesn't work yet:
	@echo "\nNow in packed mode:"
	$(GIBBON) --packed --bench-fun pass --bench-input test24_input.gpkd test24_defs.sexp
	$(GIBBON) --packed --bench-fun foo --bench-print --bench-input test25b_input.gpkd test25b_racketcore.sexp


BIG= $(TREELANGDIR)/ASTBenchmarks/cleaned_racket/share/pkgs/typed-racket-more/typed/private/framework-types.rkt.out.sexp
BIGPKD= $(TREELANGDIR)/ASTBenchmarks/binary_racket/share/pkgs/typed-racket-more/typed/private/framework-types.rkt.out.gpkd
PERF= perf stat -e cycles,instructions,cache-references,cache-misses,bus-cycles

big:
	ls -l $(BIG)
	ls -l $(BIGPKD)
	$(GIBBON) --pointer --bench-print --bench-fun countnodes -o test25f_pointer.exe --cfile=test25f_pointer.c test25f_countnodes.sexp
	$(GIBBON) --packed  --bench-print --bench-fun countnodes -o test25f_packed.exe  --cfile=test25f_packed.c  test25f_countnodes.sexp
	@echo "Pointer version"
	$(PERF) ./test25f_pointer.exe --bench-input $(BIGPKD) 0 1000
	@echo "Packed version"
	$(PERF) ./test25f_packed.exe --bench-input $(BIGPKD) 0 1000



# Assumes answers and build already available:
core_tests:
	$(MAKE) run_tests 
	$(MAKE) run_tests_packed
	$(MAKE) run_tests_interp1
	$(MAKE) run_tests_interp2

#	$(MAKE) racket_valid
# Need methodology for this.
#	$(MAKE) racket_invalid

# Set to the name of a single test you'd like to test:
ONE=test00_add.sexp
one:
	$(MAKE) $(BLD)/$(ONE:.sexp=.ans)
	$(MAKE) $(BLD)/$(ONE:.sexp=.intrp1.out)   EXTRA_SUFFIX=".intrp1"
	$(MAKE)                                   EXTRA_SUFFIX=".ptr" $(BLD)/$(ONE:.sexp=.ptr.out)
	$(MAKE) FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd" $(BLD)/$(ONE:.sexp=.pkd.out)
#	$(MAKE) $(BLD)/$(ONE).interp2.out


EXTRA_SUFFIX ?= .ptr
# How far to compile, --toC, --exe, --run:
RUN_MODE     ?= --run
# Additional args besides RUN_MODE:
GIBBONARGS ?= --optc=-g

SANDBOXDIR= $(TREELANGDIR)/.racket_sandbox/
RUNRACKET= racket -A $(SANDBOXDIR)

# Not necessary, because we build each module before we run it:
prebuild: $(BLD) test24_input.gpkd
# [2017.01.12] It's probably good enough to build ONE test, which builds upstream stuff:
	PLTADDONDIR=$(SANDBOXDIR) raco make -j $(PROCS) -v test00_add.sexp

$(BLD):
	mkdir -p $(BLD)

test24_input.gpkd: test24_input.dat test24_packit.rkt test24_defs.sexp
	$(RUNRACKET) test24_packit.rkt

#----------------------------------------
# Test direct Racket execution:
#----------------------------------------
racket_valid: treelang_lib
	@echo "\nGenerating canonical answers from Racket:"
	@echo "-----------------------------------------"
	for f in `ls test*.sexp`; do $(RUNRACKET) $$f | tee $$f.ans; done

racket_invalid: treelang_lib
	for f in `find ./error/ -name '*.sexp'`; do $(RUNRACKET) $$f | tee $$f.ans; done

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
#	@echo "Parsing $<"
	$(GIBBON) --parse $< 2> $@

# Failing tests for the pointer-based C backend (default):
FAILING_TESTS ?= \
  test13_build.sexp test13b_build.sexp \
  test11_fundata.sexp test11c_funrec.sexp test11e_funrec.sexp test12c_traverse.sexp \
  test25d_racketcore.sexp \
  test16_forlist.sexp test17_forfold.sexp \
  test25a_withprint.sexp

# test25a "works" but it still prints symbols as numbers.

# NOTE:
# test11d_funrec.sexp -- is giving the WRONG ANSWER
# test11f_funrec.sexp -- ''

PASSING_TESTS = $(filter-out $(FAILING_TESTS),$(ALL_TESTS))

OUT_FILES=$(addprefix $(BLD)/,$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).out))
EXE_FILES=$(addprefix $(BLD)/,$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).exe))

# Run the tests that work.  Assumes "build" has already executed.
run_tests: $(BLD)
	@echo "\n Compiling and running tests through our compiler ($(EXTRA_SUFFIX)):"
	@echo "------------------------------------------------------------------"
	@echo "Running test set: "$(PASSING_TESTS)
#	@echo "\nFirst clear output files:"
#	rm -f $(OUT_FILES) 
	@echo ""
	$(MAKE) all_tests

all_tests: $(BLD) answers $(OUT_FILES)

# Assumes stack build. Builds AND RUNS all the tests:
# Take care to not collide between different EXTRA_SUFFIX modes.
#
# Depend on GIBBONEXEC so we retest when the binary changes.
build_tmp/%$(EXTRA_SUFFIX).out : %.sexp $(GIBBONEXEC)
ifeq ($(RUN_MODE),--run)
	@$(MAKE) --no-print-directory $(@:.out=.exe)
	./$(@:.out=.exe) > $@ || ./err.sh $(@:.out=)
else
	$(GIBBON) $(RUN_MODE) $(GIBBONARGS) --cfile=$(@:.out=.c) --exefile=$(@:.out=.exe) $< > $@ || ./err.sh $(@:.out=)
endif
	@./diff.sh $(BLD)/$(basename $<).ans $@ || ./err.sh $(@:.out=)

# Just build the tests
build_tests: $(EXE_FILES)

build_tmp/%$(EXTRA_SUFFIX).exe : %.sexp $(GIBBONEXEC)
	$(GIBBON) --exe $(GIBBONARGS) $< --cfile=$(@:.exe=.c) --exefile=$@ || ./err.sh $(BLD)/$(basename $<)$(EXTRA_SUFFIX)


PACKED_FAILING ?=  test12_skip.sexp  test16_forlist.sexp test17_forfold.sexp \
 test20b_bintree.sexp  \
 test25a_withprint.sexp  test27a_subst.sexp

# test20b_bintree.sexp   -- requires copy insertion
# test12_skip.sexp       -- requires copy insertion!! unbound var thru to C code
# test16_forlist.sexp    -- requires lists. No lists yet
# test17_forfold.sexp    -- requires lists. No lists yet

# This is the default mode:
run_tests_pointer: run_tests

# Run the tests that work under the packed regime:
run_tests_packed: answers
	$(MAKE) run_tests RUN_MODE="--run" GIBBONARGS="$(GIBBONARGS) --packed" FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd"

# This one is seriously rotted:
INTERP2_FAILING = test00a_size.sexp test00b_printBool.sexp test00d_printData.sexp \
  test02d_printPair.sexp \
  test06a_two_cases.sexp test06b_case.sexp test06_case.sexp test06c_nested.sexp \
  test06d_rec.sexp test06e_rec.sexp test06f_rec.sexp test06g_rec.sexp \
  test06h_rec.sexp test06i_casecase.sexp test07b_iterate.sexp test11b_fundata.sexp test11c_funrec.sexp \
  test11d_funrec.sexp test11e_funrec.sexp test11f_funrec.sexp  \
  test12b_traverse.sexp test12c_traverse.sexp test12_skip.sexp \
  test16_forlist.sexp test17_forfold.sexp test20_bintree.sexp \
  text08_dict.sexp test08b_dict.sexp test08c_dict.sexp test08d_sharedict.sexp \
  test13_build.sexp test13b_build.sexp  \
  test20a_withprint.sexp test20b_bintree.sexp test20c_bintree.sexp test20d_bintree.sexp test20e_bintree.sexp \
  test20f_bintree.sexp test20g_bintree.sexp \
  test25a_withprint.sexp test25b_racketcore.sexp test25_rackcore.sexp \
  test25c_racketcore.sexp test25d_racketcore.sexp test25e_countnodes.sexp test25f_countnodes.sexp \
  test11_fundata.sexp test10_desugar.sexp test10b_desugar.sexp test24_defs.sexp \
  test09_recur.sexp test08_dict.sexp test02b_datacon.sexp test02c_case.sexp \
  test26a_treewalk.sexp test27a_subst.sexp

# Running through interpreter, no --packed/cursorize:
run_tests_interp2: answers
	$(MAKE) run_tests RUN_MODE="--interp2"  FAILING_TESTS="$(INTERP2_FAILING)" EXTRA_SUFFIX=".intrp2"


# Populated [2016.12.10]
INTERP1_FAILING = test16_forlist.sexp test17_forfold.sexp test25b_racketcore.sexp \
  test25a_withprint.sexp


run_tests_interp1: answers
	$(MAKE) run_tests RUN_MODE="--interp1"  FAILING_TESTS="$(INTERP1_FAILING)" EXTRA_SUFFIX=".intrp1"


# --------------------------------------------------------------------------------

build_tmp/%.ans : %.sexp
# Optional, could prebuild before running:
#	raco make -v $<
	$(RUNRACKET) $< > $@ || rm -f $@
#	$(RUNRACKET) $< | tee $@

run_answers:
	@echo "\nGenerating canonical answers from Racket:"
	@echo "-----------------------------------------"
	$(MAKE) answers

answers: $(ANSWERS)


# Valgrind should be part of our regression testing:
# --------------------------------------------------------------------------------

# Not assuming Valgrind for OSX atm:
OS := $(shell uname)

ifeq ($(OS),Darwin)
 VALGRIND_CMD := @echo "Not running valgrind on this: "
else
 VALGRIND_CMD := valgrind -q --error-exitcode=99
endif

valgrind_packed:
	$(MAKE) run_valgrind GIBBONARGS="--packed --optc=-g " FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd"; \

# if test `uname` == "Linux" ; then \
        #   $(MAKE) run_valgrind GIBBONARGS="--packed --optc=-g " FAILING_TESTS="$(PACKED_FAILING)" EXTRA_SUFFIX=".pkd"; \
        # else echo "Only supporting valgrind on linux."; fi

valgrind:
	$(MAKE) run_valgrind GIBBONARGS=" --optc=-g " EXTRA_SUFFIX=".ptr" 


VALGRIND_FILES=$(addprefix $(BLD)/,$(PASSING_TESTS:.sexp=$(EXTRA_SUFFIX).valgrind))

run_valgrind: $(VALGRIND_FILES)

build_tmp/%$(EXTRA_SUFFIX).valgrind : %.sexp build_tmp/%$(EXTRA_SUFFIX).exe
#	@echo "\n Valgrind, testing $(basename $(basename $@))"
#	@echo "--------------------------------------------------"
	$(VALGRIND_CMD) $(basename $@).exe
	@touch $@

# --------------------------------------------------------------------------------


treelang_lib:
	raco make -v ../../gibbon/main.rkt 

RMINTERMEDIATE = rm -rf *.exe *.o *.c  *.out *.err *.valgrind *.parsed *.intrp* *.exe.dSYM

clean:
	$(RMINTERMEDIATE)
	(cd $(BLD); $(RMINTERMEDIATE) )
	rm -rf test24_input.gpkd ./compiled 
	(cd error/; rm -f *.exe *.o *.c  *.out *.err *.valgrind *.parsed)

distclean: clean
	rm -f *.ans
	cd error/; rm -f *.ans

.PHONY: prebuild build run_racket treelang_lib answers run_tests core_tests big
.PHONY: run_tests_interp2 run_tests_packed clean distclean benchtest
