
default: test

test: answers parse_all
# Sequentialize these, they stomp on *.out:
	$(MAKE) run_tests 
	$(MAKE) run_tests_packed
	$(MAKE) run_tests_interp2

#	$(MAKE) racket_valid
# Need methodology for this.
#	$(MAKE) racket_invalid


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

TREEC= stack exec -- treec

# Make sure the compiler is built
build:
	stack build

parse_all: build
	@echo "\n Parsing all tests, even those that don't run yet."
	@echo "--------------------------------------------------"
	rm -f *.parsed
	$(MAKE) all_parsed

ALL_TESTS= $(shell ls test*.sexp)

all_parsed: $(addsuffix .parsed,$(ALL_TESTS))

%.sexp.parsed : %.sexp
#	@echo "Parsing $^"
	$(TREEC) --parse $^ 2> $@

# Failing tests for the pointer-based C backend (default):
FAILING_TESTS ?= \
  test17_forfold.sexp test06g_rec.sexp  test06i_casecase.sexp \
  test16_forlist.sexp
# NOTE:
# test11d_funrec.sexp -- is giving the WRONG ANSWER
# test11f_funrec.sexp -- ''

PASSING_TESTS = $(filter-out $(FAILING_TESTS),$(ALL_TESTS))

TREECARGS ?= --run

# Run the tests that work:
run_tests: build
	@echo "\n Compiling and running tests through our compiler:"
	@echo "--------------------------------------------------"
	rm -f *.out
	$(MAKE) all_tests

all_tests: answers $(addsuffix .out,$(PASSING_TESTS))

# Assumes stack build:
%.sexp.out : %.sexp
#	@echo "Testing $^"
	$(TREEC) $(TREECARGS) $^ > $@ || (touch $^.err; rm -f $@)
#	@echo "Now check expected $$f.ans, versus received, $$f.out";
	@./diff.sh $^.ans $@ || (touch $^.err; rm -f $@)

PACKED_FAILING ?= test11c_funrec.sexp test11d_funrec.sexp test11e_funrec.sexp test11f_funrec.sexp \
    test12b_traverse.sexp test12c_traverse.sexp \
    test12_skip.sexp test16_forlist.sexp test17_forfold.sexp test20_bintree.sexp

# test11c_funrec.sexp    -- findwitnesses
# test11d_funrec.sexp    -- findwitnesses
# test12_skip.sexp       -- Requires copy insertion!! unbound var thru to C code
# test12b_traverse.sexp  -- findWitnesses
# test12c_traverse.sexp  -- findWitnesses
# test16_forlist.sexp    -- No lists yet
# test17_forfold.sexp    -- No lists yet
# test20_bintree.sexp    -- 


# Run the tests that work under the packed regime:
run_tests_packed: answers
	$(MAKE) run_tests TREECARGS="--run --packed" FAILING_TESTS="$(PACKED_FAILING)"


INTERP_FAILING = test06b_case.sexp test06_case.sexp test06c_nested.sexp \
     test06d_rec.sexp test06e_rec.sexp test06f_rec.sexp test06g_rec.sexp \
  test06h_rec.sexp test06i_casecase.sexp test11b_fundata.sexp test11c_funrec.sexp \
  test11d_funrec.sexp test11e_funrec.sexp test11f_funrec.sexp  \
  test12b_traverse.sexp test12c_traverse.sexp test12_skip.sexp \
  test16_forlist.sexp test17_forfold.sexp test20_bintree.sexp


# Running through interpreter, no --packed/cursorize:
run_tests_interp2: test00_add.sexp.ans
	$(MAKE) run_tests TREECARGS="--interp2" FAILING_TESTS="$(INTERP_FAILING)"


# --------------------------------------------------------------------------------

ANSWERS= $(addsuffix .ans,$(INTERP_TESTS)) $(addsuffix .ans,$(PASSING_TESTS)) $(addsuffix .ans,$(PACKED_TESTS))

%.sexp.ans : %.sexp
	racket $^ | tee $@

answers: $(ANSWERS)

# --------------------------------------------------------------------------------

valgrind: $(addsuffix .valgrind,$(PASSING_TESTS))

%.sexp.valgrind : %.sexp %.sexp.out
	valgrind ./$(basename $(basename $@)).exe

# --------------------------------------------------------------------------------


treelang_lib:
	raco make -v ../treelang.rkt 

clean:
	rm -f *.exe *.o *.c *.ans *.out *.err
	cd error/; rm -f *.exe *.o *.c *.ans *.out *.err

.PHONY: run_racket treelang_lib answers run_tests run_tests_interp2 run_tests_packed
