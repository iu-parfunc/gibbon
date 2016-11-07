
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

PASSING_TESTS ?= test00_add.sexp test01_if.sexp test01b_ifif.sexp test02_arith.sexp \
    test03_let.sexp test03b_eqs.sexp test03c_eqsym.sexp \
    test04_vector.sexp test04b_vector.sexp test05_app.sexp \
    test05b_app.sexp test06_case.sexp test06b_case.sexp test06c_nested.sexp \
    test06d_rec.sexp test06e_rec.sexp \
    test07_time.sexp test09_recur.sexp test10_desugar.sexp test10b_desugar.sexp \
    test11_fundata.sexp test11b_fundata.sexp test12_skip.sexp \
    test15_packed.sexp
# FIX THESE: All are blocked on something.  Then move them to the above list:.
# ----------------------
# test06h_rec.sexp -- lower hits let with case in RHS [2016.11.06]
# test06i_casecase.sexp -- ''

# test04c_ifvector.sexp -- MkProd sneaking through in Triv context to Lower [2016.11.06]



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
	$(TREEC) $(TREECARGS) $^ > $@ || touch $^.err
#	@echo "Now check expected $$f.ans, versus received, $$f.out";
	@./diff.sh $^.ans $@

# PACKED_TESTS=$(ALL_TESTS)

# Temporary, mostly broken state:
PACKED_TESTS= test11b_fundata.sexp
# PACKED_TESTS= test00_add.sexp test02_arith.sexp test01_if.sexp test01b_ifif.sexp \
    test03b_eqs.sexp test03c_eqsym.sexp test04_vector.sexp test04b_vector.sexp \
    test05_app.sexp test05b_app.sexp \
    test06_case.sexp test06b_case.sexp test06c_nested.sexp test06i_casecase.sexp \
    test06d_rec.sexp test06e_rec.sexp test06f_rec.sexp test06g_rec.sexp test06h_rec.sexp \
    test07_time.sexp test10_desugar.sexp test10b_desugar.sexp test11b_fundata.sexp test11c_funrec.sexp

# test04c_ifvector.sexp -- MkProd sneaking through in Triv context to Lower [2016.11.06]

# test09_recur.sexp -- hits hole in Cursorize [2016.11.06]

# test12_skip.sexp 

# Run the tests that work under the packed regime:
run_tests_packed: answers
	$(MAKE) run_tests TREECARGS="--run --packed" PASSING_TESTS="$(PACKED_TESTS)"


INTERP_TESTS= test00_add.sexp test01_if.sexp test01b_ifif.sexp test02_arith.sexp \
     test03b_eqs.sexp test03c_eqsym.sexp \
     test05_app.sexp  test05b_app.sexp \
     test07_time.sexp test09_recur.sexp test10_desugar.sexp test10b_desugar.sexp

# Running through interpreter, no --packed/cursorize:
run_tests_interp2: test00_add.sexp.ans
	$(MAKE) run_tests TREECARGS="--interp2" PASSING_TESTS="$(INTERP_TESTS)"
# test03_let.sexp       -- flatten fromJust
# test04_vector.sexp    -- MkTrue in flatten
# test04b_vector.sexp   -- ''
# test04c_ifvector.sexp -- lower needs detuple
# test06_case.sexp      -- unhandled interp case
# test06b_case.sexp      -- 

# test06c_nested.sexp    -- no datacon alloc yet.

# test15_packed.sexp    -- no lowering of packed types yet
# test16_forlist.sexp   -- no list support yet
# test17_forfold.sexp   -- ''
# bintree.sexp          -- flatten fromJust

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
	rm -f *.exe *.o *.c *.ans *.out
	cd error/; rm -f *.exe *.o *.c *.ans *.out

.PHONY: run_racket treelang_lib answers run_tests run_tests_interp2 run_tests_packed
