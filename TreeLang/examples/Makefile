
default: test

test:
	$(MAKE) parse_all
	$(MAKE) run_tests
	$(MAKE) run_tests_packed
	$(MAKE) run_tests_interp2

# Run the tests straight through Racket
valid: treelang_lib
	@echo "\nGenerating canonical answers from Racket:"
	@echo "-----------------------------------------"
	for f in `ls test*.sexp`; do racket $$f | tee $$f.ans; done

invalid: treelang_lib
	for f in `find ./error/ -name '*.sexp'`; do racket $$f | tee $$f.ans; done

TREEC= stack exec -- treec

parse_all: 
	stack build
	for f in `ls *.sexp`; do \
          echo "Parsing $$f"; \
          $(TREEC) --parse $$f || exit 1; done

PASSING_TESTS ?= test00_add.sexp test01_if.sexp  test02_arith.sexp \
    test03b_eqs.sexp test03c_eqsym.sexp \
    test07_time.sexp test09_recur.sexp
# FIX THESE: All are blocked on something.  Then move them to the above list:
#  test03  -- flatten fromJust

#  test04  -- MkTrue typeExp
#  test04b -- ''
#  test04c -- lower needs detuple
#  test05  -- lower/typ'
#  test05b -- ''
#  test06  -- lower datacon

#  test08  -- parsing empty-dict
#  test08b -- ''
#  test08c -- ''

#  test10  -- codegenTail line 235
#  test15  -- lower/typ'


# let this be a proxy for the lot of them:
test00_add.sexp.ans:
	$(MAKE) valid

TREECARGS ?= --run

# Run the tests that work:
run_tests: test00_add.sexp.ans
	@echo "\n Compiling and running tests through our compiler:"
	@echo "--------------------------------------------------"
	stack build
	for f in $(PASSING_TESTS); do \
          echo "Testing $$f"; \
          $(TREEC) $(TREECARGS) $$f > $$f.out; \
          echo "Now check expected $$f.ans, versus received, $$f.out"; \
          ./diff.sh $$f.ans $$f.out; \
          if [ $$? != "0" ]; then echo "ERROR: Answers differed!"; exit 1; fi; \
          echo "  -> Success."; \
        done


PACKED_TESTS= test00_add.sexp test02_arith.sexp test03b_eqs.sexp test03c_eqsym.sexp
#  test04b -- cursorize/mkprod
#  test05  -- cursorize tail Apps
#  test05b -- cursorize/let
#  test06  -- cursorize/let
#  test07  -- cursorize/TimeIt
#  test08  -- no dictionaries yet
#  test09  -- fail on Compile.hs nonexhaustive

# Run the tests that work under the packed regime:
run_tests_packed: test00_add.sexp.ans
	$(MAKE) run_tests TREECARGS="--run --packed" PASSING_TESTS=$(PACKED_TESTS)


INTERP_TESTS= test00_add.sexp test01_if.sexp test02_arith.sexp \
     test03b_eqs.sexp test03c_eqsym.sexp \
     test05_app.sexp  \
     test05b_app.sexp  test07_time.sexp test09_recur.sexp test10_desugar.sexp

# Running through interpreter, no --packed/cursorize:
run_tests_interp2: test00_add.sexp.ans
	$(MAKE) run_tests TREECARGS="--interp2" PASSING_TESTS="$(INTERP_TESTS)"
# test03_let.sexp       -- flatten fromJust
# test04_vector.sexp    -- MkTrue in flatten
# test04b_vector.sexp   -- ''
# test04c_ifvector.sexp -- lower needs detuple
# test06_case.sexp      -- let/datacon not allowed thru lower yet

# test15_packed.sexp    -- no lowering of packed types yet
# test16_forlist.sexp   -- no list support yet
# test17_forfold.sexp   -- ''
# bintree.sexp          -- flatten fromJust


treelang_lib:
	raco make -v ../treelang.rkt 

clean:
	rm -f *.exe *.o *.c *.ans *.out
	cd error/; rm -f *.exe *.o *.c *.ans *.out

.PHONY: run_racket treelang_lib
