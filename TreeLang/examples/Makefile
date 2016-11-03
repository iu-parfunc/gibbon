

default: valid

# Run the tests straight through Racket
valid: treelang_lib
	@echo "\nGenerating canonical answers from Racket:"
	@echo "-----------------------------------------"
	for f in `ls test*.sexp`; do racket $$f | tee $$f.ans; done

invalid: treelang_lib
	for f in `find ./error/ -name '*.sexp'`; do racket $$f | tee $$f.ans; done

test:
	$(MAKE) parse_all
	$(MAKE) run_tests

TREEC= stack exec -- treec

parse_all: 
	stack build
	for f in `ls *.sexp`; do \
          echo "Parsing $$f"; \
          $(TREEC) --parse $$f; done

PASSING_TESTS= test00_add.sexp

# let this be a proxy for the lot of them:
test00_add.sexp.ans:
	$(MAKE) valid

# Run the tests that work:
run_tests: test00_add.sexp.ans
	@echo "\n Compiling and running tests through our compiler:"
	@echo "--------------------------------------------------"
	stack build
	for f in $(PASSING_TESTS); do \
          $(TREEC) --run $$f > $$f.out; \
          echo "Now check expected $$f.ans, versus received, $$f.out"; \
          diff $$f.ans $$f.out || (echo "ERROR: Answers differed!"; exit 1); \
          echo "  -> Success."; \
        done

treelang_lib:
	raco make -v ../treelang.rkt 

clean:
	rm -f *.exe *.o *.c *.ans *.out
	cd error/; rm -f *.exe *.o *.c *.ans *.out

.PHONY: run_racket treelang_lib
