

default: valid

# Run the tests straight through Racket
valid: treelang_lib
	for f in `ls test*.sexp`; do racket $$f | tee $$f.ans; done

invalid: treelang_lib
	for f in `find ./error/ -name '*.sexp'`; do racket $$f | tee $$f.ans; done



treelang_lib:
	raco make -v ../treelang.rkt 

.PHONY: run_racket treelang_lib
