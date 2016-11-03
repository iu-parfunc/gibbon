

# This is completely optional, it just precompiles and typechecks:
racket_build:
	raco make -v *.rkt

# This just shows you how to run it:
bench:
	time ./run_and_scale_all_benchmarks.rkt ../../cleaned_racket

# This checks that everything parses/subsittutes
check:
	time ./run_and_scale_all_benchmarks.rkt -t 0 ../../cleaned_racket

# An example of how to run just one:
example:
	./run_one_benchmark.rkt configure ../../cleaned_racket/share/pkgs/datalog/pretty.rkt.out.sexp 10
