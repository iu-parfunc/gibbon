
# Building
# ==============================================================================

all: treebench_mlton.exe treebench_ocaml.exe \
     treebench_rust.exe treebench.class c ghc

c: treebench_c.exe treebench_c_bumpalloc.exe treebench_c_packed.exe treebench_c_packed2.exe treebench_c_packed_loop.exe treebench_c_packed_structs.exe treebench_c_parallel.exe

ghc: treebench_ghc_strict.exe treebench_ghc_lazy.exe treebench_ghc_packed.exe


# Disabling for now, requires beta channel of rust:
# treebench_rust_sys_alloc.exe 

# Unfinished:
# treebench_rust_packed.exe

# Match whichever version the docker image is using:
GHC = stack --install-ghc --resolver=lts-7.1 exec ghc --
# GHC = stack --install-ghc --resolver=lts-6.19 exec ghc --

# CC = gcc
# CC = clang

# CPP = icpc
CPP = g++
# CPP = clang++

PACKEDTRIALS=501

CPPOPTS = -O3 -std=c++11
COPTS = -O3 -std=c11
# if debug:
# COPTS = -g -O0 -std=c++11


ghc: treebench_ghc_strict.exe treebench_ghc_lazy.exe

treebench_mlton.exe: treebench.sml
	time mlton -output $@ $^

treebench_ghc_strict.exe: treebench.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_ghc_lazy.exe: treebench_lazy.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_ghc_packed.exe: treebench_packed.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_ocaml.exe: treebench.ml
	time ocamlopt.opt $^ -o $@

treebench_rust.exe: treebench.rs
	time rustc $^ -o $@ -O

treebench_rust_sys_alloc.exe: treebench_sys_alloc.rs
	time rustc $^ -o $@ -O

# treebench_rust_packed.exe: treebench_packed.rs
# 	time rustc $^ -o $@ -O

treebench_c.exe: treebench.c
	time $(CC) $(COPTS) $^ -o $@ 

treebench_c_bumpalloc.exe: treebench.c
	time $(CC) $(COPTS) -DBUMPALLOC $^ -o $@ 


treebench_c_packed.exe: treebench_packed.c
	time $(CC) $(COPTS) -DTRIALS=$(PACKEDTRIALS) $^ -o $@ 

treebench_c_packed_loop.exe: treebench_packed_loop.c
	time $(CC) $(COPTS) -DTRIALS=$(PACKEDTRIALS) $^ -o $@ 

treebench_c_packed_structs.exe: treebench_packed_structs.c
	time $(CC) $(COPTS) $^ -o $@ 

treebench_c_packed2.exe: treebench_packed2.c
	time $(CC) $(COPTS) $^ -o $@ 

treebench_c_parallel.exe: treebench_parallel.c
	time $(CPP) -fpermissive -fcilkplus $(CPPOPTS) $^ -o $@ 

treebench.class: treebench.java
	time javac $^ 

# Running
# ==============================================================================

DEPTH=20

run_small:
	$(MAKE) DEPTH=5 run_all

# TODO: replace with an hsbencher harness / Criterion:
run_all: all
	./treebench_mlton.exe       $(DEPTH)
	./treebench_ghc_strict.exe  $(DEPTH)
	./treebench_ghc_lazy.exe    $(DEPTH)
	./treebench_ocaml.exe       $(DEPTH)
	./treebench_rust.exe        $(DEPTH)
	./treebench_c.exe           $(DEPTH)
	./treebench_c_packed.exe    $(DEPTH)
	./treebench_c_packed_loop.exe $(DEPTH)
	$(MAKE) run_racket
	$(MAKE) run_racket_packed
	$(MAKE) run_chez
	$(MAKE) run_java

#	./treebench_rust_sys_alloc.exe $(DEPTH) 

run_chez:
	scheme --script treebench.ss $(DEPTH)

run_racket:
	racket treebench.rkt $(DEPTH)

run_racket_packed:
	racket treebench_packed.rkt $(DEPTH)

run_java: treebench.class
	java treebench $(DEPTH)

clean:
	rm -f *.exe *.o *.hi treebench treebench_lazy *.cmi *.cmo *.cmx

.PHONY: all clean ghc run_chez run_java run_all run_small c ghc

