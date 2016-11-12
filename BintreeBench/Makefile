
# Building
# ==============================================================================

all: treebench_mlton.exe treebench_ocaml.exe \
     treebench_rust.exe treebench.class c ghc

c: treebench_c.exe treebench_c_bumpalloc.exe treebench_c_bumpalloc_unaligned.exe treebench_c_parallel.exe \
   treebench_c_packed.exe treebench_c_packed_loop.exe treebench_c_packed_structs.exe treebench_c_packed_parallel3.exe

# These are unfinished, or behaving badly:
# treebench_c_packed_parallel.exe 
# treebench_c_packed_parallel2.exe

ghc: treebench_ghc_strict.exe treebench_ghc_lazy.exe treebench_ghc_packed.exe


# Disabling for now, requires beta channel of rust:
# treebench_rust_sys_alloc.exe 

# Unfinished:
# treebench_rust_packed.exe

# Match whichever version the docker image is using:
RESOLVER=lts-6.23
# RESOLVER=lts-7.1

GHC = stack --install-ghc --resolver=$(RESOLVER) exec ghc -- -rtsopts -threaded

CC = gcc
# CC = clang

# CPP = icpc
CPP = g++
# CPP = clang++

# time.h is missing features in c11/c++11:
CPPOPTS = -std=gnu++11 -lrt
COPTS   = -std=gnu11   -lrt

ifeq ($(DEBUG),)
  CPPOPTS += -O3 
  COPTS += -O3 
else
  CPPOPTS += -O0 -g 
  COPTS += -O0 -g
endif


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

treebench_c_parallel.exe: treebench.c
	time $(CC) -DPARALLEL -fcilkplus $(COPTS) $^ -o $@ 

treebench_c_bumpalloc.exe: treebench.c
	time $(CC) $(COPTS) -DBUMPALLOC $^ -o $@ 

# this version uses 1 byte for tags
treebench_c_bumpalloc_unaligned.exe: treebench.c
	time $(CC) $(COPTS) -DBUMPALLOC -DUNALIGNED $^ -o $@ 

HEADERS=include/main_fragment_packed.h

treebench_c_packed.exe: treebench_packed.c $(HEADERS)
	time $(CC) $(COPTS) treebench_packed.c -o $@ 

treebench_c_packed_loop.exe: treebench_packed_loop.c $(HEADERS)
	time $(CC) $(COPTS) treebench_packed_loop.c -o $@ 

treebench_c_packed_structs.exe: treebench_packed_structs.c $(HEADERS)
	time $(CC) $(COPTS) treebench_packed_structs.c -o $@ 

treebench_c_packed_parallel.exe: treebench_packed_parallel.c
	time $(CPP) -fpermissive -fcilkplus $(CPPOPTS) $^ -o $@ 

treebench_c_packed_parallel2.exe: treebench_packed_parallel2.c
	time $(CC) -fcilkplus $(COPTS) $^ -o $@

treebench_c_packed_parallel3.exe: treebench_packed_parallel3.c $(HEADERS)
	time $(CC) -fcilkplus $(COPTS) treebench_packed_parallel3.c -o $@ 

treebench.class: treebench.java
	time javac $^ 


# BuildTree benchmark
# ==============================================================================

TREELANGDIR=$(shell cd ..; pwd)
TREEC=$(shell cd ../TreeLang; stack exec -- which treec)

# Make sure the compiler is build and not stale:
stack_build:
	cd $(TREELANGDIR)/TreeLang; stack build

buildtree: stack_build buildtree_treelang_c_packed.exe
	raco make buildtree_treelang.sexp
# buildtree_treelang_c_pointer.exe

# Here we control the C file output in case we build in parallel:
buildtree_treelang_c_packed.exe: buildtree_treelang.sexp
	$(TREEC)  --packed --cfile=$(@:.exe=.c) --exefile $@ $^

buildtree_treelang_c_pointer.exe: buildtree_treelang.sexp
	$(TREEC)  --pointer --cfile=$(@:.exe=.c) --exefile $@ $^

# SumTree benchmark
# ==============================================================================
# Identical to buildtree.  s/buildtree/sumtree/

# --pointer not working yet:
sumtree: stack_build sumtree_treelang_c_packed.exe
	raco make sumtree_treelang.sexp

sumtree_treelang_c_packed.exe: sumtree_treelang.sexp
	$(TREEC)  --packed --cfile=$(@:.exe=.c) --exefile $@ $^

sumtree_treelang_c_pointer.exe: sumtree_treelang.sexp
	$(TREEC)  --pointer --cfile=$(@:.exe=.c) --exefile $@ $^




# Running
# ==============================================================================

DEPTH=20
DEFAULT_ITERS=17

run_small:
	$(MAKE) DEPTH=6 DEFAULT_ITERS=10 run_all

run_small_core:
	$(MAKE) DEPTH=6 DEFAULT_ITERS=10 run_core

# TODO: replace with an hsbencher harness / Criterion:
run_all: all run_core
	./treebench_mlton.exe       $(DEPTH) $(DEFAULT_ITERS)
	./treebench_ocaml.exe       $(DEPTH)
	./treebench_rust.exe        $(DEPTH)
	$(MAKE) run_chez
	$(MAKE) run_java

# the main ones we are interested in benchmarking:
run_core: c ghc
	./treebench_ghc_strict.exe  seq $(DEPTH) $(DEFAULT_ITERS)
	./treebench_ghc_lazy.exe    $(DEPTH) $(DEFAULT_ITERS)
	./treebench_c.exe           $(DEPTH) $(DEFAULT_ITERS)
	./treebench_c_packed.exe    $(DEPTH) $(DEFAULT_ITERS)
	./treebench_c_packed_loop.exe $(DEPTH) $(DEFAULT_ITERS)
	$(MAKE) run_racket
	$(MAKE) run_racket_packed

#	./treebench_rust_sys_alloc.exe $(DEPTH) 

run_chez:
	scheme --script treebench.ss $(DEPTH) $(DEFAULT_ITERS)

run_racket:
	racket treebench.rkt $(DEPTH) $(DEFAULT_ITERS)

run_racket_packed:
	racket treebench_packed.rkt $(DEPTH)

run_java: treebench.class
	java treebench $(DEPTH) 33

docker:
	docker build -t bintree-bench .

clean:
	rm -f *.exe *.o *.hi treebench treebench_lazy *.cmi *.cmo *.cmx

.PHONY: all clean ghc run_chez run_java run_all run_small c ghc buildtree stack_build
