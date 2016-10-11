all: treebench_mlton.exe treebench_ocaml.exe \
     treebench_strict_ghc.exe treebench_lazy_ghc.exe treebench_ghc_packed.exe \
     treebench_rust.exe \
     treebench_c.exe treebench_c_bumpalloc.exe treebench_c_unpacked.exe \
     treebench.class

# Disabling for now, requires beta channel of rust:
# treebench_rust_sys_alloc.exe 

# Unfinished:
# treebench_rust_unpacked.exe

# Match whichever version the docker image is using:
# GHC = stack --install-ghc --resolver=lts-7.1 exec ghc --
GHC = stack --install-ghc --resolver=lts-6.19 exec ghc --
CC = icpc 
# CC = clang

# COPTS = -O3 -std=c11
COPTS = -g -O0 -std=c++11


ghc: treebench_strict_ghc.exe treebench_lazy_ghc.exe

treebench_mlton.exe: treebench.sml
	time mlton -output $@ $^

treebench_strict_ghc.exe: treebench.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_lazy_ghc.exe: treebench_lazy.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_ghc_packed.exe: treebench_packed.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_ocaml.exe: treebench.ml
	time ocamlopt.opt $^ -o $@

treebench_rust.exe: treebench.rs
	time rustc $^ -o $@ -O

treebench_rust_sys_alloc.exe: treebench_sys_alloc.rs
	time rustc $^ -o $@ -O

# treebench_rust_unpacked.exe: treebench_unpacked.rs
# 	time rustc $^ -o $@ -O

treebench_c.exe: treebench.c
	time $(CC) $(COPTS) $^ -o $@ 

treebench_c_bumpalloc.exe: treebench.c
	time $(CC) $(COPTS) -DBUMPALLOC $^ -o $@ 

treebench_c_unpacked.exe: treebench_unpacked.c
	time $(CC) $(COPTS) $^ -o $@ 

treebench_c_parallel.exe: treebench_parallel.c
	time $(CC) $(COPTS) $^ -o $@ 

treebench.class: treebench.java
	time javac $^ 

DEPTH=20

# TODO: replace with an hsbencher harness:
run_all: all
	./treebench_mlton.exe $(DEPTH)
	./treebench_strict_ghc.exe $(DEPTH)
	./treebench_lazy_ghc.exe $(DEPTH)
	./treebench_ocaml.exe $(DEPTH)
	./treebench_rust.exe $(DEPTH)
	./treebench_rust_sys_alloc.exe $(DEPTH) 
	./treebench_c.exe $(DEPTH)
	./treebench_c_unpacked.exe $(DEPTH) 
	$(MAKE) run_chez
	$(MAKE) run_java

run_chez:

run_java: treebench.class
	java treebench $(DEPTH)

clean:
	rm -f *.exe *.o *.hi treebench treebench_lazy *.cmi *.cmo *.cmx

.PHONY: all clean ghc run_chez run_java
