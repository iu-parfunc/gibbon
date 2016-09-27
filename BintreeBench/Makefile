all: treebench_mlton.exe treebench_strict_ghc.exe treebench_lazy_ghc.exe treebench_ocaml.exe \
     treebench_rust.exe treebench_rust_sys_alloc.exe \
     treebench_c_unpacked.exe
# treebench_rust_unpacked.exe

GHC = stack --install-ghc --resolver=lts-7.0 exec ghc --

ghc: treebench_strict_ghc.exe treebench_lazy_ghc.exe

treebench_mlton.exe: treebench.sml
	time mlton -output $@ $^

treebench_strict_ghc.exe: treebench.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_lazy_ghc.exe: treebench_lazy.hs
	time $(GHC) -O2 -rtsopts $^ -o $@

treebench_ocaml.exe: treebench.ml
	time ocamlopt.opt $^ -o $@

treebench_rust.exe: treebench.rs
	time rustc $^ -o $@ -O

treebench_rust_sys_alloc.exe: treebench_sys_alloc.rs
	time rustc $^ -o $@ -O

# treebench_rust_unpacked.exe: treebench_unpacked.rs
# 	time rustc $^ -o $@ -O

treebench_c_unpacked.exe: treebench_unpacked.c
	time gcc -O3 $^ -o $@ 

clean:
	rm -f *.exe *.o *.hi treebench treebench_lazy *.cmi *.cmo *.cmx

.PHONY: all clean ghc
