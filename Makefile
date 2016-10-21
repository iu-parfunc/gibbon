

all:
	cd deps/sexpr-1.3 && ./configure && $(MAKE) 
	cd ASTBenchmarks/substitution && $(MAKE) 

