

all: deps subst racket

deps:
	cd deps/sexpr-1.3 && ./configure && $(MAKE)

subst:
	cd ASTBenchmarks/substitution && $(MAKE) 

racket: gibbon-lang
gibbon-lang:
	raco pkg update --link ./gibbon

.PHONY: subst deps all racket gibbon-lang
