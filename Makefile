

all: deps subst racket

deps:
	cd deps/sexpr-1.3 && ./configure && $(MAKE)

subst:
	cd ASTBenchmarks/substitution && $(MAKE) 

racket: gibbon-lang
gibbon-lang:
# RRN [2016.12.28] was there a single command that is idempotent?
	raco pkg install --link ./gibbon || raco pkg update --link ./gibbon

.PHONY: subst deps all racket gibbon-lang
