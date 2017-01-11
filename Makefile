

all: racket subst

# [2017.01.11] Not building this by default for now. Not using it (yet):
deps:
	cd deps/sexpr-1.3 && ./configure && $(MAKE)

subst:
	cd ASTBenchmarks/substitution && $(MAKE) 

racket: gibbon-lang

SANDBOX=.racket_sandbox

gibbon-lang: $(SANDBOX)
	rm -rf $(SANDBOX)/*
	PLTADDONDIR=`pwd`/$(SANDBOX) raco pkg install --link ./gibbon

# OLD WAY: user-wide install.  No good for testing.
# RRN [2016.12.28] was there a single command that is idempotent?
#	raco pkg install --link ./gibbon || raco pkg update --link ./gibbon

$(SANDBOX):
	mkdir -p $@

.PHONY: subst deps all racket gibbon-lang
