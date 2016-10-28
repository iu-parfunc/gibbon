

CC=gcc
CFLAGS=-O0 -g

LDPATH=../../deps/sexpr-1.3/src
INCLUDES=../../deps/sexpr-1.3/src

all: subst_c.exe

LOG=subst_all_expanded_1.2.log

scan_for_errors:
	grep -v "Benchmark:" $(LOG)  | grep -v  "==========" | grep -v "Done ingesting" | grep -v "cpu time" | grep -v "^$$" | tee wc 

check: check.exe
	./check.py

check.exe: check.c 
	$(CC) $(CFLAGS) -c -I$(INCLUDES) ast.c -o ast.o 
	$(CC) $(CFLAGS) -I$(INCLUDES) $^ ast.o -o $@ -L$(LDPATH) -lsexp

subst_c.exe: subst.c
	$(CC) $(CFLAGS) -I$(INCLUDES) $^ -o $@ -L$(LDPATH) -lsexp

subst_v1_c.exe: subst_v1.c
	$(CC) $(CFLAGS) -I$(INCLUDES) $^ -o $@ -L$(LDPATH) -lsexp

subst_packed.exe: subst_packed.c
	$(CC) $(CFLAGS) -I$(INCLUDES) $^ -o $@ -L$(LDPATH) -lsexp

clean:
	rm -f *.o *.exe
