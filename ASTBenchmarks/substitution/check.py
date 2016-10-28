#!/usr/bin/python

import os
import errno
import re

CHECK="check.exe"
INFILE="sexp.in"
ITERS=1

def silentremove(filename):
    try:
        os.remove(filename)
    except OSError as e: 
        if e.errno != errno.ENOENT: 
            raise 

def check():
    with open('tests.sexp', 'r') as fp:
        data=fp.read().replace('\n', '')

    tests = re.split(">>>", data)
    tests = filter(lambda x: x != '', tests)

    fails = []
    for idx, test in enumerate(tests):
        print ("\n---> Running test %d\n" % (idx))
        test = test + '\n'
        silentremove("sexp.in")
        with open ("sexp.in", "w") as in_file:
            in_file.write(test)
        cmd = "./%s %s %d" % (CHECK, INFILE, ITERS)
        ret = os.system(cmd)
        if ret != 0:
            fails.append(idx);

    print ("\n[PASSED] : %d" % (len(tests) - len(fails)))
    print ("[FAILED] : %d"   % (len(fails)))
    print (" %s" % fails)

if __name__ == "__main__":
    check()
