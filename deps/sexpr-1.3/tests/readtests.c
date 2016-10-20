/**

SFSEXP: Small, Fast S-Expression Library version 1.0
Written by Matthew Sottile (mjsottile@gmail.com)

Copyright (2003-2006). The Regents of the University of California. This
material was produced under U.S. Government contract W-7405-ENG-36 for Los
Alamos National Laboratory, which is operated by the University of
California for the U.S. Department of Energy. The U.S. Government has rights
to use, reproduce, and distribute this software. NEITHER THE GOVERNMENT NOR
THE UNIVERSITY MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
LIABILITY FOR THE USE OF THIS SOFTWARE. If software is modified to produce
derivative works, such modified software should be clearly marked, so as not
to confuse it with the version available from LANL.

Additionally, this library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
for more details.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, U SA

LA-CC-04-094

**/

#include "config.h"

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <stdio.h>
#include "sexp.h"
#include <sys/time.h>
#include <time.h>
#include <fcntl.h>
#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include <string.h>

#define TESTFILE "test_expressions"

int main(int argc, char **argv) {
  int fd;
  sexp_t *sx;
  sexp_iowrap_t *iow;
  int diff;
  unsigned int i;
  int ch;
  char fname[BUFSIZ];
  CSTRING *s1,*s2;
  int passes, failures;
  char outbuf1[8192], outbuf2[8192];
  
  s1 = s2 = NULL;
  passes = failures = 0;

  strcpy(fname,TESTFILE);

  while ((ch = getopt(argc,argv,"f:")) != -1) {
    switch ((char)ch) {
    case 'f':
      strcpy(fname,optarg);
      break;
    default:
      break;
    }
  }

  fd = open(fname,O_RDONLY);

  iow = init_iowrap(fd);

  printf("TESTING CSTRING BASED UNPARSE:\n");
  
  sx = read_one_sexp(iow);

  while (sx != NULL) {

    print_sexp_cstr(&s1,sx,8);
    destroy_sexp(sx);
    
    sx = parse_sexp(s1->base,s1->curlen);
    if (sx == NULL) {
      fprintf(stderr,"ERROR: parser error state of %d\n",
	      sexp_errno);
      exit(1);
    }
    print_sexp_cstr(&s2,sx,8);
    destroy_sexp(sx);
    
    sexp_cleanup();

    diff = 0;
    for (i=0;i<s1->curlen;i++) {
      diff += abs((s1->base[i] - s2->base[i]));
      if (s1->base[i] == '\0')
        break;
    }
        
    /**
     * diff is the lexical difference between the first unparsing
     * of the original buffer and the unparsed version of the parsed
     * version of the first unparsed string.  In other words, does:
     * 
     *   orig->parse->unparse == orig->parse->unparse->parse->unparse
     *
     * This catches issues with print and parse to make sure the meaning
     * of the original is kept (or at least, "bugs" in the parser have
     * matching "bugs" in the printer.)
     */
    if (diff != 0) {
      printf("FIXED POINT MISSED (diff=%d): \nS1: %s\nS2: %s\n",diff,
             s1->base,s2->base);
      failures++;
    } else {
      passes++;
    }

    /** clean up strings **/
    sdestroy(s1);
    sdestroy(s2);
    s1 = s2 = NULL;

    sx = read_one_sexp(iow);
  }
  
  destroy_iowrap(iow);
  close(fd);
  
  printf("TOTAL TESTS: %d  PASS=%d FAIL=%d\n\n",
          passes+failures,passes,failures);

  passes = failures = 0;

  /***
   *** now do normal fixed length buffer unparse testing
   ***/
  fd = open(fname,O_RDONLY);

  iow = init_iowrap(fd);

  printf("TESTING FIXED SIZE BUFFER BASED UNPARSE:\n");
  
  sx = read_one_sexp(iow);

  while (sx != NULL) {
    print_sexp(outbuf1,8192,sx);
    destroy_sexp(sx);
    
    sx = parse_sexp(outbuf1,8192);
    if (sx == NULL) {
      fprintf(stderr,"ERROR: parser error state of %d\n",
	      sexp_errno);
      exit(1);
    }
    print_sexp(outbuf2,8192,sx);
    destroy_sexp(sx);
    
    sexp_cleanup();

    diff = 0;
    for (i=0;i<8192;i++) {
      diff += abs((outbuf1[i] - outbuf2[i]));
      if (outbuf1[i] == '\0' || outbuf2[i] == '\0')
        break;
    }
        
    /**
     * diff is the lexical difference between the first unparsing
     * of the original buffer and the unparsed version of the parsed
     * version of the first unparsed string.  In other words, does:
     * 
     *   orig->parse->unparse == orig->parse->unparse->parse->unparse
     *
     * This catches issues with print and parse to make sure the meaning
     * of the original is kept (or at least, "bugs" in the parser have
     * matching "bugs" in the printer.)
     */
    if (diff != 0) {
      printf("FIXED POINT MISSED (diff=%d): \nS1: %s\nS2: %s\n",diff,
             outbuf1,outbuf2);
      failures++;
    } else {
      passes++;
    }

    sx = read_one_sexp(iow);
  }
  
  destroy_iowrap(iow);
  close(fd);

  printf("TOTAL TESTS: %d  PASS=%d FAIL=%d\n",
          passes+failures,passes,failures);

  exit(0);
}
