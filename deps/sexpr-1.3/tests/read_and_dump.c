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

#include <assert.h>
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
  int ch;
  char fname[BUFSIZ];
  CSTRING *s1 = NULL;
  int binmode = 0;

  strcpy(fname,TESTFILE);

  while ((ch = getopt(argc,argv,"f:b")) != -1) {
    switch ((char)ch) {
    case 'f':
      strcpy(fname,optarg);
      break;
    case 'b':
      binmode = 1;
      break;
    default:
      break;
    }
  }

  fd = open(fname,O_RDONLY);

  iow = init_iowrap(fd);

  if (binmode == 1) {
    iow->cc = init_continuation(NULL);
    iow->cc->mode = PARSER_INLINE_BINARY;
  }

  sx = read_one_sexp(iow);

  while (sx != NULL) {
    assert(sx != NULL);
    print_sexp_cstr(&s1,sx,8);
    destroy_sexp(sx);
    
    sexp_cleanup();

    printf("READ: %s\n", s1->base);

    /** clean up strings **/
    sdestroy(s1);
    s1 = NULL;

    sx = read_one_sexp(iow);
  }
  
  destroy_iowrap(iow);
  close(fd);

  exit(0);
}
