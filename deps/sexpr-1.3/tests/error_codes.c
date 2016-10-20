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

int main(int argc, char **argv) {
  pcont_t *pc = NULL;
  char buf[128];
  CSTRING *s = NULL;
  int len;

  sprintf(buf,"(I am a (test))");

  pc = init_continuation(buf);
  if (pc == NULL) {
    printf("init_continuation returned NULL.\n");
    exit(EXIT_FAILURE);
  }

  pc = cparse_sexp(NULL,42,pc);
  if (pc->error != SEXP_ERR_OK) {
    printf("cparse reacted properly to null string.\nerr=%d\n",
	   pc->error);
  }
  destroy_continuation(pc);
  pc = NULL;

  pc = cparse_sexp(buf,strlen(buf),pc);

  if (pc->error != SEXP_ERR_OK) {
    printf("cparse returned unexpected error.\n");
    exit(EXIT_FAILURE);
  }

  len = print_sexp_cstr(&s,NULL,42);
  if (len == -1) {
    printf("print_sexp_cstr reacted correctly to NULL sexp_t.\nerr=%d\n",
	   sexp_errno);
  } else {
    printf("print_sexp_cstr did not bail out properly given a NULL sexp_t.\n");
    exit(EXIT_FAILURE);
  }
  if (s != NULL) {
    sdestroy(s);
    s = NULL;
  }

  len = print_sexp_cstr(&s,pc->last_sexp,128);
  if (len == -1) {
    printf("print_sexp_cstr reacted incorrectly to valid sexp_t.\nerr=%d\n",
	   sexp_errno);
    exit(EXIT_FAILURE);
  }
  printf("%s\n",s->base);

  sdestroy(s);
  destroy_sexp(pc->last_sexp);
  destroy_continuation(pc);
  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
