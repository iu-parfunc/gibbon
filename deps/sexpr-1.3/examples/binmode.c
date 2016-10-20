/**

SFSEXP: Small, Fast S-Expression Library version 1.3
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

#include <sys/fcntl.h>
#include "sexp.h"
#include <assert.h>
#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include <stdio.h>

/**
 * read a binary file from fd.  Length is filled in, bss is buffer
 * start size and bgs is buffer growth size for realloc()ing.  returns
 * pointer to byte-buffer.  if NULL, then there was some sort of error.
 */
char *readfile(int fd, size_t *length, size_t bss, size_t bgs) {
  char *buf, *tmp;
  size_t bused, ballocd;
  int amt;

  /* check for either errors on the caller side */
  assert(fd > 0);

  bused = 0;
  ballocd = bss;

  buf = (char *)malloc(sizeof(char)*bss);
  assert(buf != NULL);

  while ((amt = read(fd,buf+bused,ballocd-bused)) > 0) {
    bused += amt;
    if (bused == ballocd) {
      tmp = (char *)realloc(buf,ballocd+bgs);
      assert(tmp != NULL);
      buf = tmp;
      ballocd += bgs;
    }
  }

  if (bused == 0) return NULL;

  *length = bused;
  return buf;
}

int main(int argc, char **argv) {
  sexp_t *sx_in, *sx_binatom, *sx_out;
  int fd, status;
  char *b;
  size_t l = 0;
  CSTRING *s = NULL;
  pcont_t *pc;

  /* read data */
  fd = open("testdata",O_RDONLY);
  if (fd <= 0) {
    printf("Error opening test data file ``testdata''\n");
    exit(EXIT_FAILURE);
  }
  b = readfile(fd,&l,1024,256);
  close(fd);
  
  /* report */
  printf("Read %lu bytes of data.\n",(unsigned long)l);

  sx_binatom = new_sexp_binary_atom(b, l);
  assert(sx_binatom != NULL);

  /* IMPORTANT: since sx_binatom is now being subsumed by
     the list sx_in that contains it, we will not need to
     explicitly free sx_binatom since it will be freed
     during the traversal when sx_in is destroyed. */
  sx_in = new_sexp_list(sx_binatom);

  printf("Created expression.\n");

  print_sexp_cstr(&s,sx_in,l+1024);
  destroy_sexp(sx_in);

  b = NULL;
  sx_in = NULL;

  printf("Destroyed AST and buffer.\n");

  pc = init_continuation(NULL);
  pc->mode = PARSER_INLINE_BINARY;
  pc = cparse_sexp(s->base,s->len,pc);
  sx_out = pc->last_sexp;

  printf("Parsed unparsed version back to AST.\n");

  assert(sx_out != NULL);

  b = sx_out->list->bindata;
  l = sx_out->list->binlength;

  fd = open("testdata_out",O_RDWR|O_CREAT,0644);
  if (fd <= 0) {
    printf("Error opening ``testdata_out'': Create empty file to write to.\n");
    exit(EXIT_FAILURE);
  }
  status = write(fd,b,l);
  if (status < 0) {
    printf("Write failed.\n");
    exit(EXIT_FAILURE);
  }
  close(fd);

  sdestroy(s);

  destroy_continuation(pc);
  destroy_sexp(sx_out);
  sexp_cleanup();

  printf("Extracted and wrote bindata from AST.\n");

  exit(EXIT_SUCCESS);
}
