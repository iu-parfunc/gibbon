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

#include <stdio.h>
#include <sexp.h>
#include <sexp_ops.h>
#include <assert.h>
#include <string.h>

/**
 * Given an s-expression of the form:
 *
 * (tag (val_list))
 *
 * Where each value in the val_list is a float, extract the data and
 * populate the array passed in (the length of the array is in `size').
 * Assume the array was allocated properly, size is correct, and the
 * expression contains the right number of values.  This eliminates lots
 * of error checking code and makes the process a bit clearer.
 */
void extract(sexp_t *sx, float *data, size_t size) {
  sexp_t *s;
  unsigned int i;

  assert(sx != NULL && data != NULL); /* duh */

  /* assumption : s-expression is formatted as (tag (val1 val2 val3)) */
  printf("Data tag: [%s]\n",sx->list->val);

  /* s = (vallist) */
  s = sx->list->next;
  
  i = 0;
  s = s->list;
  while (s != NULL && i < size) {
    sscanf(s->val,"%f",&data[i]);
    s = s->next;
    i++;
  }
}

/**
 * example of packing three values into an expression of the form:
 *
 * (tag (v1 v2 v3))
 *
 * COMMENTS SHOW THE STATE OF SX AS IT IS CONSTRUCTED
 */
sexp_t *pack(char *tag, float v1, float v2, float v3) {
  sexp_t *sx = new_sexp_list(new_sexp_atom(tag,strlen(tag),SEXP_BASIC));
  /* sx = (tag) */
  char sbuf[32];
  sexp_t *vlist,*vptr;
  

  vlist = new_sexp_list(NULL); /* vlist = () */
  sx->list->next = vlist; /* sx = (tag ()) */

  sprintf(sbuf,"%f",v1);
  vlist->list = new_sexp_atom(sbuf,strlen(sbuf),SEXP_BASIC); /* vlist = (v1) */
  vptr = vlist->list;

  /* sx = (tag (v1)) */

  sprintf(sbuf,"%f",v2);
  vptr->next = new_sexp_atom(sbuf,strlen(sbuf),SEXP_BASIC); /* vlist = (v1 v2) */
  vptr = vptr->next;

  /* sx = (tag (v1 v2)) */

  sprintf(sbuf,"%f",v3);
  vptr->next = new_sexp_atom(sbuf,strlen(sbuf),SEXP_BASIC); /* vlist = (v1 v2 v3) */

  /* sx = (tag (v1 v2 v3))  ---- done, return. */

  return sx;
}

/****
 * main
 ****/
int main(int argc, char **argv) {
  char buf[256]; /* string to sprintf to */
  float vals[3]; /* place to put data */
  sexp_t *sx = NULL;

  /*** method #1: create expression as string on one side, extract data
       on the other. ***/

  printf("===>> PART 1 <<===\n");
  sprintf(buf,"(thetag (1.0 2.0 3.0))");
  sx = parse_sexp(buf,strlen(buf));

  extract(sx,vals,3);

  printf("Extracted V1=%f V2=%f V3=%f\n",vals[0],vals[1],vals[2]);

  destroy_sexp(sx);

  /*** method #2: packing function creates expression, same extract
                  function extracts data.  print in between to show expression.
   ***/
  printf("\n===>> PART 2 <<===\n");
  sx = pack("part2tag",4.0,5.0,6.0);
  print_sexp(buf,256,sx);
  printf("SX=%s\n",buf);

  extract(sx,vals,3);
  printf("Extracted V1=%f V2=%f V3=%f\n",vals[0],vals[1],vals[2]);

  destroy_sexp(sx);
  sexp_cleanup();

  return 0;
}
