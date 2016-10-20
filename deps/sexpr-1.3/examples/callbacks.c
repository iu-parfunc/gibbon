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

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "sexp.h"
#include <sys/time.h>
#include <time.h>
#include <assert.h>
#include <string.h>

typedef enum { WAITING, NEXT_EDGE, GET_X1, GET_X2, GET_Y1, GET_Y2 } state_t;

static double x1,x2,y1,y2;
static state_t current_state = WAITING;
static int depth = 0;

void add_edge(double ax, double ay, double bx, double by) {
  printf("EDGE DEFINED FROM (%f,%f) to (%f,%f)\n",ax,ay,bx,by);
}

void enter_sexpr() {
  depth++;
  switch (current_state) {
  case WAITING:
    current_state = NEXT_EDGE;
    break;
  case NEXT_EDGE:
  case GET_X1:
    current_state = GET_X1;
    break;
  case GET_X2:
    current_state = GET_X2;
    break;
  default:
    printf("This is unexpected.\n");
    break;
  }
}

void exit_sexpr() {
  depth--;
  switch (current_state) {
  case GET_Y1:
    current_state = GET_X2;
    break;
  case GET_Y2:
    add_edge(x1,y1,x2,y2);
  case NEXT_EDGE:
    current_state = NEXT_EDGE;
    break;
  default:
    printf("This is also unexpected.\n");
    break;
  }

  if (depth == 0) {
    printf("All edges for room found.\n");
    current_state = WAITING;
  }
}

void characters(const char *buf, size_t len, atom_t aty) {
  switch (current_state) {
  case NEXT_EDGE:
    printf("%s\n",buf);
    break;
  case GET_X1:
    x1 = strtod(buf,NULL);
    current_state = GET_Y1;
    break;
  case GET_X2:
    x2 = strtod(buf,NULL);
    current_state = GET_Y2;
    break;
  case GET_Y1:
    y1 = strtod(buf,NULL);
    break;
  case GET_Y2:
    y2 = strtod(buf,NULL);
    break;
  default:
    printf("Also not expected.\n");
    break;
  }
}

int main(int argc, char **argv) {
  parser_event_handlers_t peh_t;
  int fd;
  sexp_iowrap_t *iow = NULL;
  sexp_t *sx = NULL;

  current_state = WAITING;

  peh_t.start_sexpr = enter_sexpr;
  peh_t.end_sexpr = exit_sexpr;
  peh_t.characters = characters;

  fd = open("edgelist.dat", O_RDONLY);
  iow = init_iowrap(fd);

  iow->cc = init_continuation(NULL);
  iow->cc->event_handlers = &peh_t;

  do {
    if (sx != NULL)
      destroy_sexp(sx);
    sx = read_one_sexp(iow);
  } while (sx != NULL);

  destroy_iowrap(iow);
  sexp_cleanup();
  close(fd);

  exit(EXIT_SUCCESS);
}
