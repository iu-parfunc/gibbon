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

#include <fcntl.h>
#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include "sexp.h"
#include "sexp_ops.h"

int main(int argc, char **argv) {
  int fd;
  char pstr[1024];
  sexp_t *sx, *param;
  sexp_iowrap_t *iow;

  fd = open("testrc",O_RDONLY);
  iow = init_iowrap(fd);
  sx = read_one_sexp(iow);
  
  param = find_sexp("parameter1",sx);
  print_sexp(pstr,1024,param->next);
  printf("parameter1 = %s\n",pstr);
  
  param = find_sexp("parameter2",sx);
  print_sexp(pstr,1024,param->next);
  printf("parameter2 = %s\n",pstr);
  
  param = find_sexp("parameter3",sx);
  if (param == NULL) {
    printf("parameter3 not defined.\n");
  }
  
  destroy_sexp(sx);
  destroy_iowrap(iow);
  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
