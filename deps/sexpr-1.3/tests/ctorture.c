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

#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include <fcntl.h>
#include "sexp.h"
#include <sys/time.h>
#include <time.h>

/**
 ** defaults
 **/
#define MAXSIZE  8000000
#define DEFITERS 100000

/**
 ** for fixed size buffer tests, here are the buffers.
 **/
char buf[MAXSIZE], pstr[MAXSIZE];

/* flag defining which type of print_sexp to use */
enum { FIXBUF, CSTR };

/**
 ** usage()
 **/
void usage(char *av0) {
  printf("\nusage: %s [-t] [-c] [-i iters] [-f fname] [-h]\n\n",av0);
  printf("  -i iters : specify number of iterations (default: %d)\n", DEFITERS);
  printf("  -f fname : read expression from file fname (default: STDIN)\n");
  printf("  -c       : toggle CSTRING-based print_sexp (default: fixed buffer ~8MB)\n");
  printf("  -t       : execute original test from torture.c (see * below)\n");
  printf("  -h       : usage\n\n");
  printf("* please note that ctorture.c subsumes and replaces torture.c from now on.\n\n");
  exit(EXIT_FAILURE);
}

/**
 ** main()
 **/
int main(int argc, char **argv) {
  sexp_t           *sx;
  int              i, maxiters, fd;
  struct timeval   t_start, t_end;
  float            elapsed;
  pcont_t          *cc = NULL;
  sexp_iowrap_t    *iow;
  int              ch;
  int              print_type = FIXBUF;
  CSTRING          *cs1, *cs2;
  int              do_old_torture = 0;

  maxiters = DEFITERS; /* default */
  fd = STDIN_FILENO;   /* default */

  while ((ch = getopt(argc,argv,"i:f:cht")) != -1) {
    switch ((char)ch) {
    case 'i':
      maxiters = atoi(optarg);
      if (maxiters < 1) {
        maxiters = DEFITERS; /* default if argument was less
				than 1 */
        fprintf(stderr,"WARNING: specified iteration count invalid (%s)\n",
                optarg);
      }
      
      break;
    case 'f':
      fd = open(optarg,O_RDONLY);
      if (fd == -1) {
	fprintf(stderr,"ERROR: could not open file %s for input.\n",
		optarg);
	exit(EXIT_FAILURE);
      }
      break;
    case 'h':
      usage(argv[0]);
      break;
    case 't':
      do_old_torture = 1;
      break;
    case 'c':
      print_type = CSTR;
      break;
    default:
      break;
    }
  }

  /* init cstrings to null */
  cs1 = cs2 = NULL;

  iow = init_iowrap(fd);
  sx = read_one_sexp(iow);

  /* this is the code that replaces the old torture.c code */
  if (do_old_torture == 1) {
    print_sexp(buf,MAXSIZE,sx);
    destroy_sexp(sx);
    
    gettimeofday(&t_start,0);
    
    for (i=0;i<maxiters;i++) {
      sx = parse_sexp(buf,MAXSIZE);
      print_sexp(pstr,MAXSIZE,sx);
      destroy_sexp(sx);
    }
    
    gettimeofday(&t_end,0);
    
    elapsed = ((double)t_end.tv_sec + 
	       (double)((double)t_end.tv_usec)/1000000)-
      ((double)t_start.tv_sec + (double)((double)t_start.tv_usec)/1000000);
    
    printf("%f\n",elapsed);
    
    exit(EXIT_SUCCESS);
  }
  /* end of old torture.c code */

  if (print_type == CSTR)
    print_sexp_cstr(&cs1,sx,16535);
  else
    print_sexp(buf,MAXSIZE,sx);
  destroy_sexp(sx);
    
  gettimeofday(&t_start,0);

  /* continuation based torture. */

  for (i=0;i<maxiters;i++) {
    if (print_type == CSTR)
      cc = cparse_sexp(cs1->base,cs1->curlen,cc);
    else
      cc = cparse_sexp(buf,MAXSIZE,cc);
    sx = cc->last_sexp;
    cc->lastPos = NULL;
    if (print_type == CSTR)
      print_sexp_cstr(&cs2,sx,(cs1->curlen)+1);
    else
      print_sexp(pstr,MAXSIZE,sx);
    destroy_sexp(sx);
    if (print_type == CSTR) {
      sempty(cs2);
    }
  }

  destroy_iowrap(iow);

  if (fd != STDIN_FILENO)
    close(fd);

  sexp_cleanup();
  destroy_continuation(cc);
  if (print_type == CSTR) {
    sdestroy(cs2);
    sdestroy(cs1);
  }

  gettimeofday(&t_end,0);
  
  elapsed = ((double)t_end.tv_sec + (double)((double)t_end.tv_usec)/1000000)-
    ((double)t_start.tv_sec + (double)((double)t_start.tv_usec)/1000000);
  
  printf("%f\n",elapsed);

  exit(EXIT_SUCCESS);
}
