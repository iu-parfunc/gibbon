/**
@cond IGNORE

======================================================
 SFSEXP: Small, Fast S-Expression Library version 1.2
 Written by Matthew Sottile (mjsottile@gmail.com)
======================================================

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

@endcond
**/
#include <stdlib.h>
#include <stdio.h>
#include "sexp.h"
#include "sexp_errors.h"
#include "sexp_memory.h"

#ifdef _SEXP_LIMIT_MEMORY_

static size_t sexp_max_memory  = 32*1024*1024; /* default: 32MB */
static size_t sexp_used_memory = 0;

size_t get_sexp_max_memory() {
  return sexp_max_memory;
}

size_t get_sexp_used_memory() {
  return sexp_used_memory;
}

int set_sexp_max_memory(size_t newsize) {
  if (newsize > 0) {
    if (newsize < sexp_used_memory) {
      sexp_errno = SEXP_ERR_BAD_PARAM;
      return -1;
    } else {
      sexp_max_memory = newsize;
    }
  } else {
    sexp_errno = SEXP_ERR_BAD_PARAM;
    return -1;
  }

  return sexp_max_memory;
}

void *sexp_malloc(size_t size) {
  void *ptr;

  if (sexp_used_memory+size > sexp_max_memory) {
    sexp_errno = SEXP_ERR_MEM_LIMIT;
    return NULL;
  }

  ptr = malloc(size);
  if (ptr != NULL) sexp_used_memory += size;

  return ptr;
}

void *sexp_calloc(size_t count, size_t size) {
  void *ptr;

  if (sexp_used_memory+(size*count) > sexp_max_memory) {
    sexp_errno = SEXP_ERR_MEM_LIMIT;
    return NULL;
  }

  ptr = calloc(count, size);
  if (ptr != NULL) sexp_used_memory += size*count;

  return ptr;
}


void sexp_free(void *ptr, size_t size) {
  if (sexp_used_memory < size) {
    fprintf(stderr,"ERROR: sexp_free called too many times!\n");
  } else {
    sexp_used_memory -= size;
  }

  free(ptr);
}

void *sexp_realloc(void *ptr, size_t size, size_t oldsize) {
  void *p;

  if (sexp_used_memory+(size-oldsize) > sexp_max_memory) {
    sexp_errno = SEXP_ERR_MEM_LIMIT;
    return NULL;
  }

  p = realloc(ptr,size);
  if (p != NULL) sexp_used_memory += size-oldsize;

  return p;
}

#endif /* _SEXP_LIMIT_MEMORY_ */
