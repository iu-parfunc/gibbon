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
/**
 * Implementation of stuff in cstring.h to make ron happy 
 */
#include "cstring.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sexp_memory.h"
#include "sexp.h"

/**
 * growth size for cstrings -- default is 8k.  use sgrowsize() to adjust. 
 */
static size_t cstring_growsize = 8192;

void sgrowsize(size_t s) {
  if (s < 1) 
    return;

  cstring_growsize = s;
}

CSTRING *snew(size_t s) {
  CSTRING *cs;

#ifdef __cplusplus
  cs = (CSTRING *)sexp_malloc(sizeof(CSTRING));
#else
  cs = sexp_malloc(sizeof(CSTRING));
#endif
  if (cs == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  cs->len = s;
  cs->curlen = 0;

#ifdef __cplusplus
  cs->base = (char *)sexp_calloc(sizeof(char),s);
#else
  cs->base = sexp_calloc(sizeof(char),s);
#endif
  if (cs->base == NULL) {
    sexp_free(cs,sizeof(CSTRING));
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  return cs;
}

CSTRING *sadd(CSTRING *s, char *a) {
  size_t alen;
  char *newbase;

  /* no string, so bail */
  if (s == NULL) {
    return NULL;
  }

  /* nothing to add, return s */
  if (a  == NULL) {
    return s;
  }

  alen = strlen(a);

  if (s->curlen + alen >= s->len) {
#ifdef __cplusplus
    newbase = (char *)sexp_realloc(s->base,
                                   s->len+cstring_growsize+alen,
                                   s->len);
#else
    newbase = sexp_realloc(s->base,
                           s->len+cstring_growsize+alen,
                           s->len);
#endif
    
    /* do NOT destroy s anymore.  if realloc fails, the original data is
       still valid, so just report the error to sexp_errno and return NULL.
    */
    if (newbase == NULL) {
      sexp_errno = SEXP_ERR_MEMORY;      
      return NULL;
    }

    s->len += cstring_growsize + alen;
    s->base = newbase;
  }

  memcpy(&s->base[s->curlen],a,alen);
  s->curlen += alen;
  s->base[s->curlen] = 0;
  return s;
}

CSTRING *saddch(CSTRING *s, char a) {
  char *newbase;

  if (s == NULL) {
    return NULL;
  }

  if (s->curlen + 1 >= s->len) {
#ifdef __cplusplus
    newbase = (char *)sexp_realloc(s->base,
                                   s->len+cstring_growsize+1,
                                   s->len);
#else
    newbase = sexp_realloc(s->base,
                           s->len+cstring_growsize+1,
                           s->len);
#endif

    /* do NOT destroy s anymore.  if realloc fails, the original data is
       still valid, so just report the error to sexp_errno and return NULL.
    */
    if (newbase == NULL) {
      sexp_errno = SEXP_ERR_MEMORY;      
      return NULL;
    }

    s->len += cstring_growsize+1;
    s->base = newbase;
  }

  s->base[s->curlen] = a;
  s->curlen++;
  s->base[s->curlen] = 0;
  return s;
}

CSTRING *strim(CSTRING *s) {
  char *newbase;

  if (s == NULL) {
    return NULL;
  }

  /* no trimming necessary? */
  if (s->len == s->curlen+1) {
    return s;
  }

#ifdef __cplusplus
  newbase = (char *)sexp_realloc(s->base,
                                 s->curlen+1,
                                 s->len);
#else
  newbase = sexp_realloc(s->base,
                         s->curlen+1,
                         s->len);
#endif

  /* do NOT destroy s anymore.  if realloc fails, the original data is
     still valid, so just report the error to sexp_errno and return NULL.
  */
  if (newbase == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;      
    return NULL;
  }

  s->len = s->curlen+1;
  s->base = newbase;

  return s;
}

char *toCharPtr(CSTRING *s) {
  if (s == NULL) return NULL;
  return s->base;
}

void sempty(CSTRING *s) {
  if (s == NULL) return;
  s->curlen = 0;
}

void sdestroy(CSTRING *s) {
  if (s == NULL) return;
  sexp_free(s->base,s->len);
  sexp_free(s,sizeof(CSTRING));
}
