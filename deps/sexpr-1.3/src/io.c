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
#include <fcntl.h>
#ifndef WIN32
# include <unistd.h>
#else
# define ssize_t int
# include <io.h>
# include <sys/types.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sexp.h"

/**
 * initialize an io-wrapper
 */
sexp_iowrap_t *init_iowrap(int fd) {
  sexp_iowrap_t *iow;

#ifdef __cplusplus
  iow = (sexp_iowrap_t *)sexp_calloc(1,sizeof(sexp_iowrap_t));
#else
  iow = sexp_calloc(1,sizeof(sexp_iowrap_t));
#endif

  if (iow == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  iow->cc = NULL;
  iow->fd = fd;
  iow->cnt = 0;
  iow->buf[0] = '\0';

  return iow;
}

/**
 *
 */
void destroy_iowrap(sexp_iowrap_t *iow) {
  if (iow == NULL) return; /* idiot */

  destroy_continuation(iow->cc);
  sexp_free(iow, sizeof(sexp_iowrap_t));
}

/**
 *
 */
sexp_t *read_one_sexp(sexp_iowrap_t *iow) {
  sexp_t  *sx = NULL;

  if (iow == NULL) 
    return NULL;

  /* check if we have more to parse from the continuation. */
  if (iow->cc != NULL && iow->cc->lastPos != NULL) {
    iow->cc = cparse_sexp(iow->buf, iow->cnt, iow->cc);

    if (iow->cc == NULL) return NULL; /* cparse_sexp set sexp_errno */
    if (iow->cc->last_sexp != NULL) {
      sx = iow->cc->last_sexp;
      iow->cc->last_sexp = NULL;
      return sx;
    }
    iow->cnt = 0;
  }

  if (iow->cnt == 0) {
    iow->cnt = (size_t) read(iow->fd,iow->buf,BUFSIZ);

    if (iow->cnt == 0) {
      sexp_errno = SEXP_ERR_IO_EMPTY;
      return NULL;
    }
  }

  iow->cc = cparse_sexp(iow->buf,iow->cnt,iow->cc);

  while (iow->cc->last_sexp == NULL) {
    if (iow->cc->error != SEXP_ERR_OK) {
      sexp_errno = iow->cc->error;
      return NULL;
    }

    iow->cnt = (size_t) read(iow->fd,iow->buf,BUFSIZ);

    if (iow->cnt == 0) {
      sexp_errno = SEXP_ERR_IO_EMPTY;
      return NULL;
    }

    iow->cc = cparse_sexp(iow->buf,iow->cnt,iow->cc);
    iow->cnt = 0;
  }

  sx = iow->cc->last_sexp;
  iow->cc->last_sexp = NULL;

  return sx;
}
