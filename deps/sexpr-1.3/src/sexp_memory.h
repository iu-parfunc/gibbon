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
#ifndef __SEXP_MEMORY_H__
#define __SEXP_MEMORY_H__

/**
 * \file sexp_memory.h
 *
 * \brief Wrappers around basic memory allocation/deallocation routines to
 *        allow memory usage limiting.  Only enabled if _SEXP_LIMIT_MEMORY_
 *        is defined when building the library, otherwise the routines
 *        are defined to be the standard malloc/calloc/realloc/free 
 *        functions.
 */


#ifdef _SEXP_LIMIT_MEMORY_

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * \defgroup memory Memory management routines.
   */

  /**
   * \ingroup memory
   * Wrapper around malloc, will check and increment memory usage
   * counters if space is available.  Returns NULL if no memory left
   * to use, otherwise returns whatever malloc returns.  
   * Due to the fact that NULL could mean either the memory limit was exceeded
   * or the system call returned NULL, the user must check sexp_errno to
   * determine the root cause.
   */
  void *sexp_malloc(size_t size);

  /**
   * \ingroup memory 
   * Wrapper around calloc, will check and increment memory usage
   * counters if space is available.  Returns NULL if no memory left
   * to use, otherwise returns whatever calloc returns
   * Due to the fact that NULL could mean either the memory limit was exceeded
   * or the system call returned NULL, the user must check sexp_errno to
   * determine the root cause.
   */
  void *sexp_calloc(size_t count, size_t size);

  /**
   * \ingroup memory
   * Wrapper around free.  Instead of trusting sizeof(ptr) to return the
   * proper value, we explicitly pass the size of memory associated with
   * ptr.  Decrements memory usage counter and frees ptr.
   */
  void sexp_free(void *ptr, size_t size);

  /**
   * \ingroup memory
   * Wrapper around realloc.  Instead of trusting sizeof(ptr) to return the
   * proper value, we explicitly pass the size of memory associated with
   * ptr as the oldsize.  Increments the memory usage counter by 
   * (size-oldsize) if enough space available for realloc.  
   * Returns NULL if no memory left to use, otherwise returns whatever 
   * realloc returns.
   * Due to the fact that NULL could mean either the memory limit was exceeded
   * or the system call returned NULL, the user must check sexp_errno to
   * determine the root cause.
   */
  void *sexp_realloc(void *ptr, size_t size, size_t oldsize);

  /**
   * \ingroup memory
   * Return the memory limit imposed by the library if memory limiting was
   * enabled at compile time.
   */
  size_t get_sexp_max_memory();

  /**
   * \ingroup memory
   * Return the amount of memory used.
   */
  size_t get_sexp_used_memory();

  /**
   * \ingroup memory
   * Set the memory limit if memory limiting was enabled.  If the new value
   * is zero or less, -1 is returned and sexp_errno is set.  Similarly, if 
   * the new value is less than the current amount used by the library,
   * -1 is returned and sexp_errno is set.  If the new value is valid, the
   * new value is returned.
   */
  int set_sexp_max_memory(size_t newsize);

#ifdef __cplusplus
}
#endif

#else

/**
 * \ingroup memory
 * _SEXP_LIMIT_MEMORY_ not defined.  This is a macro that maps to calloc().
 */
#define sexp_calloc(count,size) calloc(count,size)

/**
 * \ingroup memory
 * _SEXP_LIMIT_MEMORY_ not defined.  This is a macro that maps to malloc().
 */
#define sexp_malloc(size) malloc(size)

/**
 * \ingroup memory
 * _SEXP_LIMIT_MEMORY_ not defined.  This is a macro that maps to free().
 */
#define sexp_free(ptr,size) free(ptr)

/**
 * \ingroup memory
 * _SEXP_LIMIT_MEMORY_ not defined.  This is a macro that maps to realloc().
 */
#define sexp_realloc(ptr,size,oldsize) realloc((ptr),(size))

#endif

#endif /* __SEXP_MEMORY_H__ */
