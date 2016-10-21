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
 * cstring.h : c string library to make Ron happy.  Wrapper around plain
 * C strings that handles automatically growing the string as data is
 * concattenated to the end.  (note: this is an improved version of cstring
 * from supermon.  Migrate it into that library eventually... )
 *
 * -matt sottile
 */
#ifndef __CSTRING_H__
#define __CSTRING_H__

#include <stdlib.h>

/**
 * Structure wrapping the character pointer and size counters (allocated vs.
 * actual used).
 */
typedef struct __cstring {
  /**
   * Base address of the string.
   */
  char *base; 

  /**
   * Size of the memory allocated and pointed to by the base pointer.
   */
  size_t len;

  /**
   * Current size of the string stored in the buffer.  len >= curlen
   * always, and when len < curlen would be true after a concat operation,
   * we realloc bigger space to keep len >= curlen.
   */
  size_t curlen; 
} CSTRING;

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

  /**
   * Set the growth size.  Values less than one are ignored.
   */
  void sgrowsize(size_t s);
  
  /**
   * Allocate a new CSTRING of the given size.
   * A NULL return value indicates that something went wrong and that
   * sexp_errno should be checked for the cause.
   */
  CSTRING *snew(size_t s);
  
  /**
   * Concatenate the second argument to the CSTRING passed in the first.
   * The second argument must be a pointer to a null terminated string.
   * A NULL return value indicates that something went wrong and that
   * sexp_errno should be checked for the cause.  The contents of s are
   * left alone.  As such, the caller should check the pointer returned
   * before overwriting the value of s, as this may result in a memory
   * leak if an error condition occurs.
   */
  CSTRING *sadd(CSTRING *s, char *a);
  
  /**
   * Append a character to the end of the CSTRING.
   * A NULL return value indicates that something went wrong and that
   * sexp_errno should be checked for the cause.  The contents of s are
   * left alone.  As such, the caller should check the pointer returned
   * before overwriting the value of s, as this may result in a memory
   * leak if an error condition occurs.
   */
  CSTRING *saddch(CSTRING *s, char a);
  
  /**
   * Trim the allocated memory to precisely the string length plus one char
   * to hold the null terminator
   * A NULL return value indicates that something went wrong and that
   * sexp_errno should be checked for the cause.  The contents of s are
   * left alone.  As such, the caller should check the pointer returned
   * before overwriting the value of s, as this may result in a memory
   * leak if an error condition occurs.
   */
  CSTRING *strim(CSTRING *s);
  
  /**
   * Return the base pointer of the CSTRING.  NULL either means the base
   * pointer was null, or the CSTRING itself was NULL.
   */
  char *toCharPtr(CSTRING *s);
  
  /**
   * Set the current length to zero, effectively dumping the string without
   * deallocating it so we can use it later without reallocating any memory.
   */
  void sempty(CSTRING *s);
  
  /**
   * Destroy the CSTRING struct and the data it points at.
   */
  void sdestroy(CSTRING *s);
  
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __CSTRING_H__ */
