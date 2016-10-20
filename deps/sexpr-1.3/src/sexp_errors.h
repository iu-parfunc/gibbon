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
#ifndef __SEXP_ERRORS_H__
#define __SEXP_ERRORS_H__

/**
 * \file sexp_errors.h
 *
 * \brief Error conditions are enumerated here along with any routines for
 *        translating error codes to human readable messages.
 */

/**
 * Error codes used by the library are defined in this enumeration.  They
 * are either used as values for the error field within the continuation
 * structures, or as return values for functions with a return type of
 * sexp_errcode_t.
 */
typedef enum {
  /**
   * no error.
   */
  SEXP_ERR_OK = 0,

  /**
   * memory error.  malloc/realloc/calloc failures may result in this error
   * code.  this can either result from the system calls failing, or in the
   * limited memory mode of the library, the memory limit being exceeded.
   * In limited memory mode, if this error condition is present, one should
   * check the memory limits that were in place during the erroneous call.
   */
  SEXP_ERR_MEMORY,

  /**
   * badly formed expression.  Missing, misplaced, or mismatched parenthesis
   * will result in this error.
   */
  SEXP_ERR_BADFORM,

  /**
   * a sexp_t that is inconsistent will result in this error code.  An example
   * is a SEXP_BASIC sexp_t with a null val field but a non-zero val_used
   * value.  Similar cases exist for SEXP_DQUOTE, SQUOTE, and BINARY types.
   * This value is also used in the parser to flag a case where an inlined
   * binary block is given a negative length.
   */
  SEXP_ERR_BADCONTENT,

  /**
   * if a null string is passed into the parser, this error occurs.
   */
  SEXP_ERR_NULLSTRING,

  /**
   * general IO related errors, such as failure of fopen().  these are
   * basically non-starters with respect to getting the IO routines going.
   */
  SEXP_ERR_IO,

  /**
   * I/O routines that return NULL may simply have nothing to read.  This is
   * sometimes an error condition if the io wrapper continuation contains a
   * partially complete s-expression, but nothing more is present (yet) on the
   * file descriptor.
   */
  SEXP_ERR_IO_EMPTY,

  /**
   * when running the library under limited memory (ie, _SEXP_LIMIT_MEMORY_
   * defined at build time), this error will be produced when the memory
   * limit is exceeded.
   */
  SEXP_ERR_MEM_LIMIT,

  /**
   * buffer for unparsing is full.
   */
  SEXP_ERR_BUFFER_FULL,

  /**
   * routines that take parameters such as memory limits, growth sizes, or
   * default sizes, can produce this error if a bad value has been passed in.
   * this error usually will indicate that the parameters were bad and the
   * default values were used instead (ie, it is non-fatal.).
   */
  SEXP_ERR_BAD_PARAM,

  /**
   * bad stack state encountered.
   */
  SEXP_ERR_BAD_STACK,

  /**
   * unknown parser state
   */
  SEXP_ERR_UNKNOWN_STATE,

  /**
   * parsing is incomplete and need more data to complete it.
   */
  SEXP_ERR_INCOMPLETE,

  /**
   * this error code indicates that an atom was created with
   * the incorrect constructor.  For example, attempting to
   * create a binary mode atom with the new_sexp_atom
   * constructor intended for text atoms will cause this to
   * be set.
   */
  SEXP_ERR_BAD_CONSTRUCTOR

} sexp_errcode_t;

#endif /* __SEXP_ERRORS_H__ */
