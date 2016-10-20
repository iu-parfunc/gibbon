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
#ifndef __SEXP_OPS_H__
#define __SEXP_OPS_H__

/**
 * \file sexp_ops.h
 *
 * \brief A collection of useful operations to perform on s-expressions.
 *
 * A set of routines for operating on s-expressions.  
 */

#include "sexp.h"

#ifdef __cplusplus
extern "C" {
#endif

  /*========*/
  /* MACROS */
  /*========*/
  
  /**
   * Return the head of a list \a s by reference, not copy.
   */
#define hd_sexp(s) ((s)->list)
  
  /**
   * Return the tail of a list \a s by reference, not copy.
   */
#define tl_sexp(s) ((s)->list->next) 
  
  /**
   * Return the element following the argument \a s.
   */
#define next_sexp(s) ((s)->next)
  
  /**
   * Reset the continuation \a c by setting the \c lastPos pointer to
   * \c NULL.
   */
#define reset_pcont(c) ((c)->lastPos = NULL)

  /**
   * Find an atom in a sexpression data structure and return a pointer to
   * it.  Return NULL if the string doesn't occur anywhere as an atom. 
   * This is a depth-first search algorithm.
   *
   * \param name   Value to search for.
   * \param start  Root element of the s-expression to search from.
   * \return       If the value is found, return a pointer to the first
   *               occurrence in a depth-first traversal.  NULL if not found.
   */
  sexp_t *find_sexp(const char *name, sexp_t *start);
    
  /**
   * Breadth first search for s-expressions.  Depth first search will find
   * the first occurance of a string in an s-expression by basically finding
   * the earliest occurance in the string representation of the expression
   * itself.  Breadth first search will find the first occurance of a string
   * in relation to the structure of the expression itself (IE: the instance
   * with the lowest depth will be found).
   * 
   * \param name  Value to search for.
   * \param start Root element of the s-expression to search from.
   * \return      If the value is found, return a pointer to the first
   *              occurrence in a breadth-first traversal.  NULL if not found.
   */
  sexp_t *bfs_find_sexp(const char *name, sexp_t *start);

  /**
   * Given an s-expression, determine the length of the list that it encodes.
   * A null expression has length 0.  An atom has length 1.  A list has
   * length equal to the number of sexp_t elements from the list head
   * to the end of the ->next linked list from that point.
   *
   * \param sx S-expression input.
   * \return   Number of sexp_t elements at the same level as sx, 0 for
   *           NULL, 1 for an atom.
   */
  int sexp_list_length(const sexp_t *sx);
  
  /**
   * Copy an s-expression.  This is a deep copy - so the resulting s-expression
   * shares no pointers with the original.  The new one can be changed without
   * damaging the contents of the original.
   *
   * \param sx S-expression to copy.
   * \return   A pointer to a copy of sx.  This is a deep copy, so no memory
   *           is shared between the original and the returned copy.
   */
  sexp_t *copy_sexp(const sexp_t *sx);
  
#ifdef __cplusplus
}
#endif

#endif /* __SEXP_OPS_H__ */
