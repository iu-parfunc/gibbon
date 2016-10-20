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
 * \file faststack.h
 *
 * \brief Implementation of a fast stack with smart memory management.
 */
#ifndef __FASTSTACK_H__
#define __FASTSTACK_H__

/**
 * Structure representing a single level in the stack.  Has a pointer to the
 * level above and below itself and a pointer to a generic blob of data
 * associated with this level.
 */
typedef struct stack_level {
  /**
   * Pointer to the level above this one.  If NULL, then this level is the
   * top of the stack.  If above is non-NULL, this level *may* be the top,
   * but all that can be guaranteed is that there are other allocated
   * but potentially unused levels above this one.
   */
  struct stack_level *above;

  /**
   * Pointer to the level below this one.  If NULL, then this level is the
   * bottom.
   */
  struct stack_level *below;

  /**
   * Pointer to some data associated with this level.  User is responsible
   * for knowing what to cast the \c void \c * pointer into.
   */
  void *data;
} stack_lvl_t;

/**
 * Wrapper around the stack levels - keeps a pointer to the current top and
 * bottom of the stack and a count of the current height.  This allows the top
 * to have non-null above pointer resulting from previously allocated stack
 * levels that may be recycled later without \c malloc overhead.
 */
typedef struct stack_wrapper {
  /**
   * The top of the stack.  If this is NULL, the stack is empty.
   */
  stack_lvl_t *top;
  
  /**
   * The bottom of the stack.  If this is NULL, the stack is empty.
   */
  stack_lvl_t *bottom;

  /**
   * The current height of the stack, in terms of allocated and used levels.
   */
  int height;
} faststack_t;

/** functions **/

/* this is for C++ */
#ifdef __cplusplus
extern "C" {
#endif

  /**
   * Return a pointer to an empty stack structure.  If the return value is
   * NULL, one should check sexp_errno to determine why.
   */
  faststack_t *make_stack();

  /**
   * Given a stack structure, destroy it and free all of the stack levels.
   * <B>Important note</B> : This function <I>does not</I> free the data
   * pointed to from each level of the stack - the user is responsible
   * for freeing this data themselves before calling this function to
   * prevent memory leakage.
   */
  void destroy_stack(faststack_t *s);

  /**
   * Given a stack, push a new level on referring to the data pointer.
   * If a new level cannot be allocated, NULL is returned and sexp_errno
   * is set with the appropriate error condition.  Memory allocation errors
   * will result in SEXP_ERR_MEMORY, while a null stack will result in
   * SEXP_ERR_BAD_STACK.
   */
  faststack_t *push(faststack_t *cur_stack, void *data);

  /**
   * Given a stack, pop a level off and return a pointer to that level.
   * The user is responsible for extracting the data, but the stack_lvl_t
   * structures pointed to from the level (above and below) should be left
   * alone.  If NULL is returned, either the stack contained nothing, or
   * the incoming stack s was NULL.  Consult sexp_errno to determine which
   * was the case -- SEXP_ERR_BAD_STACK indicates a null stack was passed in.
   */
  stack_lvl_t *pop(faststack_t *s);
  
/* this is for C++ */
#ifdef __cplusplus
}
#endif

/**
 * Given a stack \a s, examine the data pointer at the top.
 */
#define top_data(s) (s->top->data)

/**
 * Given a stack \a s, check to see if the stack is empty or not.  Value
 * is boolean true or false.
 */
#define empty_stack(s) (s->top == NULL)

#endif /* __FASTSTACK_H__ */
