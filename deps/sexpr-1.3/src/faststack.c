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
 * faststack.c : implementation of fast stack.
 *
 * matt sottile / matt@lanl.gov
 */
#include <stdlib.h>
#include <stdio.h>
#include "faststack.h"
#include "sexp.h"

/**
 * create an empty stack.
 */
faststack_t *
make_stack ()
{
  faststack_t *s;

#ifdef __cplusplus
  s = (faststack_t *)sexp_malloc (sizeof (faststack_t));
#else
  s = sexp_malloc (sizeof (faststack_t));
#endif

  if (s == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  s->top = s->bottom = NULL;
  s->height = 0;

  return s;
}

/**
 * free all levels of a stack
 */
void
destroy_stack (faststack_t * s)
{
  stack_lvl_t *sl;

  /* return if stack is null.  no error condition - just return. */
  if (s == NULL)
    return;

  /* start at the bottom */
  sl = s->bottom;

  /* if bottom is null, no levels to free. just free stack itself then. */
  if (sl == NULL) {
    sexp_free(s, sizeof(faststack_t));
    return;
  }

  /* go up to the top of the allocated stack */
  while (sl->above != NULL)
    sl = sl->above;

  /* until we get to the bottom (where below is null), free the data
     at each level and the level itself. */
  while (sl->below != NULL)
    {
      sl = sl->below;
      sexp_free (sl->above, sizeof(stack_lvl_t));
    }

  /* free the bottom level */
  sexp_free (sl, sizeof(stack_lvl_t));

  /* free the stack wrapper itself. */
  sexp_free (s, sizeof(faststack_t));
}

/**
 * push a level onto the cur_stack.  reuse levels that have
 * been previously allocated, allocate a new one if none
 * are available.
 */
faststack_t *
push (faststack_t * cur_stack, void *data)
{
  stack_lvl_t *top, *tmp;

  if (cur_stack == NULL) {
    sexp_errno = SEXP_ERR_BAD_STACK;
    return NULL;
  }

  top = cur_stack->top;

  /* if top isn't null, try to push above it. */
  if (top != NULL)
    {
      /* if above isn't null, set the stack top to it and set the
         data */
      if (top->above != NULL)
	{
	  top = cur_stack->top = cur_stack->top->above;
	  top->data = data;
	}
      else
	{
	  /* otherwise, allocate a new level. */

#ifdef __cplusplus
	  tmp = top->above = (stack_level *)sexp_malloc (sizeof (stack_lvl_t));
#else
	  tmp = top->above = sexp_malloc (sizeof (stack_lvl_t));
#endif

          if (tmp == NULL) {
	    sexp_errno = SEXP_ERR_MEMORY;
	    return NULL;
	  }

	  tmp->below = cur_stack->top;
	  tmp->above = NULL;
	  cur_stack->top = tmp;
	  tmp->data = data;
	}
    }
  else
    {
      if (cur_stack->bottom != NULL)
	{
	  cur_stack->top = cur_stack->bottom;
	  cur_stack->top->data = data;
	}
      else
	{
#ifdef __cplusplus
	  tmp = cur_stack->top =
            (stack_lvl_t *)sexp_malloc (sizeof (stack_lvl_t));
#else
	  tmp = cur_stack->top = sexp_malloc (sizeof (stack_lvl_t));
#endif
	  if (tmp == NULL) {
	    sexp_errno = SEXP_ERR_MEMORY;
	    return NULL;
	  }

	  cur_stack->bottom = tmp;
	  tmp->above = NULL;
	  tmp->below = NULL;
	  tmp->data = data;
	}
    }

  cur_stack->height++;

  return cur_stack;
}

/**
 * pop the top of the stack, return the stack level that was
 * popped of.
 */
stack_lvl_t *
pop (faststack_t * s)
{
  stack_lvl_t *top;

  if (s == NULL) {
    sexp_errno = SEXP_ERR_BAD_STACK;
    return NULL;
  }

  top = s->top;

  /* if stack top isn't null, set the top pointer to the next
     level down and return the old top. */
  if (top != NULL && s->height > 0)
    {
      s->top = s->top->below;
      s->height--;
    }
  else
    {
      if (s->height < 1) return NULL;
    }

  return top;
}
