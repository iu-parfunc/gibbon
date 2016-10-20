/**
@Cond IGNORE

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sexp.h"
#include "faststack.h"

/*
 * global error code that can be set by sexp library calls.  default
 * is SEXP_ERR_OK.
 */
sexp_errcode_t sexp_errno = SEXP_ERR_OK;

void reset_sexp_errno() {
	sexp_errno = SEXP_ERR_OK;
}

/**
 * Recursively walk an s-expression and free it.
 */
void
destroy_sexp (sexp_t * s)
{
  if (s == NULL)
    return;

  if (s->ty == SEXP_LIST) {
    destroy_sexp (s->list);
  } else if (s->ty == SEXP_VALUE) {    
    if (s->aty == SEXP_BINARY && s->bindata != NULL) {
      sexp_free(s->bindata, s->binlength);     
    } else if (s->val != NULL) {
      sexp_free(s->val, s->val_allocated);
    }
  }

  s->val = NULL;
  s->bindata = NULL;

  destroy_sexp (s->next);

  s->next = s->list = NULL;

  sexp_t_deallocate(s);
}

/**
 * Iterative method to walk sx and turn it back into the string
 * representation of the s-expression.  Fills the buffer.
 */
int
print_sexp (char *buf, size_t size, const sexp_t * sx)
{
  int retval;
  size_t sz;
  char *b = buf, *tc;
  size_t left = size;
  int depth = 0;
  faststack_t *stack;
  stack_lvl_t *top;
  sexp_t *tdata;
  sexp_t *fakehead;
  sexp_t tmp;

  if (sx == NULL) {
	buf[0] = '\0';
	return 0;
  }

  tmp = *sx;
  tmp.next = tmp.list = NULL;

  fakehead = copy_sexp(&tmp);

  if (fakehead == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return -1;
  }

  fakehead->list = sx->list;
  fakehead->next = NULL; /* this is the important part of fakehead */

  stack = make_stack ();
  if (stack == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    sexp_t_deallocate(fakehead);
    return -1;
  }

  push (stack, fakehead);

  while (stack->top != NULL)
    {
      top = stack->top;
      tdata = (sexp_t *) top->data;

      if (tdata == NULL)
	{
	  pop (stack);

	  if (depth > 0)
	    {
	      b[0] = ')';
	      b++;
	      left--;
	      depth--;
	      if (left == 0)
		{
		  sexp_errno = SEXP_ERR_BUFFER_FULL;
		  break;
		}
	    }

	  if (stack->top == NULL)
	    break;

	  top = stack->top;
	  top->data = ((sexp_t *) top->data)->next;
	  if (top->data != NULL)
	    {
	      b[0] = ' ';
	      b++;
	      left--;
	      if (left == 0)
		{
		  sexp_errno = SEXP_ERR_BUFFER_FULL;
		  break;
		}
	    }
	}
      else if (tdata->ty == SEXP_VALUE)
	{
	  if (tdata->aty == SEXP_DQUOTE)
	    {
	      b[0] = '\"';
	      b++;
	      left--;
	    }
	  else if (tdata->aty == SEXP_SQUOTE)
	    {
	      b[0] = '\'';
	      b++;
	      left--;
	    }

          if (tdata->aty != SEXP_BINARY && tdata->val_used > 0) {
	    tc = tdata->val;
            /* copy value into string */
            while (tc[0] != 0 && left > 0)
              {
                /* escape characters that need escaping. */
                if ((tc[0] == '\"' || tc[0] == '\\') &&
                    tdata->aty == SEXP_DQUOTE)
                  {
                    b[0] = '\\';
                    b++;
                    left--;
                    if (left == 0) break;
                  }
                
                b[0] = tc[0];
                b++;
                tc++;
                left--;
                if (left == 0)
                  break;
              }
          } else {
            if (left > 3) {
              b[0] = '#'; b[1] = 'b'; b[2] = '#';
              b+=3;
              left-=3;

#ifndef WIN32
              if ((size_t)(sz = snprintf(b,left,"%lu#",(unsigned long)tdata->binlength)) >= left) {
#else
              if ((sz = _snprintf(b,left,"%lu#",tdata->binlength)) >= left) {
#endif
                left = 0;
                break;
              }
              b += sz;
              left -= sz;
              
              if (left < tdata->binlength) {
                left = 0;
                break;
              }

	      if (tdata->binlength > 0) {
		memcpy(b,tdata->bindata,tdata->binlength);
		left -= tdata->binlength;
		b+=tdata->binlength;
	      }

              b[0] = ' ';
              left--;

            } else {
              left = 0; 
              break;
            }
          }

	  if (tdata->aty == SEXP_DQUOTE && left > 0)
	    {
	      b[0] = '\"';
	      b++;
	      left--;
	    }

	  if (left == 0)
	    {
	      sexp_errno = SEXP_ERR_BUFFER_FULL;
	      break;
	    }

	  top->data = ((sexp_t *) top->data)->next;

	  if (top->data != NULL)
	    {
	      b[0] = ' ';
	      b++;
	      left--;
	      if (left == 0)
		{
		  sexp_errno = SEXP_ERR_BUFFER_FULL;
		  break;
		}
	    }
	}
      else if (tdata->ty == SEXP_LIST)
	{
	  depth++;
	  b[0] = '(';
	  b++;
	  left--;
	  if (left == 0)
	    {
	      sexp_errno = SEXP_ERR_BUFFER_FULL;
	      break;
	    }

	  push (stack, tdata->list);
	}
      else
	{
	  sexp_errno = SEXP_ERR_BADCONTENT;
	  destroy_stack (stack);
	  sexp_t_deallocate(fakehead);
	  return -1;
	}

    }
  while (depth != 0)
    {
      b[0] = ')';
      b++;
      left--;
      depth--;
      if (left == 0)
	{
	  sexp_errno = SEXP_ERR_BUFFER_FULL;
	  break;
	}
    }

  if (left != 0) {
    b[0] = 0;
    retval = (int) (size-left);
  } else {
    b--;
    b[0] = 0;
    retval = -1;
  }

  destroy_stack (stack);
  sexp_t_deallocate(fakehead);

  return retval;
}

/**
 * Iterative method to walk sx and turn it back into the string
 * representation of the s-expression.  Fills the CSTRING that is
 * passed in.  If *s == NULL (new CSTRING, never used), snew() is called
 * and passed back.  If *s != NULL, *s is used as the CSTRING to print
 * into.  In the last case, the recycled CSTRING must have sempty() called
 * to reset the allocated vs. used counters to make it appear to be empty.
 * the code will assume that sempty() was called by the user!
 */
int
print_sexp_cstr (CSTRING **s, const sexp_t *sx, size_t ss)
{
  int retval;
  char *tc;
  int depth = 0;
  faststack_t *stack;
  stack_lvl_t *top;
  sexp_t *tdata;
  sexp_t *fakehead;
  CSTRING *_s = NULL;
  char sbuf[32];
  unsigned int i;
  sexp_t tmp;

  if (sx == NULL) {
    return -1;
  }

  if (*s == NULL)
    _s = snew(ss);
  else
    _s = *s;

  tmp = *sx;
  tmp.next = tmp.list = NULL;

  fakehead = copy_sexp(&tmp);

  if (fakehead == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return -1;
  }

  fakehead->list = sx->list;
  fakehead->next = NULL; /* this is the important part of fakehead */

  stack = make_stack ();
  if (stack == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    sexp_t_deallocate(fakehead);
    return -1;
  }

  push (stack, fakehead);

  while (stack->top != NULL)
    {
      top = stack->top;
      tdata = (sexp_t *) top->data;

      if (tdata == NULL)
	{
	  pop (stack);

	  if (depth > 0)
	    {
	      _s = saddch(_s, ')');
	      depth--;
	    }

	  if (stack->top == NULL)
	    break;

	  top = stack->top;
	  top->data = ((sexp_t *) top->data)->next;
	  if (top->data != NULL)
	    {
	      _s = saddch(_s, ' ');
	    }
	}
      else if (tdata->ty == SEXP_VALUE)
	{
	  if (tdata->aty == SEXP_DQUOTE)
	    {
	      _s = saddch(_s,'\"');
	    }
	  else if (tdata->aty == SEXP_SQUOTE)
	    {
	      _s = saddch(_s,'\'');
	    }

          if (tdata->aty == SEXP_BINARY) {	    
            sprintf(sbuf,"#b#%lu#",(unsigned long)tdata->binlength);

            _s = sadd(_s,sbuf);

            for (i=0;i<tdata->binlength;i++)
              _s = saddch(_s,tdata->bindata[i]);
            _s = saddch(_s,' ');
          } else {
	    if (tdata->val_used > 0) {
	      tc = tdata->val;
	      
	      /* copy value into string */
	      while (tc[0] != 0)
		{
		  /* escape characters that need escaping. */
		  if ((tc[0] == '\"' ||
		       tc[0] == '\\') && tdata->aty == SEXP_DQUOTE)
		    {
		      _s = saddch(_s,'\\');
		    }
		  
		  _s = saddch(_s,tc[0]);
		  tc++;
		}
	    }
          }

	  if (tdata->aty == SEXP_DQUOTE)
	    {
	      _s = saddch(_s,'\"');
	    }

	  top->data = ((sexp_t *) top->data)->next;

	  if (top->data != NULL)
	    {
	      _s = saddch(_s,' ');
	    }
	}
      else if (tdata->ty == SEXP_LIST)
	{
	  depth++;
	  _s = saddch(_s,'(');
	  push (stack, tdata->list);
	}
      else
	{
	  sexp_errno = SEXP_ERR_BADCONTENT;
	  destroy_stack (stack);
	  sexp_t_deallocate(fakehead);
	  return -1;
	}

    }
  while (depth != 0)
    {
      _s = saddch(_s,')');
      depth--;
    }

  *s = _s;
  if (_s == NULL)
    retval = 0;
  else
    retval = (int) _s->curlen;

  destroy_stack (stack);
  sexp_t_deallocate(fakehead);

  return retval;
}

/**
 * Allocate a new sexp_t element representing a list.
 */
sexp_t *new_sexp_list(sexp_t *l) {
  sexp_t *sx = sexp_t_allocate();

  if (sx == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  sx->ty = SEXP_LIST;

  sx->list = l;
  sx->next = NULL;

  sx->val = NULL;
  sx->val_used = sx->val_allocated = 0;

  return sx;
}

/**
 * allocate a new sexp_t element representing a raw binary value
 */
sexp_t *new_sexp_binary_atom(char *data, size_t binlength) {
  sexp_t *sx = sexp_t_allocate();

  if (sx == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  sx->ty = SEXP_VALUE;
  sx->next = sx->list = NULL;
  sx->aty = SEXP_BINARY;
  sx->bindata = data;
  sx->binlength = binlength;
  sx->val = NULL;
  sx->val_used = sx->val_allocated = 0;

  return sx;
}

/**
 * allocate a new sexp_t element representing a value 
 */
sexp_t *new_sexp_atom(const char *buf, size_t bs, atom_t aty) {
  sexp_t *sx = NULL;

  if (aty == SEXP_BINARY) {
    sexp_errno = SEXP_ERR_BAD_CONSTRUCTOR;
    return NULL;
  }

  sx = sexp_t_allocate();

  if (sx == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  sx->ty = SEXP_VALUE;
  sx->aty = aty;

#ifdef __cplusplus
  sx->val = (char *)sexp_malloc(sizeof(char)*(bs+1));
#else
  sx->val = sexp_malloc(sizeof(char)*(bs+1));
#endif

  if (sx->val == NULL) {
    sexp_t_deallocate(sx);
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  sx->val_used = sx->val_allocated = bs+1;

  strcpy(sx->val,buf);

  sx->list = sx->next = NULL;

  return sx;
}
