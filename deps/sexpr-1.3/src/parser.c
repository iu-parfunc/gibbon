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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sexp.h"
#include "faststack.h"

/*
 * constants related to atom buffer sizes and growth.
 */
static size_t sexp_val_start_size = 256;
static size_t sexp_val_grow_size  = 64;

/*
 * Function for tuning growth parameters.
 */
sexp_errcode_t set_parser_buffer_params(size_t ss, size_t gs) {
  if (ss > 0)
    sexp_val_start_size = ss;
  else 
    return SEXP_ERR_BAD_PARAM;

  if (gs > 0)
    sexp_val_grow_size = gs;
  else
    return SEXP_ERR_BAD_PARAM;

  return SEXP_ERR_OK;
}

/**
 * this structure is pushed onto the stack so we can keep track of the
 * first and last elements in a list.
 * !!!!DON'T USE THESE OUTSIDE THIS FILE!!!!
 */
typedef struct parse_stack_data
{
  sexp_t *fst, *lst;
}
parse_data_t;

/**
 * parse_data_t stack - similar malloc prevention to sexp_t_cache.
 */
#ifndef _NO_MEMORY_MANAGEMENT_
faststack_t *pd_cache;
#endif

/** 
 * The global <I>sexp_t_cache</I> is a faststack implementing a cache of
 * pre-alloced s-expression element entities.  Odds are a user should never 
 * touch this.  If you do, you're on your own.  This is used internally by 
 * the parser and related code to store unused but allocated sexp_t elements.
 * This should be left alone and manipulated only by the sexp_t_allocate and
 * sexp_t_deallocate functions.  Touching the stack is bad.  
 */ 
#ifndef _NO_MEMORY_MANAGEMENT_
faststack_t *sexp_t_cache;
#endif

/**
 * sexp_t allocation
 */
#ifdef _NO_MEMORY_MANAGEMENT_
sexp_t *
sexp_t_allocate(void) {
  sexp_t *sx = sexp_calloc(1, sizeof(sexp_t));
  if (sx == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }

  return(sx);
}
#else
sexp_t *
sexp_t_allocate(void) {
  sexp_t *sx;
  stack_lvl_t *l;

  if (sexp_t_cache == NULL) {
    sexp_t_cache = make_stack();
    if (sexp_t_cache == NULL) {
      sexp_errno = SEXP_ERR_MEMORY;
      return NULL;
    }

#ifdef __cplusplus
    sx = (sexp_t *)sexp_malloc(sizeof(sexp_t));
#else
    sx = sexp_malloc(sizeof(sexp_t));
#endif
    if (sx == NULL) {
      sexp_errno = SEXP_ERR_MEMORY;
      return NULL;
    }

    sx->next = sx->list = NULL;
  } else {
    if (empty_stack(sexp_t_cache)) {
#ifdef __cplusplus
      sx = (sexp_t *)sexp_malloc(sizeof(sexp_t));
#else
      sx = sexp_malloc(sizeof(sexp_t));
#endif
      if (sx == NULL) {
	sexp_errno = SEXP_ERR_MEMORY;
	return NULL;
      }

      sx->next = sx->list = NULL;
    } else {
      l = pop(sexp_t_cache);
      sx = (sexp_t *)l->data;
    }
  }

  return sx;
}
#endif

/**
 * sexp_t de-allocation
 */
#ifdef _NO_MEMORY_MANAGEMENT_
void
sexp_t_deallocate(sexp_t *s) {
  if (s->ty == SEXP_VALUE && s->val != NULL) {
    sexp_free(s->val,s->val_allocated);
  }

  sexp_free(s,sizeof(sexp_t));
}
#else
void
sexp_t_deallocate(sexp_t *s) {
  if (s == NULL) return;

  if (sexp_t_cache == NULL) {
    sexp_t_cache = make_stack();
    if (sexp_t_cache == NULL) {

      /**** HOW DO WE GET THE USER TO KNOW SOMETHING HAPPENED? ****/

      sexp_errno = SEXP_ERR_MEMORY;
      if (s->ty == SEXP_VALUE && s->val != NULL) {
	sexp_free(s->val,s->val_allocated);
      }
      sexp_free(s,sizeof(sexp_t));
      return;
    }
  }

  s->list = s->next = NULL;

  if (s->ty == SEXP_VALUE && s->val != NULL) {
    sexp_free(s->val,s->val_allocated);
  }

  s->val = NULL;

  sexp_t_cache = push(sexp_t_cache, s);
}
#endif

/**
 * cleanup the sexp library.  Note this is implemented HERE since we need
 * to know about pd_cache, which is local to this file.
 */
#ifdef _NO_MEMORY_MANAGEMENT_
void sexp_cleanup(void) {
}
#else
void sexp_cleanup(void) {
  stack_lvl_t *l;

  if (pd_cache != NULL) {
    l = pd_cache->top;
    while (l != NULL) {
      sexp_free(l->data,sizeof(parse_data_t));
      l = l->below;
    }
    destroy_stack(pd_cache);
    pd_cache = NULL;
  }

  if (sexp_t_cache != NULL) {
    l = sexp_t_cache->top;
    while (l != NULL) {
      sexp_free(l->data,sizeof(sexp_t));
      l = l->below;
    }
    destroy_stack(sexp_t_cache);
    sexp_t_cache = NULL;
  }
}
#endif

/**
 * allocation
 */
#ifdef _NO_MEMORY_MANAGEMENT_
parse_data_t *
pd_allocate(void) {
  parse_data_t *p = NULL;
  p = sexp_malloc(sizeof(parse_data_t));
  return p;
}
#else
parse_data_t *
pd_allocate(void) {
  parse_data_t *p;
  stack_lvl_t *l;

  if (pd_cache == NULL) {
    pd_cache = make_stack();
    
    if (pd_cache == NULL) {
      sexp_errno = SEXP_ERR_MEMORY;
      return NULL;
    }

#ifdef __cplusplus
    p = (parse_data_t *)sexp_malloc(sizeof(parse_data_t));
#else
    p = sexp_malloc(sizeof(parse_data_t));
#endif

    if (p == NULL) {
      sexp_errno = SEXP_ERR_MEMORY;
      return NULL;
    }

  } else {
    if (empty_stack(pd_cache)) {
#ifdef __cplusplus
      p = (parse_data_t *)sexp_malloc(sizeof(parse_data_t));
#else
      p = sexp_malloc(sizeof(parse_data_t));
#endif

      if (p == NULL) {
	sexp_errno = SEXP_ERR_MEMORY;
	return NULL;
      }
    } else {
      l = pop(pd_cache);
      p = (parse_data_t *)l->data;
    }
  }

  return p;
}
#endif /* _NO_MEMORY_MANAGEMENT_ */

/**
 * de-allocation
 */
#ifdef _NO_MEMORY_MANAGEMENT_
void
pd_deallocate(parse_data_t *p) {
  sexp_free(p, sizeof(parse_data_t));
}
#else
void
pd_deallocate(parse_data_t *p) {
  if (pd_cache == NULL) {
    pd_cache = make_stack();
    if (pd_cache == NULL) {
      sexp_free(p, sizeof(parse_data_t));
      sexp_errno = SEXP_ERR_MEMORY;
      return;
    }
  }

  pd_cache = push(pd_cache, p);
}
#endif /* _NO_MEMORY_MANAGEMENT_ */

/**
 * print the current parsing state based on the contents of the parser
 * continuation.  Useful for error reporting if an error is detected
 * while the current expression being parsed is incomplete.
 */
void print_pcont(pcont_t * pc, char * buf, size_t buflen) {
  char *cur = buf;
  int loc = 0;
  int n;
  stack_lvl_t *lvl;
  parse_data_t *pdata;
  sexp_t *sx;
  
  /* return if either the buffer or continuation are null */
  if (buf == NULL) return;
  if (pc == NULL) return;

  /* if continuation has no stack, return */
  if (pc->stack == NULL) return;

  /* start at the bottom of the stack */
  lvl = pc->stack->bottom;

  /* go until we either run out of buffer space or we hit the
     top of the stack */
  while (loc < buflen-1 && lvl != NULL) {
    /* get the data at the current stack level */
    pdata = (parse_data_t *)lvl->data;

    /* if this is null, we're at a level with nothing added yet */
    if (pdata == NULL) break;

    /* get first fully parsed sexpr for this level. this could be
       any sub-expression, like an atom or a full s-expression */
    sx = pdata->fst;

    /* spin through all of the s-expressions at this level */
    while (sx != NULL) {

      /* if we have a list that has no contents, just add the open
	 paren.  this means we haven't finished this expression and the
	 stack contains it's partial contents.  Just print the open paren
	 and break out so we can pop up the stack. */
      if (sx->ty == SEXP_LIST && sx->list == NULL) {
	cur[0] = '(';
	cur++;
	loc++;
	break;
      } else {
	/* print the fully parsed sub-expression */
	n = print_sexp(cur,buflen-loc,sx);

	/* add a space between this and the next expression.  note that
	   this may induce spaces that were not part of the original
	   expression.  */
	cur[n] = ' ';

	/* increment n to compensate for the space we added */
	n++;

	/* push the pointer into the output buffer forward by n */
	cur += n;

	/* increment counter for location in buffer by n */
	loc += n;
      }

      /* go to next s-expr */
      sx = sx->next;
    }
    
    /* go up to next level in stack */
    lvl = lvl->above;
  }

  /* at this point, all that may remain is a partially parsed string
     that hasn't been turned into a sexpr yet.  attach it to the 
     output string. */
  if (pc->val_used < (buflen-loc)-1) {
    strncpy(cur, pc->val, pc->val_used);
    cur += pc->val_used;
  } else {
    /* don't bother if we're so close to the end of the buffer that
       we can't attach our null terminator. */
    if (buflen-loc > 2) {
      strncpy(cur, pc->val, (buflen-loc)-2);
      cur += (buflen-loc)-2;
    }
  }
  
  /* add null terminator */
  cur[0] = '\0';
}

/**
 * Destroy a continuation by freeing all of its fields that it is responsible
 * for managing, and then free the continuation itself.  This includes internal
 * buffers, stacks, etc..
 */
void
destroy_continuation (pcont_t * pc)
{
  stack_lvl_t *lvl;
  parse_data_t *lvl_data;

  if (pc == NULL) return; /* return if null passed in */

  if (pc->stack != NULL) {
    lvl = pc->stack->top;
    
    /*
     * note that destroy_stack() does not free the data hanging off of the
     * stack.  we have to walk down the stack and do that here. 
     */
    
    while (lvl != NULL) {
      lvl_data = (parse_data_t *)lvl->data;

      /**
       * Seems to have fixed bug with destroying partially parsed
       * expression continuations with the short three lines below.
       */
      if (lvl_data != NULL) {
        lvl_data->lst = NULL;
        destroy_sexp(lvl_data->fst);
        lvl_data->fst = NULL;

        pd_deallocate(lvl_data);
        lvl->data = lvl_data = NULL;
      }

      lvl = lvl->below;
    }
    
    /*
     * stack has no data on it anymore, so we can free it.
     */
    destroy_stack(pc->stack);
    pc->stack = NULL;
  }

  /*
   * free up data used for INLINE_BINARY mode
   */
  if (pc->bindata != NULL) {
    sexp_free(pc->bindata,pc->binexpected);
    pc->bindata = NULL;
  }

  if (pc->val != NULL) {
    sexp_free (pc->val,pc->val_allocated);
    pc->val = NULL;
  }

  sexp_free (pc,sizeof(pcont_t));
}

/* 
 * wrapper around cparse_sexp.  assumes s contains a single, complete,
 * null terminated s-expression.  partial sexps or strings containing more
 * than one will act up.
 */
sexp_t *
parse_sexp (char *s, size_t len)
{
  pcont_t *pc = NULL;
  sexp_t *sx = NULL;

  if (len < 1 || s == NULL) return NULL; /* empty string - return */  

  pc = cparse_sexp (s, len, pc);
  if (pc == NULL)  return NULL; /* assume that cparse_sexp set sexp_errno */
  sx = pc->last_sexp;

  destroy_continuation(pc);

  return sx;
}

pcont_t *
init_continuation(char *str) 
{
  pcont_t *cc;
  /* new continuation... */
#ifdef __cplusplus
  cc = (pcont_t *)sexp_malloc(sizeof(pcont_t));
#else
  cc = sexp_malloc(sizeof(pcont_t));
#endif
  
  if (cc == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    return NULL;
  }
  
  /* allocate atom buffer */
#ifdef __cplusplus
  cc->val = (char *)sexp_malloc(sizeof(char)*sexp_val_start_size);
#else
  cc->val = sexp_malloc(sizeof(char)*sexp_val_start_size);
#endif

  if (cc->val == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    sexp_free(cc,sizeof(pcont_t));
    return NULL;
  }
  
  /* by default we assume a normal parser */
  cc->mode = PARSER_NORMAL;

  cc->val_allocated = sexp_val_start_size;
  cc->val_used = 0;

  cc->bindata = NULL;
  cc->binread = cc->binexpected = 0;

  /* allocate stack */
  cc->esc = 0;
  cc->stack = make_stack();

  if (cc->stack == NULL) {
    sexp_errno = SEXP_ERR_MEMORY;
    sexp_free(cc->val,sizeof(char)*sexp_val_start_size);
    sexp_free(cc,sizeof(pcont_t));
    return NULL;
  }

  cc->sbuffer = str;
  cc->lastPos = NULL;
  cc->state = 1;
  cc->vcur = cc->val;
  cc->depth = 0;
  cc->qdepth = 0;
  cc->squoted = 0;
  cc->event_handlers = NULL;

  return cc;
}

/**
 * Iterative parser.  Wrapper around parse_sexp that is slightly more
 * intelligent and allows users to iteratively "pop" the expressions
 * out of a string that contains a bunch of expressions.
 * Useful if you have a string like "(foo bar)(goo har)(moo mar)" and
 * want to get "(foo bar)", "(goo har)", and "(moo mar)" individually on
 * repeated calls.
 */
sexp_t *
iparse_sexp (char *s, size_t len, pcont_t *cc) {
  pcont_t *pc;
  sexp_t *sx = NULL;
  
  /* 
   * error check.  note that cc must be non-null, as this routine returns
   * a sexp_t .  If cc is null and a new one gets allocated, there is no
   * way to return it.  Thus this call requires cc to be allocated outside
   * the routine.  A null return value should cause sexp_errno to be checked.
   */
  if (cc == NULL) {
    sexp_errno = SEXP_ERR_BAD_PARAM;
    return NULL;
  }
  
  /* call the parser */
  pc = cparse_sexp(s,len,cc);

  if (pc == NULL) return NULL; /* assume cparse_sexp set sexp_errno */
  
  if (cc->last_sexp != NULL) {
    sx = cc->last_sexp;
    cc->last_sexp = NULL;
  }
  
  return sx;
}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

/* TEMPORARY -- THIS WILL GO AWAY WHEN eparse_sexp GETS ROLLED BACK INTO
 * cparse_sexp */
pcont_t *eparse_sexp (char *str, size_t len, pcont_t *lc);

/**
 * Continuation based parser - the guts of the package.
 */
pcont_t *
cparse_sexp (char *str, size_t len, pcont_t *lc)
{
  char *t = NULL;
  char *s = NULL;
  register size_t       binexpected = 0;
  register size_t       binread = 0; 
  register parsermode_t mode = PARSER_NORMAL;
  register size_t       val_allocated = 0;
  register unsigned int squoted = 0;
  register size_t       val_used = 0;
  register unsigned int state = 1;
  register unsigned int depth = 0;
  register unsigned int qdepth = 0;
  register unsigned int elts = 0;
  register unsigned int esc = 0;
  pcont_t *cc = NULL;
  char *val = NULL;
  char *vcur = NULL;
  char *bindata = NULL;
  sexp_t *sx = NULL;
  faststack_t *stack = NULL;
  parse_data_t *data = NULL;
  stack_lvl_t *lvl = NULL;
  char *bufEnd = NULL;
  int keepgoing = 1;
  parser_event_handlers_t *event_handlers = NULL;

  /*** define a macro used for stashing continuation state away ***/
  /** NOTE1: sbuffer is set manually as appropriate. **/
  /** NOTE2: this also sets sexp_errno to the same value as the
             error field in the continuation.  This used to be
             done in iparse_sexp and parse_sexp, but that meant that
             direct callers of cparse_sexp would see inconsistent errors.
             sexp_errno could say one thing, but cc would say the other.
             This has been fixed. **/
#define SAVE_CONT_STATE(err,ls) {      \
  cc->bindata = bindata;               \
  cc->binread = binread;               \
  cc->binexpected = binexpected;       \
  cc->val = val;                       \
  cc->mode = mode;                     \
  cc->squoted = squoted;               \
  cc->val_used = val_used;             \
  cc->val_allocated = val_allocated;   \
  cc->vcur = vcur;                     \
  cc->lastPos = t;                     \
  cc->depth = depth;                   \
  cc->qdepth = qdepth;                 \
  cc->state = state;                   \
  cc->stack = stack;                   \
  cc->esc = esc;                       \
  cc->last_sexp = (ls);                \
  cc->error = (err);                   \
  cc->event_handlers = event_handlers; \
  sexp_errno = (err);                  \
}
  /*** end continuation state saving macro ***/

  /* make sure non-null string */
  if (str == NULL) {
    cc = lc;

    if (cc == NULL) {
      cc = init_continuation(str);
      if (cc == NULL) return NULL; /* sexp_errno was set in call */
    }
    cc->error = SEXP_ERR_NULLSTRING;
    cc->last_sexp = NULL;

    return cc;
  }
  
  /* first, if we have a non null continuation passed in, restore state. */
  if (lc != NULL) {
    /* if the parser mode is events only, call the parser that doesn't
       allocate any elements or stack parts */
    if (lc->mode == PARSER_EVENTS_ONLY) 
      return eparse_sexp(str,len,lc);

    cc = lc;
    binexpected = cc->binexpected;
    binread = cc->binread;
    bindata = cc->bindata;
    val_used = cc->val_used;
    val_allocated = cc->val_allocated;
    squoted = cc->squoted;
    val = cc->val;
    vcur = cc->vcur;
    state = cc->state;
    depth = cc->depth;
    qdepth = cc->qdepth;
    stack = cc->stack;
    esc = cc->esc;
    mode = cc->mode;
    event_handlers = cc->event_handlers;
    s = str;
    if (cc->lastPos != NULL)
      t = cc->lastPos;
    else {
      t = s;
      cc->sbuffer = str;
    }
  } else {
    /* new continuation... */
    cc = init_continuation(str);
    if (cc == NULL) return NULL;

    /* explicitly set mode -- init continuation defaults to PARSER_NORMAL */
    cc->mode = mode;
    val = cc->val;
    
    val_used = cc->val_used;
    val_allocated = cc->val_allocated;

    vcur = val;
    
    /* allocate stack */
    stack = cc->stack;

    /* t is temp pointer into s for current position */
    s = str;
    t = s;
  }
  
  bufEnd = cc->sbuffer+len;

  /* guard for loop - see end of loop for info.  Put it out here in the
     event that we're restoring state from a continuation and need to
     check before we start up. */
  if (state != 15 && t[0] == '\0') keepgoing = 0;

  /*==================*/
  /* main parser loop */
  /*==================*/
  while (keepgoing == 1 && t != bufEnd)
    {
      /* based on the current state in the FSM, do something */
      switch (state)
	{
	case 1:
	  switch (t[0])
	    {
	      /* space,tab,CR,LF considered white space */
	    case '\n':
	    case ' ':
	    case '\t':
	    case '\r':             
	      t++;
	      break;
              /* semicolon starts a comment that extends until a \n is
                 encountered. */
            case ';':
              t++;
              state = 11;
              break;
	      /* enter state 2 for open paren */
	    case '(':
	      state = 2;
	      t++;
              if (event_handlers != NULL && 
                  event_handlers->start_sexpr != NULL)
                event_handlers->start_sexpr();
	      break;
	      /* enter state 3 for close paren */
	    case ')':
	      state = 3;
	      break;              
	      /* begin quoted string - enter state 5 */
	    case '\"':
	      state = 5;
	      /* set cur pointer to beginning of val buffer */
	      vcur = val;
	      t++;
	      break;
	      /* single quote - enter state 7 */
	    case '\'':
	      state = 7;
	      t++;
	      break;
	      /* other characters are assumed to be atom parts */
	    default:
	      /* set cur pointer to beginning of val buffer */
	      vcur = val;

              /** NOTE: the following code originally required a transition
                  to state 4 before processing the first atom character --
                  this required two iterations for the first character
                  of each atom.  merging this into here allows us to process
                  what we already know to be a valid atom character before
                  entering state 4. **/
	      vcur[0] = t[0];
	      if (t[0] == '\\') esc = 1;
	      else esc = 0;
              val_used++;

              if (val_used == val_allocated) {
#ifdef __cplusplus
                val = (char *)sexp_realloc(val,
                                           val_allocated+sexp_val_grow_size,
                                           val_allocated);
#else
                val = sexp_realloc(val,
                                   val_allocated+sexp_val_grow_size,
                                   val_allocated);
#endif

		if (val == NULL) {
		  SAVE_CONT_STATE(SEXP_ERR_MEMORY,NULL);
		  return cc;
		}

                vcur = val + val_used;
                val_allocated += sexp_val_grow_size;
              } else vcur++;

              /* if the atom starts with # and we're in inline
                 binary mode, we need to go to state 12 to start
                 checking for the #b# prefix.  otherwise,
                 if it's not a # or we're just in normal mode,
                 proceed to state 4 as usual. */
              if (t[0] == '#' && mode == PARSER_INLINE_BINARY) {
                state = 12;
              } else {
                state = 4;
              }

              t++;
	      break;
	    }
	  break;
	case 2:
	  /* open paren */
	  depth++;

          sx = sexp_t_allocate();

	  if (sx == NULL) {
	    SAVE_CONT_STATE(SEXP_ERR_MEMORY,NULL);
	    return cc;
	  }

	  elts++;
	  sx->ty = SEXP_LIST;
	  sx->next = NULL;
	  sx->list = NULL;
	  
	  if (stack->height < 1)
	    {
              data = pd_allocate();

	      if (data == NULL) {
		sexp_t_deallocate(sx);
		SAVE_CONT_STATE(SEXP_ERR_MEMORY,NULL);
		return cc;
	      }

	      data->fst = data->lst = sx;
	      push (stack, data);
	    }
	  else
	    {
	      data = (parse_data_t *) top_data (stack);
	      if (data->lst != NULL)
		data->lst->next = sx;
	      else
		data->fst = sx;
	      data->lst = sx;
	    }
	  
          data = pd_allocate();
	  if (data == NULL) {
	    SAVE_CONT_STATE(SEXP_ERR_MEMORY,NULL);
	    return cc;
	  }
	  data->fst = data->lst = NULL;
	  push (stack, data);
	  
	  state = 1;
	  break;
	case 3:
	  /** close paren **/

          /* check for close parens that were never opened. */
          if (depth == 0) {
	    esc = 0;
	    state = 1;
	    SAVE_CONT_STATE(SEXP_ERR_BADFORM,NULL);
	    return cc;
          }

	  t++;
	  depth--;

	  lvl = pop (stack);
	  data = (parse_data_t *) lvl->data;
	  sx = data->fst;
          pd_deallocate(data);
          lvl->data = NULL;

	  if (stack->top != NULL)
	    {
	      data = (parse_data_t *) top_data (stack);
	      data->lst->list = sx;
	    }
	  else
	    {
	      SAVE_CONT_STATE(SEXP_ERR_BAD_STACK, NULL);
	      return cc;
	    }

          if (event_handlers != NULL && 
              event_handlers->end_sexpr != NULL)
            event_handlers->end_sexpr();

	  state = 1;

	  /** if depth = 0 then we finished a sexpr, and we return **/
	  if (depth == 0) {
	    while (stack->top != NULL)
	      {
		lvl = pop (stack);
		data = (parse_data_t *) lvl->data;
		sx = data->fst;
                pd_deallocate(data);
                lvl->data = NULL;
	      }

	    esc = 0;
	    state = 1;
	    SAVE_CONT_STATE(SEXP_ERR_OK, sx);

	    return cc;
	  }
	  break;
	case 4: /** parsing atom **/
	  if (esc == 1 && (t[0] == '\"' || t[0] == '(' ||
			   t[0] == ')' || t[0] == '\'' ||
			   t[0] == '\\')) {
	    vcur--; /* back up to overwrite the \ */
	    vcur[0] = t[0];
	    vcur++;
	    t++;
	    esc = 0;
	    break;
	  }

	  /* look at an ascii table - these ranges are the non-whitespace, non
	     paren and quote characters that are legal in atoms */
	  if (!((t[0] >= '*' && t[0] <= '~') ||
		((unsigned char)(t[0]) > 127) || 
		(t[0] == '!') ||
		(t[0] >= '#' && t[0] <= '&')))
	    {
	      vcur[0] = '\0';
              val_used++;

              sx = sexp_t_allocate();

	      if (sx == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	      elts++;
	      sx->ty = SEXP_VALUE;
              sx->val = val;
              sx->val_allocated = val_allocated;
              sx->val_used = val_used;
	      sx->next = NULL;
              if (squoted != 0)
                sx->aty = SEXP_SQUOTE;
              else
                sx->aty = SEXP_BASIC;

              if (event_handlers != NULL &&
                  event_handlers->characters != NULL)
                event_handlers->characters(sx->val,sx->val_used,sx->aty);

#ifdef __cplusplus
              val = (char *)sexp_malloc(sizeof(char)*sexp_val_start_size);
#else
              val = sexp_malloc(sizeof(char)*sexp_val_start_size);
#endif
	      
	      if (val == NULL) {
		sexp_t_deallocate(sx);
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

              val_allocated = sexp_val_start_size;
              val_used = 0;
              vcur = val;
	      
	      if (!empty_stack (stack))
		{
		  data = (parse_data_t *) top_data (stack);
		  if (data->fst == NULL)
		    {
		      data->fst = data->lst = sx;
		    }
		  else
		    {
		      data->lst->next = sx;
		      data->lst = sx;
		    }
		}
	      else
		{
                  /* looks like this expression was just a basic atom - so
                     return it. */
		  squoted = 0;
		  state = 1;
		  esc = 0;
		  SAVE_CONT_STATE(SEXP_ERR_OK, sx);
                  return cc;
		}

	      switch (t[0]) {
              case ' ':
              case '\t':
              case '\n':
              case '\r':
                /** NOTE: we know whitespace following atom, so spin ahead
                    one and let state 1 do what it needs to for the next
                    character. **/
                state = 1;
                t++;
                squoted = 0;
                break;
              case ')':
                squoted = 0;
                state = 3;
                break;
              default:
                squoted = 0;
                state = 1;
              }
	    }
	  else
	    {
	      vcur[0] = t[0];
	      if (t[0] == '\\') esc = 1;
	      else esc = 0;
              val_used++;

              if (val_used == val_allocated) {
		char *valnew = NULL;
#ifdef __cplusplus
                valnew = (char *)sexp_realloc(val,
					      val_allocated+sexp_val_grow_size,
					      val_allocated);
#else
                valnew = sexp_realloc(val,
				      val_allocated+sexp_val_grow_size,
				      val_allocated);
#endif

		if (valnew == NULL) {
		  SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		  return cc;
		}

		val = valnew;

                vcur = val + val_used;
                val_allocated += sexp_val_grow_size;
              } else vcur++;

	      t++;
	    }
	  break;
	case 5:
	  if (esc == 1 && (t[0] == '\"' ||
			   t[0] == '\'' ||
			   t[0] == '(' ||
			   t[0] == ')' ||
			   t[0] == '\\')) {
	    vcur--;
	    vcur[0] = t[0];
	    vcur++;
            /** NO NEED TO UPDATE VAL COUNTS **/
	    t++;
	    esc = 0;
	  }

	  if (t[0] == '\"')
	    {
	      state = 6;

              if (squoted == 1) {
                vcur[0] = '\"';
                val_used++;
                
                if (val_used == val_allocated) {
		  char *valnew = NULL;

#ifdef __cplusplus
                  valnew = (char *)sexp_realloc(val,
						val_allocated+
						  sexp_val_grow_size,
						val_allocated);
#else
                  valnew = sexp_realloc(val,
					val_allocated+sexp_val_grow_size,
					val_allocated);
#endif
		  
		  if (valnew == NULL) {
		    SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		    return cc;
		  }

		  val = valnew;

                  vcur = val + val_used;
                  val_allocated += sexp_val_grow_size;
                } else vcur++;
              }

              vcur[0] = '\0';

              val_used++;
              sx = sexp_t_allocate();

	      if (sx == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	      elts++;
	      sx->ty = SEXP_VALUE;
              sx->val = val;
              sx->val_used = val_used;
              sx->val_allocated = val_allocated;
	      sx->next = NULL;

              if (squoted == 1) {
                sx->aty = SEXP_SQUOTE;
                squoted = 0;
              } else
                sx->aty = SEXP_DQUOTE;

              if (event_handlers != NULL &&
                  event_handlers->characters != NULL)
                event_handlers->characters(sx->val,sx->val_used,sx->aty);

#ifdef __cplusplus
              val = (char *)sexp_malloc(sizeof(char)*sexp_val_start_size);
#else
              val = sexp_malloc(sizeof(char)*sexp_val_start_size);
#endif
	      
	      if (val == NULL) {
		sexp_t_deallocate(sx);
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

              val_allocated = sexp_val_start_size;
              val_used = 0;
              vcur = val;
	      
	      if (!empty_stack (stack))
		{
		  data = (parse_data_t *) top_data (stack);
		  if (data->fst == NULL)
		    {
		      data->fst = data->lst = sx;
		    }
		  else
		    {
		      data->lst->next = sx;
		      data->lst = sx;
		    }
		}
	      else
		{
                  /* looks like this expression was just a basic double
                     quoted atom - so return it. */
                  t++; /* spin past the quote */

		  squoted = 0;
		  esc = 0;
		  state = 1;
		  SAVE_CONT_STATE(SEXP_ERR_OK, sx);

                  return cc;
		}
	    }
	  else
	    {
	      vcur[0] = t[0];
              val_used++;

              if (val_used == val_allocated) {
		char *valnew = NULL;

#ifdef __cplusplus
                valnew = (char *)sexp_realloc(val,
					      val_allocated+sexp_val_grow_size,
					      val_allocated);
#else
                valnew = sexp_realloc(val,
				      val_allocated+sexp_val_grow_size,
				      val_allocated);
#endif

		if (valnew == NULL) {
		  SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		  return cc;
		}

		val = valnew;

                vcur = val + val_used;
                val_allocated += sexp_val_grow_size;
              } else vcur++;

	      if (t[0] == '\\') { 
                esc = 1;  
	      } else 
                esc = 0;
	    }

	  t++;
	  break;
	case 6:
	  vcur = val;
	  state = 1;
	  break;
	case 7:
	  if (t[0] == '\"')
	    {
	      state = 5;
	      vcur = val;
              t++;

              vcur[0] = '\"';
              val_used++;
              
              if (val_used == val_allocated) {
		char *valnew = NULL;

#ifdef __cplusplus
                valnew = (char *)sexp_realloc(val,
					      val_allocated+sexp_val_grow_size,
					      val_allocated);
#else
                valnew = sexp_realloc(val,
				      val_allocated+sexp_val_grow_size,
				      val_allocated);
#endif
		if (valnew == NULL) {
		  SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		  return cc;
		}
		
		val = valnew;

                vcur = val + val_used;
                val_allocated += sexp_val_grow_size;
              } else vcur++;
              
              squoted = 1;
	    }
	  else if (t[0] == '(')
	    {
	      vcur = val;
	      state = 8;
	    }
	  else
	    {
	      vcur = val;
	      state = 4;
              squoted = 1;
	    }
	  break;
	case 8:
	  if (esc == 0) {
	    if (t[0] == '(')
	      {
		qdepth++;
	      }
	    else if (t[0] == ')')
	      {
		qdepth--;
		state = 9;
	      }
            else if (t[0] == '\"')
              {
                state = 10;
              }
	  } else {
	    esc = 0;
	  }
	  vcur[0] = t[0];
	  if (t[0] == '\\') esc = 1;
	  else esc = 0;
          val_used++;

          if (val_used == val_allocated) {
	    char *valnew = NULL;

#ifdef __cplusplus
            valnew = (char *)sexp_realloc(val,
					  val_allocated+sexp_val_grow_size,
					  val_allocated);
#else
            valnew = sexp_realloc(val,
				  val_allocated+sexp_val_grow_size,
				  val_allocated);
#endif
	    if (valnew == NULL) {
	      SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
	      return cc;
	    }

	    val = valnew;

            vcur = val + val_used;
            val_allocated += sexp_val_grow_size;
          } else vcur++;

	  t++;
          /* let it fall through to state 9 if we know we're transitioning
             into that state */
          if (state != 9)
            break;
	case 9:
	  if (qdepth == 0)
	    {
	      state = 1;
	      vcur[0] = '\0';
              sx = sexp_t_allocate();

	      if (sx == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	      elts++;
	      sx->ty = SEXP_VALUE;
              sx->val = val;
              sx->val_allocated = val_allocated;
              sx->val_used = val_used;
	      sx->next = NULL;
	      sx->aty = SEXP_SQUOTE;

              if (event_handlers != NULL &&
                  event_handlers->characters != NULL)
                event_handlers->characters(sx->val,sx->val_used,sx->aty);

#ifdef __cplusplus
              val = (char *)sexp_malloc(sizeof(char)*sexp_val_start_size);
#else
              val = sexp_malloc(sizeof(char)*sexp_val_start_size);
#endif

	      if (val == NULL) { 
		sexp_t_deallocate(sx);
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

              val_allocated = sexp_val_start_size;
              val_used = 0;
              vcur = val;
	      
	      if (!empty_stack (stack))
		{
		  data = (parse_data_t *) top_data (stack);
		  if (data->fst == NULL)
		    {
		      data->fst = data->lst = sx;
		    }
		  else
		    {
		      data->lst->next = sx;
		      data->lst = sx;
		    }
		}
	      else
		{
                  /* looks like the whole expression was a single
                     quoted value!  So return it. */
		  squoted = 0;
		  esc = 0;
		  state = 1;
		  SAVE_CONT_STATE(SEXP_ERR_OK, sx);
                  return cc;
		}
	    }
	  else
	    state = 8;
	  break;
        case 10:
          if (t[0] == '\"' && esc == 0)
            {
              state = 8;
            }
          vcur[0] = t[0];
          if (t[0] == '\\') esc = 1;
          else esc = 0;
          val_used++;

          if (val_used == val_allocated) {
	    char *valnew = NULL;

#ifdef __cplusplus
            valnew = (char *)sexp_realloc(val,
					  val_allocated+sexp_val_grow_size,
					  val_allocated);
#else
            valnew = sexp_realloc(val,
				  val_allocated+sexp_val_grow_size,
				  val_allocated);
#endif

	    if (valnew == NULL) {
	      SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
	      return cc;
	    } 

	    val = valnew;

            vcur = val + val_used;
            val_allocated += sexp_val_grow_size;
          } else vcur++;

          t++;
          break;
        case 11:
          if (t[0] == '\n') {
            state = 1;
          }
          t++;
          break;
        case 12: /* pre: we saw a # and we're in inline binary mode */
          if (t[0] == 'b') {
            vcur[0] = t[0];
            if (t[0] == '\\') esc = 1;
            else esc = 0;
            val_used++;
            
            if (val_used == val_allocated) {
	      char *valnew = NULL;

#ifdef __cplusplus
              valnew = (char *)sexp_realloc(val,
					    val_allocated+sexp_val_grow_size,
					    val_allocated);
#else
              valnew = sexp_realloc(val,
				    val_allocated+sexp_val_grow_size,
				    val_allocated);
#endif
	      
	      if (valnew == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	      val = valnew;

              vcur = val + val_used;
              val_allocated += sexp_val_grow_size;
            } else vcur++;         
            
            state = 13; /* so far, #b */
            t++;
          } else {
            state = 4; /* not #b, so plain ol' atom */
          }

          break;

        case 13: /* pre: we saw a #b and we're in inline binary mode */
          if (t[0] == '#') {
            vcur[0] = t[0];
            if (t[0] == '\\') esc = 1;
            else esc = 0;
            val_used++;
            
            if (val_used == val_allocated) {
	      char *valnew = NULL;

#ifdef __cplusplus
              valnew = (char *)sexp_realloc(val,
					    val_allocated+sexp_val_grow_size,
					    val_allocated);
#else
              valnew = sexp_realloc(val,
				    val_allocated+sexp_val_grow_size,
				    val_allocated);
#endif

	      if (valnew == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	      val = valnew;

              vcur = val + val_used;
              val_allocated += sexp_val_grow_size;
            } else vcur++;
                        
            state = 14; /* so far, #b# - we're definitely in binary
                           land now. */
            /* reset vcur to val, overwrite #b# with the size string. */
            vcur = val;
            val_used = 0;
            t++;
          } else {
            state = 4; /* not #b#, so plain ol' atom */
          }

          break;
          
        case 14:
          /**
           * so far we've read #b#.  Now, the steps of the process become:
           * proceed to read bytes in until we see # again.  This will be
           * an ASCII representation of the size.  At this point, we want
           * to read as many bytes as specified by this size string after
           * the #.
           */
          if (t[0] == '#') { /* done with size string */
            t++;
            state = 15;
            vcur[0] = '\0';

	    binexpected = (size_t) atoi(val);

            binread = 0;
	    if (binexpected > 0) {
#ifdef __cplusplus
	      bindata = (char *)sexp_malloc(sizeof(char)*binexpected);
#else
	      bindata = sexp_malloc(sizeof(char)*binexpected);
#endif

	      if (bindata == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	    } else {
	      bindata = NULL;
	    }
          } else { /* still reading size string */
            vcur[0] = t[0];
            if (t[0] == '\\') esc = 1;
            else esc = 0;
            val_used++;
            
            if (val_used == val_allocated) {
	      char *valnew = NULL;

#ifdef __cplusplus
              valnew = (char *)sexp_realloc(val,
					    val_allocated+sexp_val_grow_size,
					    val_allocated);
#else
              valnew = sexp_realloc(val,
				    val_allocated+sexp_val_grow_size,
				    val_allocated);
#endif
	      
	      if (valnew == NULL) {
		SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
		return cc;
	      }

	      val = valnew;

              vcur = val + val_used;
              val_allocated += sexp_val_grow_size;
            } else vcur++;

            t++;
          }

          break;

        case 15: /* reading binary blob */
	  if (binread < binexpected) {
	    bindata[binread] = t[0];
	    binread++;
	    t++;
	  }

          if (binread == binexpected) {
            /* state = 1 -- create a sexp_t and head back */
            sx = sexp_t_allocate();

	    if (sx == NULL) {
	      SAVE_CONT_STATE(SEXP_ERR_MEMORY, NULL);
	      return cc;
	    }

            elts++;
            sx->ty = SEXP_VALUE;
            sx->bindata = bindata;
            sx->binlength = binread;
            sx->next = NULL;
            sx->aty = SEXP_BINARY;

            if (event_handlers != NULL &&
                event_handlers->binary != NULL)
              event_handlers->binary(sx->bindata, sx->binlength);

            bindata = NULL;
            binread = binexpected = 0;
            
            state = 1;

            val_used = 0;
            vcur = val;
	      
            if (!empty_stack (stack))
              {
                data = (parse_data_t *) top_data (stack);
                if (data->fst == NULL)
                  {
                    data->fst = data->lst = sx;
                  }
                else
                  {
                    data->lst->next = sx;
                    data->lst = sx;
                  }
              }

          }

          break;

	default:
	  SAVE_CONT_STATE(SEXP_ERR_UNKNOWN_STATE, NULL);
	  return cc;
	}

      /* the null check used to be part of the guard on the while loop.
         unfortunately, if we're in state 15, null is considered a
         perfectly valid byte.  This means the length passed in better
         be accurate for the parser to not walk off the end of the 
         string! */
      if (state != 15 && t[0] == '\0') keepgoing = 0;
    }

  if (depth == 0 && elts > 0) {
    while (stack->top != NULL)
    {
      lvl = pop (stack);
      data = (parse_data_t *) lvl->data;
      sx = data->fst;
      pd_deallocate(data);
      lvl->data = NULL;
    }

    esc = 0;
    state = 1;
    SAVE_CONT_STATE(SEXP_ERR_OK, sx);
  } else {
    SAVE_CONT_STATE(SEXP_ERR_INCOMPLETE, NULL);
    if (t[0] == '\0' || t == bufEnd)
      cc->lastPos = NULL;
    else
      cc->lastPos = t;
  }
  
  return cc;
}
