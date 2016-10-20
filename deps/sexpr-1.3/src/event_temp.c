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
#include <assert.h>
#include "sexp.h"

static size_t sexp_val_start_size = 256;
static size_t sexp_val_grow_size  = 64;

/*************************************************************************/


/**
 * event parser : based on cparse_sexp from v1.91 of this file.  separate out
 * in the event that the parser mode is PARSER_EVENTS_ONLY.
 */
pcont_t *
eparse_sexp (char *str, size_t len, pcont_t *lc)
{
  char *t, *s;
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
  pcont_t *cc;
  char *val, *vcur, *bindata = NULL;
  faststack_t *stack;
  char *bufEnd;
  int keepgoing = 1;
  parser_event_handlers_t *event_handlers = NULL;

  /* make sure non-null string */
  if (str == NULL) {
    lc->error = SEXP_ERR_NULLSTRING;
    return lc;
  }
  
  /* first, if we have a non null continuation passed in, restore state. */
  if (lc != NULL) {
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
#ifdef __cplusplus
    cc = (pcont_t *)sexp_malloc(sizeof(pcont_t));
#else
    cc = sexp_malloc(sizeof(pcont_t));
#endif
    assert(cc != NULL);
    
    cc->mode = mode;

    /* allocate atom buffer */
#ifdef __cplusplus
    cc->val = val = (char *)sexp_malloc(sizeof(char)*sexp_val_start_size);
#else
    cc->val = val = sexp_malloc(sizeof(char)*sexp_val_start_size);
#endif
    assert(val != NULL);
    
    cc->val_used = val_used = 0;
    cc->val_allocated = val_allocated = sexp_val_start_size;

    vcur = val;
    
    /* allocate stack */
    cc->stack = stack = make_stack();
    cc->bindata = NULL;
    cc->binread = cc->binexpected = 0;

    /* event handlers are null */
    cc->event_handlers = NULL;

    /* t is temp pointer into s for current position */
    s = str;
    t = s;
    cc->sbuffer = str;
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
                assert(val != NULL);
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

	  elts++;
	  
	  if (stack->height < 1)
            stack->height++;

          stack->height++;
	  
	  state = 1;
	  break;
	case 3:
	  /** close paren **/

          /* check for close parens that were never opened. */
          if (depth == 0) {
            cc->bindata = bindata;
            cc->binread = binread;
            cc->binexpected = binexpected;
	    cc->val = val;
            cc->mode = mode;
            cc->val_used = val_used;
            cc->val_allocated = val_allocated;
	    cc->vcur = vcur;
	    cc->lastPos = t;
	    cc->depth = depth;
	    cc->qdepth = qdepth;
	    cc->state = 1;
	    cc->stack = stack;
	    cc->esc = 0;
	    cc->last_sexp = NULL;
            cc->error = SEXP_ERR_BADFORM;
            cc->event_handlers = event_handlers;

	    return cc;
          }

	  t++;
	  depth--;

          stack->height--;

          if (event_handlers != NULL && 
              event_handlers->end_sexpr != NULL)
            event_handlers->end_sexpr();

	  state = 1;

	  /** if depth = 0 then we finished a sexpr, and we return **/
	  if (depth == 0) {
            cc->bindata = bindata;
            cc->binread = binread;
            cc->binexpected = binexpected;
            cc->error = SEXP_ERR_OK;
            cc->mode = mode;
	    cc->val = val;
            cc->val_allocated = val_allocated;
            cc->val_used = val_used;
	    cc->vcur = vcur;
	    cc->lastPos = t;
	    cc->depth = depth;
	    cc->qdepth = qdepth;
	    cc->state = 1;
	    cc->stack = stack;
	    cc->esc = 0;
            cc->event_handlers = event_handlers;
	    cc->last_sexp = NULL;
            stack->height = 0;

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

	      elts++;

              if (event_handlers != NULL &&
                  event_handlers->characters != NULL) {
                if (squoted != 0)
                  event_handlers->characters(val,val_used,SEXP_SQUOTE);
                else
                  event_handlers->characters(val,val_used,SEXP_BASIC);
              }

              vcur = val;
              val_used = 0;

              if (stack->height < 1) {
                  /* looks like this expression was just a basic atom - so
                     return it. */
                  cc->bindata = bindata;
                  cc->binread = binread;
                  cc->binexpected = binexpected;
                  cc->mode = mode;
                  cc->error = SEXP_ERR_OK;
                  cc->val = val;
                  cc->val_used = val_used;
                  cc->val_allocated = val_allocated;
                  cc->vcur = vcur;
                  cc->squoted = 0;
                  cc->lastPos = t;
                  cc->depth = depth;
                  cc->qdepth = qdepth;
                  cc->state = 1;
                  cc->stack = stack;
                  cc->esc = 0;
                  cc->last_sexp = NULL;
                  cc->event_handlers = event_handlers;

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
#ifdef __cplusplus
                val = (char *)sexp_realloc(val,
                                           val_allocated+sexp_val_grow_size,
                                           val_allocated);
#else
                val = sexp_realloc(val,
                                   val_allocated+sexp_val_grow_size,
                                   val_allocated);
#endif
                assert(val != NULL);
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
#ifdef __cplusplus
                  val = (char *)sexp_realloc(val,
                                             val_allocated+sexp_val_grow_size,
                                             val_allocated);
#else
                  val = sexp_realloc(val,
                                     val_allocated+sexp_val_grow_size,
                                     val_allocated);
#endif
                  assert(val != NULL);
                  vcur = val + val_used;
                  val_allocated += sexp_val_grow_size;
                } else vcur++;
              }

              vcur[0] = '\0';

              val_used++;
	      elts++;

              if (event_handlers != NULL &&
                  event_handlers->characters != NULL) {
                if (squoted == 1)
                  event_handlers->characters(val,val_used,SEXP_SQUOTE);
                else
                  event_handlers->characters(val,val_used,SEXP_DQUOTE);
              }

              vcur = val;
              val_used = 0;
	      
	      if (stack->height < 1) {
                  /* looks like this expression was just a basic double
                     quoted atom - so return it. */
                  t++; /* spin past the quote */

                  cc->bindata = bindata;
                  cc->binread = binread;
                  cc->binexpected = binexpected;
                  cc->mode = mode;
                  cc->squoted = 0;
                  cc->error = SEXP_ERR_OK;
                  cc->val = val;
                  cc->val_used = val_used;
                  cc->val_allocated = val_allocated;
                  cc->vcur = vcur;
                  cc->lastPos = t++;
                  cc->depth = depth;
                  cc->qdepth = qdepth;
                  cc->state = 1;
                  cc->stack = stack;
                  cc->esc = 0;
                  cc->last_sexp = NULL;
                  cc->event_handlers = event_handlers;

                  return cc;
		}
	    }
	  else
	    {
	      vcur[0] = t[0];
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
                assert(val != NULL);
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
#ifdef __cplusplus
                val = (char *)sexp_realloc(val,
                                           val_allocated+sexp_val_grow_size,
                                           val_allocated);
#else
                val = sexp_realloc(val,
                                   val_allocated+sexp_val_grow_size,
                                   val_allocated);
#endif
                assert(val != NULL);
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
#ifdef __cplusplus
            val = (char *)sexp_realloc(val,
                                       val_allocated+sexp_val_grow_size,
                                       val_allocated);
#else
            val = sexp_realloc(val,
                               val_allocated+sexp_val_grow_size,
                               val_allocated);
#endif
            assert(val != NULL);
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

	      elts++;

              if (event_handlers != NULL &&
                  event_handlers->characters != NULL)
                event_handlers->characters(val,val_used,SEXP_SQUOTE);

              vcur = val;
              val_used = 0;
	      
	      if (stack->height < 1) {
                  /* looks like the whole expression was a single
                     quoted value!  So return it. */
                  cc->bindata = bindata;
                  cc->binread = binread;
                  cc->binexpected = binexpected;
                  cc->mode = mode;
                  cc->error = SEXP_ERR_OK;
                  cc->squoted = 0;
                  cc->val = val;
                  cc->val_used = val_used;
                  cc->val_allocated = val_allocated;
                  cc->vcur = vcur;
                  cc->lastPos = t;
                  cc->depth = depth;
                  cc->qdepth = qdepth;
                  cc->state = 1;
                  cc->stack = stack;
                  cc->esc = 0;
                  cc->last_sexp = NULL;
                  cc->event_handlers = event_handlers;

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
#ifdef __cplusplus
            val = (char *)sexp_realloc(val,
                                       val_allocated+sexp_val_grow_size,
                                       val_allocated);
#else
            val = sexp_realloc(val,
                               val_allocated+sexp_val_grow_size,
                               val_allocated);
#endif
            assert(val != NULL);
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
#ifdef __cplusplus
              val = (char *)sexp_realloc(val,
                                         val_allocated+sexp_val_grow_size,
                                         val_allocated);
#else
              val = sexp_realloc(val,
                                 val_allocated+sexp_val_grow_size,
                                 val_allocated);
#endif
              assert(val != NULL);
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
#ifdef __cplusplus
              val = (char *)sexp_realloc(val,
                                         val_allocated+sexp_val_grow_size,
                                         val_allocated);
#else
              val = sexp_realloc(val,
                                 val_allocated+sexp_val_grow_size,
                                 val_allocated);
#endif
              assert(val != NULL);
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
            assert(binexpected > 0);
            binread = 0;
#ifdef __cplusplus
            bindata = (char *)sexp_malloc(sizeof(char)*binexpected);
#else
            bindata = sexp_malloc(sizeof(char)*binexpected);            
#endif
            assert(bindata != NULL);
          } else { /* still reading size string */
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
              assert(val != NULL);
              vcur = val + val_used;
              val_allocated += sexp_val_grow_size;
            } else vcur++;

            t++;
          }

          break;

        case 15: /* reading binary blob */
          bindata[binread] = t[0];
          binread++;
          t++;

          if (binread == binexpected) {
            /* state = 1 -- create a sexp_t and head back */

            elts++;

            if (event_handlers != NULL &&
                event_handlers->binary != NULL)
              event_handlers->binary(bindata, binread);

            sexp_free(bindata,binread);
            bindata = NULL;
            binread = binexpected = 0;
            
            state = 1;

            val_used = 0;
            vcur = val;
	      
          }

          break;

	default:
	  fprintf (stderr, "eparse_sexp: unknown parser state %d.\n", state);
	  break;
	}

      /* the null check used to be part of the guard on the while loop.
         unfortunately, if we're in state 15, null is considered a
         perfectly valid byte.  This means the length passed in better
         be accurate for the parser to not walk off the end of the 
         string! */
      if (state != 15 && t[0] == '\0') keepgoing = 0;
    }

  if (depth == 0 && elts > 0) {
    cc->bindata = bindata;
    cc->binread = binread;
    cc->binexpected = binexpected;
    cc->mode = mode;
    cc->error = SEXP_ERR_OK;
    cc->val = val;
    cc->squoted = squoted;
    cc->val_used = val_used;
    cc->val_allocated = val_allocated;
    cc->vcur = vcur;
    cc->lastPos = t;
    cc->depth = depth;
    cc->qdepth = qdepth;
    cc->state = 1;
    cc->stack = stack;
    cc->esc = 0;
    cc->event_handlers = event_handlers;
    stack->height = 0;
    cc->last_sexp = NULL;
  } else {
    cc->bindata = bindata;
    cc->binread = binread;
    cc->binexpected = binexpected;
    cc->mode = mode;
    cc->val = val;
    cc->esc = esc;
    cc->squoted = squoted;
    cc->vcur = vcur;
    cc->val_allocated = val_allocated;
    cc->val_used = val_used;
    if (t[0] == '\0' || t == bufEnd)
      cc->lastPos = NULL;
    else
      cc->lastPos = t;
    cc->depth = depth;
    cc->qdepth = qdepth;
    cc->state = state;
    cc->stack = stack;
    cc->last_sexp = NULL;
    cc->event_handlers = event_handlers;
    cc->error = SEXP_ERR_OK;
  }
  
  return cc;
}
