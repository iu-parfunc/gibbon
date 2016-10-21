/**

SFSEXP: Small, Fast S-Expression Library version 1.3
Written by Matthew Sottile (mjsottile@gmail.com)

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

**/

/*
 * Simple interpreter of s-expressions
 */
#include <stdio.h>
#include <sexp.h>
#include <sexp_ops.h>
#include <string.h>

typedef struct dict {
  char varname[255];
  sexp_t *valexp;
  struct dict *next;
} dict_t;

/**
 * lookup a variable.  if we find it, return it.  Otherwise
 * return null.
 */
sexp_t *lookup(char *varname, dict_t *d) {
  sexp_t *ret = NULL;
  dict_t *_d = d;

  printf("Lookup : %s\n",varname);
  
  while (_d != NULL) {
    if (strcmp(varname,_d->varname) == 0) {
      ret = _d->valexp;
      break;
    }
    _d = _d->next;
  }

  return ret;
}

/**
 * insert a variable into the dictionary
 */
dict_t *insert(char *varname, sexp_t *val, dict_t *d) {
  dict_t *_d = d;
  char dbgbuf[BUFSIZ];

  printf("Inserting %s into dictionary.\n",varname);
  print_sexp(dbgbuf,BUFSIZ,val);
  printf("%s value is : %s\n",varname,dbgbuf);

  if (_d == NULL) {
    /* empty dictionary -- create one entry and return */
    _d = (dict_t *)malloc(sizeof(dict_t));
    _d->valexp = val;
    strcpy(_d->varname,varname);
    _d->next = NULL;
  } else {
    /* not empty, so first see if the name is already used.
       If so, purge the expression that was there and replace it. */
    while (1) {
      if (strcmp(_d->varname,varname) == 0) {
        destroy_sexp(_d->valexp);
        _d->valexp = val;
        return d;
      }

      /* if we're at the end with nothing left, break out of the loop */
      if (_d->next == NULL) break;
      _d = _d->next;
    }

    /* we're at the end, so tack on one entry for the value we want
       to add. */     
    _d->next = (dict_t *)malloc(sizeof(dict_t));
    _d->next->valexp = val;
    strcpy(_d->next->varname,varname);
    _d->next->next = NULL;
    return d;
  }

  return _d;
}

/**
 * look up an entry and purge it -- not done yet
 */
dict_t *purge(char *varname, dict_t *d) {
  /* find an entry and purge it */
  return d;
}

/**
 * purge all entries in the dictionary and free the dictionary itself.
 */
void purge_all(dict_t *d) {
  dict_t *_d = d;
  dict_t *td;
  
  while (_d != NULL) {
    td = _d->next;
    printf("PURGING: %s\n",_d->varname);
    destroy_sexp(_d->valexp);
    free(_d);
    _d = td;
  }
}


/******************
   eval function
*******************/
dict_t *eval(sexp_t *exp, dict_t *env) {
  char *v;
  dict_t *d = env;
  sexp_t *tmpsx, *tmpsx2;

  /**
   * values understood here: 
   *   setq
   *   circle
   *   point
   *   section
   *   draw
   *   segment
   */
  if (exp->ty == SEXP_LIST) {
    if (exp->list->ty == SEXP_VALUE)
      v = exp->list->val;
    else return env;
  } else return env;

  if (strcmp(v,"setq") == 0) {
    d = insert(exp->list->next->val,exp->list->next->next,env);
    exp->list->next->next = NULL;
  } else if (strcmp(v,"circle") == 0) {
    printf("CIRCLE: \n");
    tmpsx = lookup(exp->list->next->val,d);
    d = eval(tmpsx,d);
    tmpsx = lookup(exp->list->next->next->val,d);
    d = eval(tmpsx,d);
  } else if (strcmp(v,"point") == 0) {
    printf("POINT AT: %s,%s\n",exp->list->next->val,
            exp->list->next->next->val);
  } else if (strcmp(v,"section") == 0) {
    printf("SECTION: \n");
    tmpsx = lookup(exp->list->next->val,d);
    d = eval(tmpsx,d);
    tmpsx = lookup(exp->list->next->next->val,d);
    d = eval(tmpsx,d);
  } else if (strcmp(v,"draw") == 0) {
    tmpsx = exp->list->next;
    while (tmpsx != NULL) {
      printf("DRAWING: ");
      if (tmpsx->ty == SEXP_VALUE) {
        tmpsx2 = lookup(tmpsx->val,d);
        d = eval(tmpsx2,d);
      } else {
        d = eval(tmpsx,d);
      }
      tmpsx = tmpsx->next;
    }
  } else if (strcmp(v,"segment") == 0) {
    printf("SEGMENT:\n");

    d = eval(exp->list->next,d);
    d = eval(exp->list->next->next,d);
    
  } else if (strcmp(v,"point1") == 0) {
    printf("POINT1 OF :\n");
    tmpsx = lookup(exp->list->next->val,d);

    d = eval(tmpsx,d);

  } else if (strcmp(v,"point2") == 0) {
    printf("POINT2 OF :\n");

    tmpsx = lookup(exp->list->next->val,d);

    d = eval(tmpsx,d);

  } else {
    printf("EVAL: Unknown = %s\n",v);
  }

  return d;
}

/****
 * main
 ****/
int main(int argc, char **argv) {
  char linebuf[BUFSIZ];
  FILE *fp;
  char *status;
  sexp_t *sx;
  dict_t *env = NULL;

  fp = fopen("sexps.in","r+");

  while (1) {
    status = fgets(linebuf,BUFSIZ,fp);

    if (feof(fp) != 0) break;
    
    /* if not EOF and status was NULL, something bad happened. */
    if (status != linebuf) {
      printf("Error encountered on fgets.\n");
      exit(EXIT_FAILURE);
    }

    sx = parse_sexp(linebuf,BUFSIZ);
    print_sexp(linebuf,BUFSIZ,sx);

    env = eval(sx,env);

    destroy_sexp(sx);

    fflush(stderr);
  }

  purge_all(env);
  sexp_cleanup();

  fclose(fp);

  exit(EXIT_SUCCESS);
}
