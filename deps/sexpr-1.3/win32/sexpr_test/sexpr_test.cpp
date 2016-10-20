// This is the main project file for VC++ application project 
// generated using an Application Wizard.

#include "stdafx.h"

#include <stdio.h>
#include "sexp.h"
#include <assert.h>
#include <string.h>

#using <mscorlib.dll>

using namespace System;


/**
 * Given an s-expression of the form:
 *
 * (tag (val_list))
 *
 * Where each value in the val_list is a float, extract the data and
 * populate the array passed in (the length of the array is in `size').
 * Assume the array was allocated properly, size is correct, and the
 * expression contains the right number of values.  This eliminates lots
 * of error checking code and makes the process a bit clearer.
 */
void extract(sexp_t *sx, float *data, int size) {
  sexp_t *s;
  int i;
  assert(sx != NULL && data != NULL && size > 0); /* duh */

  /* assumption : s-expression is formatted as (tag (val1 val2 val3)) */
  printf("Data tag: [%s]\n",sx->list->val);

  /* s = (vallist) */
  s = sx->list->next;
  
  i = 0;
  s = s->list;
  while (s != NULL && i < size) {
    sscanf(s->val,"%f",&data[i]);
    s = s->next;
    i++;
  }
}

/**
 * example of packing three values into an expression of the form:
 *
 * (tag (v1 v2 v3))
 *
 * COMMENTS SHOW THE STATE OF SX AS IT IS CONSTRUCTED
 */
sexp_t *pack(char *tag, float v1, float v2, float v3) {
  sexp_t *sx = new_sexp_list(new_sexp_atom(tag,strlen(tag)));
  /* sx = (tag) */
  char sbuf[32];
  sexp_t *vlist,*vptr;
  

  vlist = new_sexp_list(NULL); /* vlist = () */
  sx->list->next = vlist; /* sx = (tag ()) */

  sprintf(sbuf,"%f",v1);
  vlist->list = new_sexp_atom(sbuf,strlen(sbuf)); /* vlist = (v1) */
  vptr = vlist->list;

  /* sx = (tag (v1)) */

  sprintf(sbuf,"%f",v2);
  vptr->next = new_sexp_atom(sbuf,strlen(sbuf)); /* vlist = (v1 v2) */
  vptr = vptr->next;

  /* sx = (tag (v1 v2)) */

  sprintf(sbuf,"%f",v3);
  vptr->next = new_sexp_atom(sbuf,strlen(sbuf)); /* vlist = (v1 v2 v3) */

  /* sx = (tag (v1 v2 v3))  ---- done, return. */

  return sx;
}

int _tmain()
{
    // TODO: Please replace the sample code below with your own.
    Console::WriteLine(S"Hello World");

	  char buf[256]; /* string to sprintf to */
  float vals[3]; /* place to put data */
  sexp_t *sx;

  /*** method #1: create expression as string on one side, extract data
       on the other. ***/

  printf("===>> PART 1 <<===\n");
  sprintf(buf,"(thetag (1.0 2.0 3.0))");
  sx = parse_sexp(buf,strlen(buf));

  extract(sx,vals,3);

  printf("Extracted V1=%f V2=%f V3=%f\n",vals[0],vals[1],vals[2]);

  destroy_sexp(sx);

  /*** method #2: packing function creates expression, same extract
                  function extracts data.  print in between to show expression.
   ***/
  printf("\n===>> PART 2 <<===\n");
  sx = pack("part2tag",4.0,5.0,6.0);
  print_sexp(buf,256,sx);
  printf("SX=%s\n",buf);

  extract(sx,vals,3);
  printf("Extracted V1=%f V2=%f V3=%f\n",vals[0],vals[1],vals[2]);

  destroy_sexp(sx);

	return 0;
}