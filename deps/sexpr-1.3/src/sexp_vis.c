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
#include "faststack.h"
#include "sexp.h"

void _sexp_to_dotfile(const sexp_t *sx, FILE *fp) {
  const sexp_t *tmp;

  tmp = sx;

  while (tmp != NULL) {
    fprintf(fp,"  sx%lu [shape=record,label=\"",(unsigned long)tmp);
    if (tmp->ty == SEXP_VALUE) {
      fprintf(fp,"{ <type> SEXP_VALUE | ");
      switch (tmp->aty) {
      case SEXP_BASIC:
	fprintf(fp,"SEXP_BASIC }");
	break;
      case SEXP_SQUOTE:
	fprintf(fp,"SEXP_SQUOTE }");
	break;
      case SEXP_DQUOTE:
	fprintf(fp,"SEXP_DQUOTE }");
	break;
      case SEXP_BINARY:
	fprintf(fp,"SEXP_BINARY }");
	break;
      default:
	fprintf(fp,"ATY Unknown }");
	break;
      }
    } else
      fprintf(fp,"<type> SEXP_LIST");

    if (tmp->ty == SEXP_LIST) {
      fprintf(fp,"| <list> list | <next> next\"];\n");	      

      if (tmp->list != NULL) {
	fprintf(fp,"  sx%lu:list -> sx%lu:type;\n",
                (unsigned long)tmp,
                (unsigned long)tmp->list);
	_sexp_to_dotfile(tmp->list,fp);
	if (tmp->next != NULL)
	  fprintf(fp,"  sx%lu:next -> sx%lu:type;\n",
                  (unsigned long)tmp,
                  (unsigned long)tmp->next);
	tmp = tmp->next;
      }
    } else {
      if (tmp->aty == SEXP_BINARY)
	fprintf(fp,"| binlength=%lu | <next> next\"];\n",
		(unsigned long)tmp->binlength);
      else 
	fprintf(fp,"| { va=%lu | vu=%lu } | val=%s | <next> next\"];\n",
		(unsigned long)tmp->val_allocated,
		(unsigned long)tmp->val_used,
		tmp->val);

      if (tmp->next != NULL) 
	fprintf(fp,"  sx%lu:next -> sx%lu:type;\n",
                (unsigned long)tmp,
                (unsigned long)tmp->next);
      tmp = tmp->next;

    }
  }
}

sexp_errcode_t sexp_to_dotfile(const sexp_t *sx, const char *fname) {
  FILE *fp;
 
  if (sx == NULL || fname == NULL) {
    return SEXP_ERR_NULLSTRING;
  }

  fp = fopen(fname,"w+");
  if (fp == NULL) {
    return SEXP_ERR_IO;
  }

  fprintf(fp,"digraph sexp {\n");

  _sexp_to_dotfile(sx,fp);

  fprintf(fp,"}\n");

  fclose(fp);

  return SEXP_ERR_OK;
}
