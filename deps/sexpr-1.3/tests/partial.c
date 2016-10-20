#include <stdio.h>
#include "sexp.h"
#include <sys/time.h>
#include <time.h>
#include <assert.h>
#include <string.h>

#define RAWSTRING "(this expression (is incomplete) and (has more\0"

int main(int argc, char **argv) {
  unsigned int len;
  pcont_t *pc;
  char inbuf[256];
  char outbuf[1024];

  strcpy(inbuf,RAWSTRING);
  len = strlen(inbuf);

  pc = NULL;

  pc = cparse_sexp(inbuf,len,pc);

  if (sexp_errno == SEXP_ERR_INCOMPLETE) {
    printf("Incomplete expression detected.\n");
  } else {
    printf("Unexpected error: %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  print_pcont(pc,outbuf,1024);

  printf("%s\n",outbuf);

  destroy_continuation(pc);
  sexp_cleanup();
  
  exit(EXIT_SUCCESS);
}
