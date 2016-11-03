
#include "uthash.h"
#include <stdio.h>

struct my_struct {
  int id;            /* we'll use this field as the key */
  char* name;             
  UT_hash_handle hh; /* makes this structure hashable */
};

struct my_struct *users = NULL;

void add_user(struct my_struct *s) {
  HASH_ADD_KEYPTR(hh,  users, s->name, strlen(s->name), s );    
}

struct my_struct *find_user(char* name) {
  struct my_struct *s;

  HASH_FIND_STR( users, name, s );  
  return s;
}

int main() {
  struct my_struct s = {0, "abc"};
  add_user(&s);
  struct my_struct s0 = {1, "cde"};
  add_user(&s0);
  char* name = "abc";
  struct my_struct* res = find_user(name);
  printf("%s\n", res->name);

}
