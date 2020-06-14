#include <stdlib.h>

int *f() {
  return (int *) malloc(sizeof(int));
}

int *g() {
  return (int *) malloc(sizeof(int));
}

int *h(int x) {
  int *p = f();
  int *q = g();
  int *r = p;
  if (x) r = q;
  *r = x;
  return r;
}
