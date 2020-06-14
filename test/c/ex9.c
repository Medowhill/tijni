#include <stdlib.h>

long f(int *p, int x) {
  *p = x;
  return (long) malloc(sizeof(int));
}

void g(int *p, int x) {
  int *q = (int *) f(p, x);
  *q = x;
}
