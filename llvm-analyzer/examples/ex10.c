#include <stdlib.h>

void f(int *p) {
  *p = 0;
}

void g(int *p) {
  free(p);
}

void h(int *p, int *q, int x) {
  if (x) {
    f(p);
    g(q);
  } else {
    g(p);
    f(q);
  }
}
