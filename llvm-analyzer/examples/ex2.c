#include <stdlib.h>

int *f(int x) {
  int *p = malloc(sizeof(int));
  *p = x;
  return p;
}
