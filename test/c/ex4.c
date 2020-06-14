#include <stdlib.h>

long f(int x) {
  int *p = malloc(sizeof(int));
  *p = x;
  return (long) p;
}
