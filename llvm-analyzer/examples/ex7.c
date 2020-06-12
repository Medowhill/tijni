#include <stdlib.h>

long f(int x, int y, void *p) {
  if (x) return (long) p;
  if (y) return (long) malloc(sizeof(int));
  return 1;
}
