#include <stdlib.h>

void f(long p) {
  free((void *) p);
}
