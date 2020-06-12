void f(int *p, int x) {
  *p = x;
}

void g(int *p, int x) {
  f(p, x);
}
