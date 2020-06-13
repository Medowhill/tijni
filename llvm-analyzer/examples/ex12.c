int f(int **x) {
  return **x;
}

int g(int **x, int **y, int z) {
  int **p = x;
  if (z) p = y;
  return f(p);
}
