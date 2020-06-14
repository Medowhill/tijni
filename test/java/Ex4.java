class Ex4 {

  public static void main(String[] args) {
    Ex4 obj = new Ex4();
    obj.sum(5);
    System.out.println(obj.get());
  }

  long x;

  long get() {
    return x;
  }

  void sum(long nx) {
    if (nx == 0) return;
    else {
      x = x + nx;
      sum(nx - 1);
    }
  }
}
