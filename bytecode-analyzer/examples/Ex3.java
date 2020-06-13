class Ex3 {

  public static void main(String[] args) {
    Ex3 obj = new Ex3();
    long x = 1;
    for (long i = 1; i != x + x + x + x + x + x; i++) {
      obj.set(i);
      System.out.println(obj.get());
    }
  }

  long x;

  long get() {
    return x;
  }

  void set(long nx) {
    x = nx;
  }
}
