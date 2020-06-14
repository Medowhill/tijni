class Ex7 {
  public static void main(String[] args) {
    Ex7 obj1 = new Ex7();
    Ex7 obj2 = new Ex7();
    if (obj1.get() == 0) {
      obj1.set(1);
    } else {
      obj2.set(1);
    }
    long l1 = obj1.get();
    long l2 = obj2.get();
    System.out.println(l1 + l2);
  }

  long x;

  long get() { return x; }

  void set(long nx) {
    x = nx;
  }
}
