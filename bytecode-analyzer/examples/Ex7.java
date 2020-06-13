class Ex7 {
  public static void main(String[] args) {
    Ex7 obj1 = new Ex7();
    Ex7 obj2 = new Ex7();
    if (obj1.get() == 0) {
      obj1.set(1);
    } else {
      obj2.set(1);
    }
    System.out.println(obj1.get() + obj2.get());
  }

  long x;

  long get() { return x; }

  void set(long nx) {
    x = nx;
  }
}
