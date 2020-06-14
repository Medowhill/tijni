class Ex2 {

  public static void main(String[] args) {
    Ex2 obj = new Ex2();
    System.out.println(obj.get());
    obj.set(1);
    System.out.println(obj.get());
    obj.set(0);
    System.out.println(obj.get());
  }

  long x;

  long get() {
    return x;
  }

  void set(long nx) {
    if (nx != 0)
      x = nx;
  }
}
