class Ex1 {

  public static void main(String[] args) {
    Ex1 obj = new Ex1();
    System.out.println(obj.get());
    obj.set(1);
    System.out.println(obj.get());
  }

  long x;

  long get() {
    return x;
  }

  void set(long nx) {
    x = nx;
  }
}
