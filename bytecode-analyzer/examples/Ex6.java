class Ex6 {
  public static void main(String[] args) {
    Ex6 obj = new Ex6();
    System.out.println(obj.foo(1, 2, 3));
  }

  long foo(long l1, long l2, long l3) {
    System.out.println(l1);
    System.out.println(l2);
    System.out.println(l3);
    return l1 + l2 + l3;
  }
}
