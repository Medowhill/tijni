class Ex5 {
  public static void main(String[] args) {
    Ex5 obj = new Ex5();
    obj.foo();
  }

  void foo() { bar(); }
  void bar() { foo(); }
}
