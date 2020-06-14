class Ptr {
  static { System.loadLibrary("Ptr"); }

  public static void main(String[] args) {
    Ptr p = new Ptr();
    p.malloc();
    p.free();
    p.free();
  }

  long ptr;

  void malloc() {
    ptr = nativeMalloc();
  }

  void free() {
    nativeFree(ptr);
  }

  native long nativeMalloc();
  native void nativeFree(long ptr);
}
