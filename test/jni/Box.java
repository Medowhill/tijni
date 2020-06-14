class Box {
  static { System.loadLibrary("Box"); }

  public static void main(String[] args) {
    Box p = new Box();
    p.malloc();
    p.write(1);
    p.read();
    p.free();
  }

  long ptr;

  void malloc() {
    ptr = nativeMalloc();
  }

  void free() {
    nativeFree(ptr);
  }

  long read() {
    return nativeRead(ptr);
  }

  void write(long x) {
    nativeWrite(ptr, x);
  }

  native long nativeMalloc();
  native void nativeFree(long ptr);
  native long nativeRead(long ptr);
  native void nativeWrite(long ptr, long x);
}
