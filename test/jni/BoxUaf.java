class BoxUaf {
  static { System.loadLibrary("BoxUaf"); }

  public static void main(String[] args) {
    BoxUaf p = new BoxUaf();
    p.malloc();
    long ptr = p.ptr;
    p.nativeWrite(ptr, 1);
    p.nativeFree(ptr);
    p.nativeRead(ptr);
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
