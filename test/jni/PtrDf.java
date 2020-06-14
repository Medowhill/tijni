class PtrDf {
  static { System.loadLibrary("PtrDf"); }

  public static void main(String[] args) {
    PtrDf p = new PtrDf();
    p.malloc();
    p.free();
    long ptr = p.ptr;
    p.nativeFree(ptr);
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
