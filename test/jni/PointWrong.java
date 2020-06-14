class PointWrong {
  static { System.loadLibrary("PointWrong"); }

  public static void main(String[] args) {
    PointWrong p = new PointWrong();
    p.malloc();
    long ptr = p.ptr;

    p.nativeGetX(ptr);
    p.nativeSetX(ptr, 1);
    p.nativeFree(ptr);

    p.nativeGetX(ptr);
    p.nativeSetX(ptr, 1);
    p.nativeFree(ptr);
  }

  long ptr;

  void malloc() {
    if (ptr == 0)
      ptr = nativeMalloc();
  }

  void free() {
    if (ptr != 0) {
      nativeFree(ptr);
      ptr = 0;
    }
  }

  long getX() {
    if (ptr != 0)
      return nativeGetX(ptr);
    else
      return 0;
  }

  long getY() {
    if (ptr != 0)
      return nativeGetY(ptr);
    else
      return 0;
  }

  void setX(long x) {
    if (ptr != 0)
      nativeSetX(ptr, x);
  }

  void setY(long y) {
    if (ptr != 0)
      nativeSetY(ptr, y);
  }

  long dist(PointWrong that) {
    return nativeDist(ptr, that.ptr);
  }

  native long nativeMalloc();
  native void nativeFree(long ptr);
  native long nativeGetX(long ptr);
  native long nativeGetY(long ptr);
  native void nativeSetX(long ptr, long x);
  native void nativeSetY(long ptr, long y);
  native long nativeDist(long ptr1, long ptr2);
}
