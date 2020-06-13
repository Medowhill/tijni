class Point {
  static { System.loadLibrary("Point"); }

  public static void main(String[] args) {
    Point p1 = new Point();
    p1.malloc();
    p1.setX(1);
    p1.setY(0);

    Point p2 = new Point();
    p2.malloc();
    p2.setX(1);
    p2.setY(1);

    p1.free();
    p2.free();
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

  long dist(Point that) {
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
