class PointEx {
  public static void main(String[] args) {
    Point p1 = new Point();
    p1.malloc();
    p1.setX(1);
    p1.setY(0);

    Point p2 = new Point();
    p2.malloc();
    p2.setX(1);
    p2.setY(1);

    System.out.println(p1.dist(p2));

    p1.free();
    p2.free();
  }
}

class Point {
  static { System.loadLibrary("Point"); }

  long ptr = 0;

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

  int getX() {
    if (ptr != 0)
      return nativeGetX(ptr);
    else
      return 0;
  }

  int getY() {
    if (ptr != 0)
      return nativeGetY(ptr);
    else
      return 0;
  }

  void setX(int x) {
    if (ptr != 0)
      nativeSetX(ptr, x);
  }

  void setY(int y) {
    if (ptr != 0)
      nativeSetY(ptr, y);
  }

  int dist(Point that) {
    return nativeDist(ptr, that.ptr);
  }

  native long nativeMalloc();
  native void nativeFree(long ptr);
  native int nativeGetX(long ptr);
  native int nativeGetY(long ptr);
  native void nativeSetX(long ptr, int x);
  native void nativeSetY(long ptr, int y);
  native int nativeDist(long ptr1, long ptr2);
}
