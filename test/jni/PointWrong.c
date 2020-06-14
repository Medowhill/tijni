#include <jni.h>
#include <stdlib.h>
#include "PointWrong.h"

struct point {
  long x, y;
};

typedef struct point point;

long square_root(long x) {
  long i;
  for (i = 0; i * i < x; i++) ;
  return i;
}

point *point_new() {
  point *st = malloc(sizeof(point));
  st->x = 0;
  st->y = 0;
  return st;
}

void point_del(point *p) {
  free(p);
}

long point_get_x(point *p) {
  if (p != NULL)
    return p->x;
  else
    return 0;
}

long point_get_y(point *p) {
  if (p != NULL)
    return p->y;
  else
    return 0;
}

void point_set_x(point *p, long x) {
  if (p != NULL)
    p->x = x;
}

void point_set_y(point *p, long y) {
  if (p != NULL)
    p->y = y;
}

long point_dist(point *p, point *q) {
  if (p != NULL && q != NULL) {
    long dx = p->x - q->x;
    long dy = p->y - q->y;
    return square_root(dx * dx + dy * dy);
  } else
    return 0;
}

JNIEXPORT
jlong JNICALL Java_PointWrong_nativeMalloc(JNIEnv *env, jobject thiz) {
  return (jlong) point_new();
}

JNIEXPORT
void JNICALL Java_PointWrong_nativeFree(JNIEnv *env, jobject thiz, jlong ptr) {
  point_del((point *) ptr);
}

JNIEXPORT
jlong JNICALL Java_PointWrong_nativeGetX(JNIEnv *env, jobject thiz, jlong ptr) {
  return point_get_x((point *) ptr);
}


JNIEXPORT
jlong JNICALL Java_PointWrong_nativeGetY(JNIEnv *env, jobject thiz, jlong ptr) {
  return point_get_y((point *) ptr);
}

JNIEXPORT
void JNICALL Java_PointWrong_nativeSetX(JNIEnv *env, jobject thiz, jlong ptr, jlong x) {
  point_set_x((point *) ptr, x);
}

JNIEXPORT
void JNICALL Java_PointWrong_nativeSetY(JNIEnv *env, jobject thiz, jlong ptr, jlong y) {
  point_set_y((point *) ptr, y);
}

JNIEXPORT
jlong JNICALL Java_PointWrong_nativeDist(JNIEnv *env, jobject thiz, jlong ptr1, jlong ptr2) {
  return point_dist((point *) ptr1, (point *) ptr2);
}
