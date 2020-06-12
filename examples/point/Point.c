#include <jni.h>
#include <stdlib.h>
#include "Point.h"

struct point {
  int x, y;
};

typedef struct point point;

int square_root(int x) {
  int i;
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

int point_get_x(point *p) {
  if (p != NULL)
    return p->x;
  else
    return 0;
}

int point_get_y(point *p) {
  if (p != NULL)
    return p->y;
  else
    return 0;
}

void point_set_x(point *p, int x) {
  if (p != NULL)
    p->x = x;
}

void point_set_y(point *p, int y) {
  if (p != NULL)
    p->y = y;
}

int point_dist(point *p, point *q) {
  if (p != NULL && q != NULL) {
    int dx = p->x - q->x;
    int dy = p->y - q->y;
    return square_root(dx * dx + dy * dy);
  } else
    return 0;
}

JNIEXPORT
jlong JNICALL Java_Point_nativeMalloc(JNIEnv *env, jobject thiz) {
  return (jlong) point_new();
}

JNIEXPORT
void JNICALL Java_Point_nativeFree(JNIEnv *env, jobject thiz, jlong ptr) {
  point_del((point *) ptr);
}

JNIEXPORT
jint JNICALL Java_Point_nativeGetX(JNIEnv *env, jobject thiz, jlong ptr) {
  return point_get_x((point *) ptr);
}


JNIEXPORT
jint JNICALL Java_Point_nativeGetY(JNIEnv *env, jobject thiz, jlong ptr) {
  return point_get_y((point *) ptr);
}

JNIEXPORT
void JNICALL Java_Point_nativeSetX(JNIEnv *env, jobject thiz, jlong ptr, jint x) {
  point_set_x((point *) ptr, x);
}

JNIEXPORT
void JNICALL Java_Point_nativeSetY(JNIEnv *env, jobject thiz, jlong ptr, jint y) {
  point_set_y((point *) ptr, y);
}

JNIEXPORT
jint JNICALL Java_Point_nativeDist(JNIEnv *env, jobject thiz, jlong ptr1, jlong ptr2) {
  return point_dist((point *) ptr1, (point *) ptr2);
}