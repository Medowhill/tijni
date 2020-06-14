#include <jni.h>
#include <stdlib.h>
#include "Box.h"

JNIEXPORT
jlong JNICALL Java_Box_nativeMalloc(JNIEnv *env, jobject thiz) {
  return (jlong) malloc(sizeof(jlong));
}

JNIEXPORT
void JNICALL Java_Box_nativeFree(JNIEnv *env, jobject thiz, jlong ptr) {
  free((void *) ptr);
}

JNIEXPORT
jlong JNICALL Java_Box_nativeRead(JNIEnv *env, jobject thiz, jlong ptr) {
  return *((jlong *) ptr);
}

JNIEXPORT
void JNICALL Java_Box_nativeWrite(JNIEnv *env, jobject thiz, jlong ptr, jlong x) {
  *((jlong *) ptr) = x;
}
