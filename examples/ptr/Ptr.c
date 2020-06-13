#include <jni.h>
#include <stdlib.h>
#include "Ptr.h"

JNIEXPORT
jlong JNICALL Java_Ptr_nativeMalloc(JNIEnv *env, jobject thiz) {
  return (jlong) malloc(sizeof(int));
}

JNIEXPORT
void JNICALL Java_Ptr_nativeFree(JNIEnv *env, jobject thiz, jlong ptr) {
  free((void *) ptr);
}
