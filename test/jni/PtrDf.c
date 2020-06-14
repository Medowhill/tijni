#include <jni.h>
#include <stdlib.h>
#include "PtrDf.h"

JNIEXPORT
jlong JNICALL Java_PtrDf_nativeMalloc(JNIEnv *env, jobject thiz) {
  return (jlong) malloc(sizeof(int));
}

JNIEXPORT
void JNICALL Java_PtrDf_nativeFree(JNIEnv *env, jobject thiz, jlong ptr) {
  free((void *) ptr);
}
