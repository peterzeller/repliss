/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class vectorUnsignedInt {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected vectorUnsignedInt(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(vectorUnsignedInt obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_vectorUnsignedInt(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public vectorUnsignedInt() {
    this(CVC4JNI.new_vectorUnsignedInt__SWIG_0(), true);
  }

  public vectorUnsignedInt(long n) {
    this(CVC4JNI.new_vectorUnsignedInt__SWIG_1(n), true);
  }

  public long size() {
    return CVC4JNI.vectorUnsignedInt_size(swigCPtr, this);
  }

  public long capacity() {
    return CVC4JNI.vectorUnsignedInt_capacity(swigCPtr, this);
  }

  public void reserve(long n) {
    CVC4JNI.vectorUnsignedInt_reserve(swigCPtr, this, n);
  }

  public boolean isEmpty() {
    return CVC4JNI.vectorUnsignedInt_isEmpty(swigCPtr, this);
  }

  public void clear() {
    CVC4JNI.vectorUnsignedInt_clear(swigCPtr, this);
  }

  public void add(long x) {
    CVC4JNI.vectorUnsignedInt_add(swigCPtr, this, x);
  }

  public long get(int i) {
    return CVC4JNI.vectorUnsignedInt_get(swigCPtr, this, i);
  }

  public void set(int i, long val) {
    CVC4JNI.vectorUnsignedInt_set(swigCPtr, this, i, val);
  }

}
