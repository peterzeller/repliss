/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class vectorVectorExpr {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected vectorVectorExpr(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(vectorVectorExpr obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_vectorVectorExpr(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public vectorVectorExpr() {
    this(CVC4JNI.new_vectorVectorExpr__SWIG_0(), true);
  }

  public vectorVectorExpr(long n) {
    this(CVC4JNI.new_vectorVectorExpr__SWIG_1(n), true);
  }

  public long size() {
    return CVC4JNI.vectorVectorExpr_size(swigCPtr, this);
  }

  public long capacity() {
    return CVC4JNI.vectorVectorExpr_capacity(swigCPtr, this);
  }

  public void reserve(long n) {
    CVC4JNI.vectorVectorExpr_reserve(swigCPtr, this, n);
  }

  public boolean isEmpty() {
    return CVC4JNI.vectorVectorExpr_isEmpty(swigCPtr, this);
  }

  public void clear() {
    CVC4JNI.vectorVectorExpr_clear(swigCPtr, this);
  }

  public void add(vectorExpr x) {
    CVC4JNI.vectorVectorExpr_add(swigCPtr, this, vectorExpr.getCPtr(x), x);
  }

  public vectorExpr get(int i) {
    return new vectorExpr(CVC4JNI.vectorVectorExpr_get(swigCPtr, this, i), false);
  }

  public void set(int i, vectorExpr val) {
    CVC4JNI.vectorVectorExpr_set(swigCPtr, this, i, vectorExpr.getCPtr(val), val);
  }

}
