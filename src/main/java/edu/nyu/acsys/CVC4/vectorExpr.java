/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class vectorExpr {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected vectorExpr(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(vectorExpr obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_vectorExpr(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public vectorExpr() {
    this(CVC4JNI.new_vectorExpr__SWIG_0(), true);
  }

  public vectorExpr(long n) {
    this(CVC4JNI.new_vectorExpr__SWIG_1(n), true);
  }

  public long size() {
    return CVC4JNI.vectorExpr_size(swigCPtr, this);
  }

  public long capacity() {
    return CVC4JNI.vectorExpr_capacity(swigCPtr, this);
  }

  public void reserve(long n) {
    CVC4JNI.vectorExpr_reserve(swigCPtr, this, n);
  }

  public boolean isEmpty() {
    return CVC4JNI.vectorExpr_isEmpty(swigCPtr, this);
  }

  public void clear() {
    CVC4JNI.vectorExpr_clear(swigCPtr, this);
  }

  public void add(Expr x) {
    CVC4JNI.vectorExpr_add(swigCPtr, this, Expr.getCPtr(x), x);
  }

  public Expr get(int i) {
    return new Expr(CVC4JNI.vectorExpr_get(swigCPtr, this, i), false);
  }

  public void set(int i, Expr val) {
    CVC4JNI.vectorExpr_set(swigCPtr, this, i, Expr.getCPtr(val), val);
  }

}
