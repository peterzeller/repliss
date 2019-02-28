/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class UnsatCore implements Iterable<Expr> {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected UnsatCore(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(UnsatCore obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_UnsatCore(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public UnsatCore() {
    this(CVC4JNI.new_UnsatCore__SWIG_0(), true);
  }

  public UnsatCore(SmtEngine smt, vectorExpr core) {
    this(CVC4JNI.new_UnsatCore__SWIG_1(SmtEngine.getCPtr(smt), smt, vectorExpr.getCPtr(core), core), true);
  }

  public SmtEngine getSmtEngine() {
    long cPtr = CVC4JNI.UnsatCore_getSmtEngine(swigCPtr, this);
    return (cPtr == 0) ? null : new SmtEngine(cPtr, false);
  }

  public long size() {
    return CVC4JNI.UnsatCore_size(swigCPtr, this);
  }

  public void toStream(java.io.OutputStream out) {
    JavaOutputStreamAdapter tempout = new JavaOutputStreamAdapter();
    try {
      CVC4JNI.UnsatCore_toStream(swigCPtr, this, JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public JavaIteratorAdapter_UnsatCore iterator() {
    return new JavaIteratorAdapter_UnsatCore(CVC4JNI.UnsatCore_iterator(swigCPtr, this), true);
  }

}
