/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class SygusPrintCallback {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected SygusPrintCallback(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(SygusPrintCallback obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_SygusPrintCallback(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void toStreamSygus(SWIGTYPE_p_CVC4__Printer p, java.io.OutputStream out, Expr e) {
    JavaOutputStreamAdapter tempout = new JavaOutputStreamAdapter();
    try {
      CVC4JNI.SygusPrintCallback_toStreamSygus(swigCPtr, this, SWIGTYPE_p_CVC4__Printer.getCPtr(p), JavaOutputStreamAdapter.getCPtr(tempout), Expr.getCPtr(e), e);
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

}
