/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class StatisticsBase implements Iterable<Object[]> {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected StatisticsBase(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(StatisticsBase obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_StatisticsBase(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void setPrefix(String prefix) {
    CVC4JNI.StatisticsBase_setPrefix(swigCPtr, this, prefix);
  }

  public void flushInformation(java.io.OutputStream out) {
    JavaOutputStreamAdapter tempout = new JavaOutputStreamAdapter();
    try {
      CVC4JNI.StatisticsBase_flushInformation(swigCPtr, this, JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public void safeFlushInformation(int fd) {
    CVC4JNI.StatisticsBase_safeFlushInformation(swigCPtr, this, fd);
  }

  public SExpr getStatistic(String name) {
    return new SExpr(CVC4JNI.StatisticsBase_getStatistic(swigCPtr, this, name), true);
  }

  public JavaIteratorAdapter_StatisticsBase iterator() {
    return new JavaIteratorAdapter_StatisticsBase(CVC4JNI.StatisticsBase_iterator(swigCPtr, this), true);
  }

}
