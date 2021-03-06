/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class Statistics extends StatisticsBase {
  private transient long swigCPtr;

  protected Statistics(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.Statistics_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(Statistics obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_Statistics(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public Statistics(StatisticsBase stats) {
    this(CVC4JNI.new_Statistics__SWIG_0(StatisticsBase.getCPtr(stats), stats), true);
  }

  public Statistics(Statistics stats) {
    this(CVC4JNI.new_Statistics__SWIG_1(Statistics.getCPtr(stats), stats), true);
  }

  public Statistics assign(StatisticsBase stats) {
    return new Statistics(CVC4JNI.Statistics_assign__SWIG_0(swigCPtr, this, StatisticsBase.getCPtr(stats), stats), false);
  }

  public Statistics assign(Statistics stats) {
    return new Statistics(CVC4JNI.Statistics_assign__SWIG_1(swigCPtr, this, Statistics.getCPtr(stats), stats), false);
  }

}
