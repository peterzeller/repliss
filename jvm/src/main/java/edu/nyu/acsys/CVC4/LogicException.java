/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class LogicException extends Exception {
  private transient long swigCPtr;

  protected LogicException(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.LogicException_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(LogicException obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_LogicException(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public LogicException() {
    this(CVC4JNI.new_LogicException__SWIG_0(), true);
  }

  public LogicException(String msg) {
    this(CVC4JNI.new_LogicException__SWIG_1(msg), true);
  }

}
