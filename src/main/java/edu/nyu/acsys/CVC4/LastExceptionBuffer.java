/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class LastExceptionBuffer {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected LastExceptionBuffer(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(LastExceptionBuffer obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_LastExceptionBuffer(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public LastExceptionBuffer() {
    this(CVC4JNI.new_LastExceptionBuffer(), true);
  }

  public void setContents(String string) {
    CVC4JNI.LastExceptionBuffer_setContents(swigCPtr, this, string);
  }

  public String getContents() {
    return CVC4JNI.LastExceptionBuffer_getContents(swigCPtr, this);
  }

  public static LastExceptionBuffer getCurrent() {
    long cPtr = CVC4JNI.LastExceptionBuffer_getCurrent();
    return (cPtr == 0) ? null : new LastExceptionBuffer(cPtr, false);
  }

  public static void setCurrent(LastExceptionBuffer buffer) {
    CVC4JNI.LastExceptionBuffer_setCurrent(LastExceptionBuffer.getCPtr(buffer), buffer);
  }

  public static String currentContents() {
    return CVC4JNI.LastExceptionBuffer_currentContents();
  }

}
