/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class JavaOutputStreamAdapter {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected JavaOutputStreamAdapter(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(JavaOutputStreamAdapter obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_JavaOutputStreamAdapter(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public String toString() {
    return CVC4JNI.JavaOutputStreamAdapter_toString(swigCPtr, this);
  }

  public JavaOutputStreamAdapter() {
    this(CVC4JNI.new_JavaOutputStreamAdapter(), true);
  }

}
