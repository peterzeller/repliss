/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class pairStringType {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected pairStringType(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(pairStringType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_pairStringType(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public pairStringType() {
    this(CVC4JNI.new_pairStringType__SWIG_0(), true);
  }

  public pairStringType(String first, Type second) {
    this(CVC4JNI.new_pairStringType__SWIG_1(first, Type.getCPtr(second), second), true);
  }

  public pairStringType(pairStringType p) {
    this(CVC4JNI.new_pairStringType__SWIG_2(pairStringType.getCPtr(p), p), true);
  }

  public void setFirst(String value) {
    CVC4JNI.pairStringType_first_set(swigCPtr, this, value);
  }

  public String getFirst() {
    return CVC4JNI.pairStringType_first_get(swigCPtr, this);
  }

  public void setSecond(Type value) {
    CVC4JNI.pairStringType_second_set(swigCPtr, this, Type.getCPtr(value), value);
  }

  public Type getSecond() {
    long cPtr = CVC4JNI.pairStringType_second_get(swigCPtr, this);
    return (cPtr == 0) ? null : new Type(cPtr, false);
  }

}