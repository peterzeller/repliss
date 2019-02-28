/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class UninterpretedConstant {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected UninterpretedConstant(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(UninterpretedConstant obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_UninterpretedConstant(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public UninterpretedConstant(SWIGTYPE_p_Type type, Integer index) {
    this(CVC4JNI.new_UninterpretedConstant(SWIGTYPE_p_Type.getCPtr(type), Integer.getCPtr(index), index), true);
  }

  public SWIGTYPE_p_Type getType() {
    return new SWIGTYPE_p_Type(CVC4JNI.UninterpretedConstant_getType(swigCPtr, this), true);
  }

  public Integer getIndex() {
    return new Integer(CVC4JNI.UninterpretedConstant_getIndex(swigCPtr, this), false);
  }

  public boolean equals(UninterpretedConstant uc) {
    return CVC4JNI.UninterpretedConstant_equals(swigCPtr, this, UninterpretedConstant.getCPtr(uc), uc);
  }

  public boolean less(UninterpretedConstant uc) {
    return CVC4JNI.UninterpretedConstant_less(swigCPtr, this, UninterpretedConstant.getCPtr(uc), uc);
  }

  public boolean lessEqual(UninterpretedConstant uc) {
    return CVC4JNI.UninterpretedConstant_lessEqual(swigCPtr, this, UninterpretedConstant.getCPtr(uc), uc);
  }

  public boolean greater(UninterpretedConstant uc) {
    return CVC4JNI.UninterpretedConstant_greater(swigCPtr, this, UninterpretedConstant.getCPtr(uc), uc);
  }

  public boolean greaterEqual(UninterpretedConstant uc) {
    return CVC4JNI.UninterpretedConstant_greaterEqual(swigCPtr, this, UninterpretedConstant.getCPtr(uc), uc);
  }

}
