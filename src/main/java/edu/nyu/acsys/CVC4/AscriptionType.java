/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class AscriptionType {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected AscriptionType(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(AscriptionType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_AscriptionType(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public AscriptionType(Type t) {
    this(CVC4JNI.new_AscriptionType(Type.getCPtr(t), t), true);
  }

  public Type getType() {
    return new Type(CVC4JNI.AscriptionType_getType(swigCPtr, this), true);
  }

  public boolean equals(AscriptionType other) {
    return CVC4JNI.AscriptionType_equals(swigCPtr, this, AscriptionType.getCPtr(other), other);
  }

}
