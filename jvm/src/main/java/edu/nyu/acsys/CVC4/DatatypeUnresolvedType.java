/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class DatatypeUnresolvedType {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected DatatypeUnresolvedType(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(DatatypeUnresolvedType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_DatatypeUnresolvedType(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public DatatypeUnresolvedType(String name) {
    this(CVC4JNI.new_DatatypeUnresolvedType(name), true);
  }

  public String getName() {
    return CVC4JNI.DatatypeUnresolvedType_getName(swigCPtr, this);
  }

}
