/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class FunctionType extends Type {
  private transient long swigCPtr;

  protected FunctionType(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.FunctionType_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(FunctionType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_FunctionType(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public FunctionType(Type type) {
    this(CVC4JNI.new_FunctionType__SWIG_0(Type.getCPtr(type), type), true);
  }

  public FunctionType() {
    this(CVC4JNI.new_FunctionType__SWIG_1(), true);
  }

  public long getArity() {
    return CVC4JNI.FunctionType_getArity(swigCPtr, this);
  }

  public vectorType getArgTypes() {
    return new vectorType(CVC4JNI.FunctionType_getArgTypes(swigCPtr, this), true);
  }

  public Type getRangeType() {
    return new Type(CVC4JNI.FunctionType_getRangeType(swigCPtr, this), true);
  }

}
