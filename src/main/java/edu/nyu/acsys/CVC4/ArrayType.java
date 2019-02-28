/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class ArrayType extends Type {
  private transient long swigCPtr;

  protected ArrayType(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.ArrayType_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(ArrayType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_ArrayType(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public ArrayType(Type type) {
    this(CVC4JNI.new_ArrayType__SWIG_0(Type.getCPtr(type), type), true);
  }

  public ArrayType() {
    this(CVC4JNI.new_ArrayType__SWIG_1(), true);
  }

  public Type getIndexType() {
    return new Type(CVC4JNI.ArrayType_getIndexType(swigCPtr, this), true);
  }

  public Type getConstituentType() {
    return new Type(CVC4JNI.ArrayType_getConstituentType(swigCPtr, this), true);
  }

}
