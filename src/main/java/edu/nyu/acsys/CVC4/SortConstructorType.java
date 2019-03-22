/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class SortConstructorType extends Type {
  private transient long swigCPtr;

  protected SortConstructorType(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.SortConstructorType_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(SortConstructorType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_SortConstructorType(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public SortConstructorType(Type type) {
    this(CVC4JNI.new_SortConstructorType__SWIG_0(Type.getCPtr(type), type), true);
  }

  public SortConstructorType() {
    this(CVC4JNI.new_SortConstructorType__SWIG_1(), true);
  }

  public String getName() {
    return CVC4JNI.SortConstructorType_getName(swigCPtr, this);
  }

  public long getArity() {
    return CVC4JNI.SortConstructorType_getArity(swigCPtr, this);
  }

  public SortType instantiate(vectorType params) {
    return new SortType(CVC4JNI.SortConstructorType_instantiate(swigCPtr, this, vectorType.getCPtr(params), params), true);
  }

}
