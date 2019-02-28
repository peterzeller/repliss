/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class DatatypeType extends Type {
  private transient long swigCPtr;

  protected DatatypeType(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.DatatypeType_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(DatatypeType obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_DatatypeType(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public DatatypeType(Type type) {
    this(CVC4JNI.new_DatatypeType__SWIG_0(Type.getCPtr(type), type), true);
  }

  public DatatypeType() {
    this(CVC4JNI.new_DatatypeType__SWIG_1(), true);
  }

  public Datatype getDatatype() {
    return new Datatype(CVC4JNI.DatatypeType_getDatatype(swigCPtr, this), false);
  }

  public boolean isParametric() {
    return CVC4JNI.DatatypeType_isParametric(swigCPtr, this);
  }

  public Expr getConstructor(String name) {
    return new Expr(CVC4JNI.DatatypeType_getConstructor(swigCPtr, this, name), true);
  }

  public boolean isInstantiated() {
    return CVC4JNI.DatatypeType_isInstantiated(swigCPtr, this);
  }

  public boolean isParameterInstantiated(long n) {
    return CVC4JNI.DatatypeType_isParameterInstantiated(swigCPtr, this, n);
  }

  public vectorType getParamTypes() {
    return new vectorType(CVC4JNI.DatatypeType_getParamTypes(swigCPtr, this), true);
  }

  public long getArity() {
    return CVC4JNI.DatatypeType_getArity(swigCPtr, this);
  }

  public DatatypeType instantiate(vectorType params) {
    return new DatatypeType(CVC4JNI.DatatypeType_instantiate(swigCPtr, this, vectorType.getCPtr(params), params), true);
  }

  public long getTupleLength() {
    return CVC4JNI.DatatypeType_getTupleLength(swigCPtr, this);
  }

  public vectorType getTupleTypes() {
    return new vectorType(CVC4JNI.DatatypeType_getTupleTypes(swigCPtr, this), true);
  }

  public Record getRecord() {
    return new Record(CVC4JNI.DatatypeType_getRecord(swigCPtr, this), false);
  }

}
