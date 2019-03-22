/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class Datatype implements Iterable<DatatypeConstructor> {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected Datatype(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(Datatype obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_Datatype(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public static Datatype datatypeOf(Expr item) {
    return new Datatype(CVC4JNI.Datatype_datatypeOf(Expr.getCPtr(item), item), false);
  }

  public static long indexOf(Expr item) {
    return CVC4JNI.Datatype_indexOf(Expr.getCPtr(item), item);
  }

  public static long cindexOf(Expr item) {
    return CVC4JNI.Datatype_cindexOf(Expr.getCPtr(item), item);
  }

  public static long indexOfInternal(Expr item) {
    return CVC4JNI.Datatype_indexOfInternal(Expr.getCPtr(item), item);
  }

  public static long cindexOfInternal(Expr item) {
    return CVC4JNI.Datatype_cindexOfInternal(Expr.getCPtr(item), item);
  }

  public Datatype(String name, boolean isCo) {
    this(CVC4JNI.new_Datatype__SWIG_0(name, isCo), true);
  }

  public Datatype(String name) {
    this(CVC4JNI.new_Datatype__SWIG_1(name), true);
  }

  public Datatype(String name, vectorType params, boolean isCo) {
    this(CVC4JNI.new_Datatype__SWIG_2(name, vectorType.getCPtr(params), params, isCo), true);
  }

  public Datatype(String name, vectorType params) {
    this(CVC4JNI.new_Datatype__SWIG_3(name, vectorType.getCPtr(params), params), true);
  }

  public void addConstructor(DatatypeConstructor c) {
    CVC4JNI.Datatype_addConstructor(swigCPtr, this, DatatypeConstructor.getCPtr(c), c);
  }

  public void setSygus(Type st, Expr bvl, boolean allow_const, boolean allow_all) {
    CVC4JNI.Datatype_setSygus(swigCPtr, this, Type.getCPtr(st), st, Expr.getCPtr(bvl), bvl, allow_const, allow_all);
  }

  public void addSygusConstructor(Expr op, SWIGTYPE_p_std__string cname, vectorType cargs, SWIGTYPE_p_std__shared_ptrT_CVC4__SygusPrintCallback_t spc, int weight) {
    CVC4JNI.Datatype_addSygusConstructor__SWIG_0(swigCPtr, this, Expr.getCPtr(op), op, SWIGTYPE_p_std__string.getCPtr(cname), vectorType.getCPtr(cargs), cargs, SWIGTYPE_p_std__shared_ptrT_CVC4__SygusPrintCallback_t.getCPtr(spc), weight);
  }

  public void addSygusConstructor(Expr op, SWIGTYPE_p_std__string cname, vectorType cargs, SWIGTYPE_p_std__shared_ptrT_CVC4__SygusPrintCallback_t spc) {
    CVC4JNI.Datatype_addSygusConstructor__SWIG_1(swigCPtr, this, Expr.getCPtr(op), op, SWIGTYPE_p_std__string.getCPtr(cname), vectorType.getCPtr(cargs), cargs, SWIGTYPE_p_std__shared_ptrT_CVC4__SygusPrintCallback_t.getCPtr(spc));
  }

  public void addSygusConstructor(Expr op, SWIGTYPE_p_std__string cname, vectorType cargs) {
    CVC4JNI.Datatype_addSygusConstructor__SWIG_2(swigCPtr, this, Expr.getCPtr(op), op, SWIGTYPE_p_std__string.getCPtr(cname), vectorType.getCPtr(cargs), cargs);
  }

  public void setTuple() {
    CVC4JNI.Datatype_setTuple(swigCPtr, this);
  }

  public void setRecord() {
    CVC4JNI.Datatype_setRecord(swigCPtr, this);
  }

  public String getName() {
    return CVC4JNI.Datatype_getName(swigCPtr, this);
  }

  public long getNumConstructors() {
    return CVC4JNI.Datatype_getNumConstructors(swigCPtr, this);
  }

  public boolean isParametric() {
    return CVC4JNI.Datatype_isParametric(swigCPtr, this);
  }

  public long getNumParameters() {
    return CVC4JNI.Datatype_getNumParameters(swigCPtr, this);
  }

  public Type getParameter(long i) {
    return new Type(CVC4JNI.Datatype_getParameter(swigCPtr, this, i), true);
  }

  public vectorType getParameters() {
    return new vectorType(CVC4JNI.Datatype_getParameters(swigCPtr, this), true);
  }

  public boolean isCodatatype() {
    return CVC4JNI.Datatype_isCodatatype(swigCPtr, this);
  }

  public boolean isSygus() {
    return CVC4JNI.Datatype_isSygus(swigCPtr, this);
  }

  public boolean isTuple() {
    return CVC4JNI.Datatype_isTuple(swigCPtr, this);
  }

  public boolean isRecord() {
    return CVC4JNI.Datatype_isRecord(swigCPtr, this);
  }

  public Record getRecord() {
    long cPtr = CVC4JNI.Datatype_getRecord(swigCPtr, this);
    return (cPtr == 0) ? null : new Record(cPtr, false);
  }

  public Cardinality getCardinality(Type t) {
    return new Cardinality(CVC4JNI.Datatype_getCardinality__SWIG_0(swigCPtr, this, Type.getCPtr(t), t), true);
  }

  public Cardinality getCardinality() {
    return new Cardinality(CVC4JNI.Datatype_getCardinality__SWIG_1(swigCPtr, this), true);
  }

  public boolean isFinite(Type t) {
    return CVC4JNI.Datatype_isFinite__SWIG_0(swigCPtr, this, Type.getCPtr(t), t);
  }

  public boolean isFinite() {
    return CVC4JNI.Datatype_isFinite__SWIG_1(swigCPtr, this);
  }

  public boolean isInterpretedFinite(Type t) {
    return CVC4JNI.Datatype_isInterpretedFinite__SWIG_0(swigCPtr, this, Type.getCPtr(t), t);
  }

  public boolean isInterpretedFinite() {
    return CVC4JNI.Datatype_isInterpretedFinite__SWIG_1(swigCPtr, this);
  }

  public boolean isWellFounded() {
    return CVC4JNI.Datatype_isWellFounded(swigCPtr, this);
  }

  public boolean isRecursiveSingleton(Type t) {
    return CVC4JNI.Datatype_isRecursiveSingleton__SWIG_0(swigCPtr, this, Type.getCPtr(t), t);
  }

  public boolean isRecursiveSingleton() {
    return CVC4JNI.Datatype_isRecursiveSingleton__SWIG_1(swigCPtr, this);
  }

  public long getNumRecursiveSingletonArgTypes(Type t) {
    return CVC4JNI.Datatype_getNumRecursiveSingletonArgTypes__SWIG_0(swigCPtr, this, Type.getCPtr(t), t);
  }

  public Type getRecursiveSingletonArgType(Type t, long i) {
    return new Type(CVC4JNI.Datatype_getRecursiveSingletonArgType__SWIG_0(swigCPtr, this, Type.getCPtr(t), t, i), true);
  }

  public long getNumRecursiveSingletonArgTypes() {
    return CVC4JNI.Datatype_getNumRecursiveSingletonArgTypes__SWIG_1(swigCPtr, this);
  }

  public Type getRecursiveSingletonArgType(long i) {
    return new Type(CVC4JNI.Datatype_getRecursiveSingletonArgType__SWIG_1(swigCPtr, this, i), true);
  }

  public Expr mkGroundTerm(Type t) {
    return new Expr(CVC4JNI.Datatype_mkGroundTerm(swigCPtr, this, Type.getCPtr(t), t), true);
  }

  public DatatypeType getDatatypeType() {
    return new DatatypeType(CVC4JNI.Datatype_getDatatypeType__SWIG_0(swigCPtr, this), true);
  }

  public DatatypeType getDatatypeType(vectorType params) {
    return new DatatypeType(CVC4JNI.Datatype_getDatatypeType__SWIG_1(swigCPtr, this, vectorType.getCPtr(params), params), true);
  }

  public boolean equals(Datatype other) {
    return CVC4JNI.Datatype_equals(swigCPtr, this, Datatype.getCPtr(other), other);
  }

  public boolean isResolved() {
    return CVC4JNI.Datatype_isResolved(swigCPtr, this);
  }

  public DatatypeConstructor get(long index) {
    return new DatatypeConstructor(CVC4JNI.Datatype_get__SWIG_0(swigCPtr, this, index), false);
  }

  public DatatypeConstructor get(String name) {
    return new DatatypeConstructor(CVC4JNI.Datatype_get__SWIG_1(swigCPtr, this, name), false);
  }

  public Expr getConstructor(String name) {
    return new Expr(CVC4JNI.Datatype_getConstructor(swigCPtr, this, name), true);
  }

  public Type getSygusType() {
    return new Type(CVC4JNI.Datatype_getSygusType(swigCPtr, this), true);
  }

  public Expr getSygusVarList() {
    return new Expr(CVC4JNI.Datatype_getSygusVarList(swigCPtr, this), true);
  }

  public boolean getSygusAllowConst() {
    return CVC4JNI.Datatype_getSygusAllowConst(swigCPtr, this);
  }

  public boolean getSygusAllowAll() {
    return CVC4JNI.Datatype_getSygusAllowAll(swigCPtr, this);
  }

  public Expr getSygusEvaluationFunc() {
    return new Expr(CVC4JNI.Datatype_getSygusEvaluationFunc(swigCPtr, this), true);
  }

  public boolean involvesExternalType() {
    return CVC4JNI.Datatype_involvesExternalType(swigCPtr, this);
  }

  public boolean involvesUninterpretedType() {
    return CVC4JNI.Datatype_involvesUninterpretedType(swigCPtr, this);
  }

  public JavaIteratorAdapter_Datatype iterator() {
    return new JavaIteratorAdapter_Datatype(CVC4JNI.Datatype_iterator(swigCPtr, this), true);
  }

  public String toString() {
    return CVC4JNI.Datatype_toString(swigCPtr, this);
  }

}
