/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class ExprManager implements ExprManagerI {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected ExprManager(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(ExprManager obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   public void finalize() {
    delete();
  } */

  @Override
  public synchronized void delete() {
    SmtEngine.dlRef(options);
    options = null;
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_ExprManager(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  // a ref is kept here to keep Java GC from collecting the Options
  // before the ExprManager
  private Object options;

  public ExprManager() {
    this(CVC4JNI.new_ExprManager__SWIG_0(), true);
    this.options = SmtEngine.mkRef(options); // keep ref to options in SWIG proxy class
  }

  public ExprManager(Options options) {
    this(CVC4JNI.new_ExprManager__SWIG_1(Options.getCPtr(options), options), true);
    this.options = SmtEngine.mkRef(options); // keep ref to options in SWIG proxy class
  }

  @Override
  public Options getOptions() {
    return new Options(CVC4JNI.ExprManager_getOptions(swigCPtr, this), false);
  }

  @Override
  public ResourceManager getResourceManager() {
    long cPtr = CVC4JNI.ExprManager_getResourceManager(swigCPtr, this);
    return (cPtr == 0) ? null : new ResourceManager(cPtr, false);
  }

  @Override
  public BooleanType booleanType() {
    return new BooleanType(CVC4JNI.ExprManager_booleanType(swigCPtr, this), true);
  }

  @Override
  public StringType stringType() {
    return new StringType(CVC4JNI.ExprManager_stringType(swigCPtr, this), true);
  }

  @Override
  public RegExpType regExpType() {
    return new RegExpType(CVC4JNI.ExprManager_regExpType(swigCPtr, this), true);
  }

  @Override
  public RealType realType() {
    return new RealType(CVC4JNI.ExprManager_realType(swigCPtr, this), true);
  }

  @Override
  public IntegerType integerType() {
    return new IntegerType(CVC4JNI.ExprManager_integerType(swigCPtr, this), true);
  }

  @Override
  public RoundingModeType roundingModeType() {
    return new RoundingModeType(CVC4JNI.ExprManager_roundingModeType(swigCPtr, this), true);
  }

  @Override
  public Expr mkExpr(Kind kind, Expr child1) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_0(swigCPtr, this, kind.swigValue(), Expr.getCPtr(child1), child1), true);
  }

  @Override
  public Expr mkExpr(Kind kind, Expr child1, Expr child2) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_1(swigCPtr, this, kind.swigValue(), Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2), true);
  }

  @Override
  public Expr mkExpr(Kind kind, Expr child1, Expr child2, Expr child3) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_2(swigCPtr, this, kind.swigValue(), Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2, Expr.getCPtr(child3), child3), true);
  }

  @Override
  public Expr mkExpr(Kind kind, Expr child1, Expr child2, Expr child3, Expr child4) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_3(swigCPtr, this, kind.swigValue(), Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2, Expr.getCPtr(child3), child3, Expr.getCPtr(child4), child4), true);
  }

  @Override
  public Expr mkExpr(Kind kind, Expr child1, Expr child2, Expr child3, Expr child4, Expr child5) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_4(swigCPtr, this, kind.swigValue(), Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2, Expr.getCPtr(child3), child3, Expr.getCPtr(child4), child4, Expr.getCPtr(child5), child5), true);
  }

  @Override
  public Expr mkExpr(Kind kind, vectorExpr children) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_5(swigCPtr, this, kind.swigValue(), vectorExpr.getCPtr(children), children), true);
  }

  @Override
  public Expr mkExpr(Kind kind, Expr child1, vectorExpr otherChildren) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_6(swigCPtr, this, kind.swigValue(), Expr.getCPtr(child1), child1, vectorExpr.getCPtr(otherChildren), otherChildren), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_7(swigCPtr, this, Expr.getCPtr(opExpr), opExpr), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr, Expr child1) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_8(swigCPtr, this, Expr.getCPtr(opExpr), opExpr, Expr.getCPtr(child1), child1), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr, Expr child1, Expr child2) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_9(swigCPtr, this, Expr.getCPtr(opExpr), opExpr, Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr, Expr child1, Expr child2, Expr child3) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_10(swigCPtr, this, Expr.getCPtr(opExpr), opExpr, Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2, Expr.getCPtr(child3), child3), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr, Expr child1, Expr child2, Expr child3, Expr child4) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_11(swigCPtr, this, Expr.getCPtr(opExpr), opExpr, Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2, Expr.getCPtr(child3), child3, Expr.getCPtr(child4), child4), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr, Expr child1, Expr child2, Expr child3, Expr child4, Expr child5) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_12(swigCPtr, this, Expr.getCPtr(opExpr), opExpr, Expr.getCPtr(child1), child1, Expr.getCPtr(child2), child2, Expr.getCPtr(child3), child3, Expr.getCPtr(child4), child4, Expr.getCPtr(child5), child5), true);
  }

  @Override
  public Expr mkExpr(Expr opExpr, vectorExpr children) {
    return new Expr(CVC4JNI.ExprManager_mkExpr__SWIG_13(swigCPtr, this, Expr.getCPtr(opExpr), opExpr, vectorExpr.getCPtr(children), children), true);
  }

  @Override
  public Expr mkAssociative(Kind kind, vectorExpr children) {
    return new Expr(CVC4JNI.ExprManager_mkAssociative(swigCPtr, this, kind.swigValue(), vectorExpr.getCPtr(children), children), true);
  }

  @Override
  public Expr operatorOf(Kind k) {
    return new Expr(CVC4JNI.ExprManager_operatorOf(swigCPtr, this, k.swigValue()), true);
  }

  @Override
  public Kind operatorToKind(Expr e) {
    return Kind.swigToEnum(CVC4JNI.ExprManager_operatorToKind(swigCPtr, this, Expr.getCPtr(e), e));
  }

  @Override
  public FunctionType mkFunctionType(Type domain, Type range) {
    return new FunctionType(CVC4JNI.ExprManager_mkFunctionType__SWIG_0(swigCPtr, this, Type.getCPtr(domain), domain, Type.getCPtr(range), range), true);
  }

  @Override
  public FunctionType mkFunctionType(vectorType argTypes, Type range) {
    return new FunctionType(CVC4JNI.ExprManager_mkFunctionType__SWIG_1(swigCPtr, this, vectorType.getCPtr(argTypes), argTypes, Type.getCPtr(range), range), true);
  }

  @Override
  public FunctionType mkFunctionType(vectorType sorts) {
    return new FunctionType(CVC4JNI.ExprManager_mkFunctionType__SWIG_2(swigCPtr, this, vectorType.getCPtr(sorts), sorts), true);
  }

  @Override
  public FunctionType mkPredicateType(vectorType sorts) {
    return new FunctionType(CVC4JNI.ExprManager_mkPredicateType(swigCPtr, this, vectorType.getCPtr(sorts), sorts), true);
  }

  @Override
  public DatatypeType mkTupleType(vectorType types) {
    return new DatatypeType(CVC4JNI.ExprManager_mkTupleType(swigCPtr, this, vectorType.getCPtr(types), types), true);
  }

  @Override
  public DatatypeType mkRecordType(Record rec) {
    return new DatatypeType(CVC4JNI.ExprManager_mkRecordType(swigCPtr, this, Record.getCPtr(rec), rec), true);
  }

  @Override
  public SExprType mkSExprType(vectorType types) {
    return new SExprType(CVC4JNI.ExprManager_mkSExprType(swigCPtr, this, vectorType.getCPtr(types), types), true);
  }

  @Override
  public FloatingPointType mkFloatingPointType(long exp, long sig) {
    return new FloatingPointType(CVC4JNI.ExprManager_mkFloatingPointType(swigCPtr, this, exp, sig), true);
  }

  @Override
  public BitVectorType mkBitVectorType(long size) {
    return new BitVectorType(CVC4JNI.ExprManager_mkBitVectorType(swigCPtr, this, size), true);
  }

  @Override
  public ArrayType mkArrayType(Type indexType, Type constituentType) {
    return new ArrayType(CVC4JNI.ExprManager_mkArrayType(swigCPtr, this, Type.getCPtr(indexType), indexType, Type.getCPtr(constituentType), constituentType), true);
  }

  @Override
  public SetType mkSetType(Type elementType) {
    return new SetType(CVC4JNI.ExprManager_mkSetType(swigCPtr, this, Type.getCPtr(elementType), elementType), true);
  }

  @Override
  public DatatypeType mkDatatypeType(Datatype datatype) {
    return new DatatypeType(CVC4JNI.ExprManager_mkDatatypeType(swigCPtr, this, Datatype.getCPtr(datatype), datatype), true);
  }

  @Override
  public vectorDatatypeType mkMutualDatatypeTypes(vectorDatatype datatypes) {
    return new vectorDatatypeType(CVC4JNI.ExprManager_mkMutualDatatypeTypes__SWIG_0(swigCPtr, this, vectorDatatype.getCPtr(datatypes), datatypes), true);
  }

  @Override
  public vectorDatatypeType mkMutualDatatypeTypes(vectorDatatype datatypes, setOfType unresolvedTypes) {
    return new vectorDatatypeType(CVC4JNI.ExprManager_mkMutualDatatypeTypes__SWIG_1(swigCPtr, this, vectorDatatype.getCPtr(datatypes), datatypes, setOfType.getCPtr(unresolvedTypes), unresolvedTypes), true);
  }

  @Override
  public ConstructorType mkConstructorType(DatatypeConstructor constructor, Type range) {
    return new ConstructorType(CVC4JNI.ExprManager_mkConstructorType(swigCPtr, this, DatatypeConstructor.getCPtr(constructor), constructor, Type.getCPtr(range), range), true);
  }

  @Override
  public SelectorType mkSelectorType(Type domain, Type range) {
    return new SelectorType(CVC4JNI.ExprManager_mkSelectorType(swigCPtr, this, Type.getCPtr(domain), domain, Type.getCPtr(range), range), true);
  }

  @Override
  public TesterType mkTesterType(Type domain) {
    return new TesterType(CVC4JNI.ExprManager_mkTesterType(swigCPtr, this, Type.getCPtr(domain), domain), true);
  }

  @Override
  public SortType mkSort(String name, long flags) {
    return new SortType(CVC4JNI.ExprManager_mkSort__SWIG_0(swigCPtr, this, name, flags), true);
  }

  @Override
  public SortType mkSort(String name) {
    return new SortType(CVC4JNI.ExprManager_mkSort__SWIG_1(swigCPtr, this, name), true);
  }

  @Override
  public SortConstructorType mkSortConstructor(String name, long arity) {
    return new SortConstructorType(CVC4JNI.ExprManager_mkSortConstructor(swigCPtr, this, name, arity), true);
  }

  @Override
  public Type getType(Expr e, boolean check) {
    return new Type(CVC4JNI.ExprManager_getType__SWIG_0(swigCPtr, this, Expr.getCPtr(e), e, check), true);
  }

  @Override
  public Type getType(Expr e) {
    return new Type(CVC4JNI.ExprManager_getType__SWIG_1(swigCPtr, this, Expr.getCPtr(e), e), true);
  }

  @Override
  public Expr mkVar(String name, Type type, long flags) {
    return new Expr(CVC4JNI.ExprManager_mkVar__SWIG_0(swigCPtr, this, name, Type.getCPtr(type), type, flags), true);
  }

  @Override
  public Expr mkVar(String name, Type type) {
    return new Expr(CVC4JNI.ExprManager_mkVar__SWIG_1(swigCPtr, this, name, Type.getCPtr(type), type), true);
  }

  @Override
  public Expr mkVar(Type type, long flags) {
    return new Expr(CVC4JNI.ExprManager_mkVar__SWIG_2(swigCPtr, this, Type.getCPtr(type), type, flags), true);
  }

  @Override
  public Expr mkVar(Type type) {
    return new Expr(CVC4JNI.ExprManager_mkVar__SWIG_3(swigCPtr, this, Type.getCPtr(type), type), true);
  }

  @Override
  public Expr mkBoundVar(String name, Type type) {
    return new Expr(CVC4JNI.ExprManager_mkBoundVar__SWIG_0(swigCPtr, this, name, Type.getCPtr(type), type), true);
  }

  @Override
  public Expr mkBoundVar(Type type) {
    return new Expr(CVC4JNI.ExprManager_mkBoundVar__SWIG_1(swigCPtr, this, Type.getCPtr(type), type), true);
  }

  @Override
  public Expr mkNullaryOperator(Type type, Kind k) {
    return new Expr(CVC4JNI.ExprManager_mkNullaryOperator(swigCPtr, this, Type.getCPtr(type), type, k.swigValue()), true);
  }

  @Override
  public Statistics getStatistics() {
    return new Statistics(CVC4JNI.ExprManager_getStatistics(swigCPtr, this), true);
  }

  @Override
  public SExpr getStatistic(String name) {
    return new SExpr(CVC4JNI.ExprManager_getStatistic(swigCPtr, this, name), true);
  }

  @Override
  public void safeFlushStatistics(int fd) {
    CVC4JNI.ExprManager_safeFlushStatistics(swigCPtr, this, fd);
  }

  @Override
  public Expr mkConst(ArrayStoreAll arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_1(swigCPtr, this, ArrayStoreAll.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorSize arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_2(swigCPtr, this, BitVectorSize.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(AscriptionType arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_3(swigCPtr, this, AscriptionType.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorBitOf arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_4(swigCPtr, this, BitVectorBitOf.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorRepeat arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_5(swigCPtr, this, BitVectorRepeat.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorExtract arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_6(swigCPtr, this, BitVectorExtract.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorRotateLeft arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_7(swigCPtr, this, BitVectorRotateLeft.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorSignExtend arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_8(swigCPtr, this, BitVectorSignExtend.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorZeroExtend arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_9(swigCPtr, this, BitVectorZeroExtend.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVectorRotateRight arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_10(swigCPtr, this, BitVectorRotateRight.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(IntToBitVector arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_11(swigCPtr, this, IntToBitVector.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(UninterpretedConstant arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_12(swigCPtr, this, UninterpretedConstant.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(Kind arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_13(swigCPtr, this, arg0.swigValue()), true);
  }

  @Override
  public Expr mkConst(DatatypeIndexConstant arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_14(swigCPtr, this, DatatypeIndexConstant.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(TupleUpdate arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_15(swigCPtr, this, TupleUpdate.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(RecordUpdate arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_16(swigCPtr, this, RecordUpdate.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(Rational arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_17(swigCPtr, this, Rational.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(BitVector arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_18(swigCPtr, this, BitVector.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(EmptySet arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_19(swigCPtr, this, EmptySet.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(CVC4String arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_20(swigCPtr, this, CVC4String.getCPtr(arg0), arg0), true);
  }

  @Override
  public Expr mkConst(TypeConstant arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_21(swigCPtr, this, arg0.swigValue()), true);
  }

  @Override
  public Expr mkConst(boolean arg0) {
    return new Expr(CVC4JNI.ExprManager_mkConst__SWIG_22(swigCPtr, this, arg0), true);
  }

  public final static int SORT_FLAG_NONE = CVC4JNI.ExprManager_SORT_FLAG_NONE_get();
  public final static int SORT_FLAG_PLACEHOLDER = CVC4JNI.ExprManager_SORT_FLAG_PLACEHOLDER_get();

  public final static int VAR_FLAG_NONE = CVC4JNI.ExprManager_VAR_FLAG_NONE_get();
  public final static int VAR_FLAG_GLOBAL = CVC4JNI.ExprManager_VAR_FLAG_GLOBAL_get();
  public final static int VAR_FLAG_DEFINED = CVC4JNI.ExprManager_VAR_FLAG_DEFINED_get();

}
